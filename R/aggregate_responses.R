#' @title Aggregate Responses
#'
#' @description Aggregate response-level data to group-level.
#' Performs three types of aggregation (sum, mean, distinct) and joins them.
#' Auto-detects standard pipeline columns by pattern; user can add extra
#' columns via parameters. Optionally creates noNA versions of direction
#' and valence columns (replacing NA with 0).
#' @param data The full long-format data.frame from the pipeline
#' @param group_cols Character vector of grouping columns (default c("Synonym.GroupX", "Group", "Level"))
#' @param sum_cols Columns to sum-aggregate. If NULL, auto-detects _binary2, None2y, traditional
#' @param mean_cols Columns to mean-aggregate. If NULL, auto-detects standard patterns
#' @param distinct_cols Columns to preserve by distinct. If NULL, auto-detects Warning columns
#' @param extra_sum_cols Additional columns to include in sum aggregation
#' @param extra_mean_cols Additional columns to include in mean aggregation
#' @param extra_distinct_cols Additional columns to include in distinct aggregation
#' @param create_noNA Logical. Create *noNA versions of direction/valence? (default TRUE)
#' @param verbose Print progress? (default TRUE)
#' @return A data.frame with one row per unique combination of group_cols
#' @export aggregate_responses

aggregate_responses <- function(data,
                                group_cols = c("Synonym.GroupX", "Group", "Level"),
                                sum_cols = NULL,
                                mean_cols = NULL,
                                distinct_cols = NULL,
                                extra_sum_cols = NULL,
                                extra_mean_cols = NULL,
                                extra_distinct_cols = NULL,
                                create_noNA = TRUE,
                                verbose = TRUE) {
  message("--- Stage 7: Aggregating responses ---")

  all_names <- names(data)

  # ---- Auto-detect columns ----
  if (is.null(sum_cols)) {
    sum_cols <- grep("_dic_binary2$|^None2y$|^traditional$", all_names, value = TRUE)
  }
  if (!is.null(extra_sum_cols)) {
    extra_sum_cols <- extra_sum_cols[extra_sum_cols %in% all_names]
    sum_cols <- unique(c(sum_cols, extra_sum_cols))
  }

  if (is.null(mean_cols)) {
    mean_cols <- grep(
      paste0("^Valy$|^ValyNA$|",
             "_dic_binary2$|^None2y$|",
             "_ValyNA$|^NONE_ValyNA$|",
             "_valy3$|_dirx3$|",
             "^SBERT_|^Gemini_|",
             "\\.seed$|",
             "^traditional$"),
      all_names, value = TRUE
    )
  }
  if (!is.null(extra_mean_cols)) {
    extra_mean_cols <- extra_mean_cols[extra_mean_cols %in% all_names]
    mean_cols <- unique(c(mean_cols, extra_mean_cols))
  }

  if (is.null(distinct_cols)) {
    distinct_cols <- grep("Warning$|^Level$", all_names, value = TRUE)
    # Remove Level if it's already in group_cols
    distinct_cols <- setdiff(distinct_cols, group_cols)
  }
  if (!is.null(extra_distinct_cols)) {
    extra_distinct_cols <- extra_distinct_cols[extra_distinct_cols %in% all_names]
    distinct_cols <- unique(c(distinct_cols, extra_distinct_cols))
  }

  # Ensure group_cols exist
  missing_groups <- setdiff(group_cols, all_names)
  if (length(missing_groups) > 0) {
    stop("Group columns not found in data: ", paste(missing_groups, collapse = ", "))
  }

  if (verbose) {
    message("  Sum columns: ", length(sum_cols))
    message("  Mean columns: ", length(mean_cols))
    message("  Distinct columns: ", length(distinct_cols))
  }

  # ---- Sum aggregation ----
  if (length(sum_cols) > 0) {
    sum_data <- data %>%
      dplyr::select(dplyr::all_of(c(group_cols, sum_cols))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(., na.rm = TRUE)),
                       .groups = "drop")
    # Rename sum columns with _Sum suffix
    sum_rename <- setdiff(names(sum_data), group_cols)
    names(sum_data)[names(sum_data) %in% sum_rename] <- paste0(sum_rename, "_Sum")
  } else {
    sum_data <- NULL
  }

  # ---- Mean aggregation ----
  if (length(mean_cols) > 0) {
    mean_data <- data %>%
      dplyr::select(dplyr::all_of(c(group_cols, mean_cols))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~mean(., na.rm = TRUE)),
                       .groups = "drop")
  } else {
    mean_data <- NULL
  }

  # ---- Distinct aggregation ----
  if (length(distinct_cols) > 0) {
    distinct_data <- data %>%
      dplyr::select(dplyr::all_of(c(group_cols, distinct_cols))) %>%
      dplyr::distinct()
  } else {
    distinct_data <- NULL
  }

  # ---- Join all ----
  parts <- Filter(Negate(is.null), list(distinct_data, mean_data, sum_data))
  if (length(parts) == 0) {
    stop("No columns selected for aggregation.")
  }
  result <- plyr::join_all(parts, by = group_cols, type = "left")

  # Replace NaN with NA
  result <- replace_nan_with_na(result)

  # ---- Create noNA versions ----
  if (create_noNA) {
    # Direction noNA: _dirx3 columns
    dirx3_cols <- grep("_dirx3$", names(result), value = TRUE)
    for (col in dirx3_cols) {
      result[[paste0(col, "noNA")]] <- ifelse(is.na(result[[col]]), 0, result[[col]])
    }

    # Valence noNA: _valy3 columns
    valy3_cols <- grep("_valy3$", names(result), value = TRUE)
    for (col in valy3_cols) {
      result[[paste0(col, "noNA")]] <- ifelse(is.na(result[[col]]), 0, result[[col]])
    }

    if (verbose) {
      message("  Created ", length(dirx3_cols) + length(valy3_cols), " noNA columns.")
    }
  }

  if (verbose) message("  Aggregation complete. Result: ",
                       nrow(result), " rows x ", ncol(result), " columns.")
  return(result)
}
