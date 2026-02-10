#' @title Match Text Against SADCAT Dictionaries
#'
#' @description Match text responses against SADCAT dictionaries.
#' Tokenizes response text, matches against SADCAT stereotype dictionaries,
#' and computes binary indicators, percentages, direction scores, and
#' per-dimension valence/direction columns.
#' @param data A data.frame with preprocessed text and valence scores
#' @param text_col Column with singularized text to match (default "tv3")
#' @param response_col Column with original lowercased text for NA checks (default "tv")
#' @param valence_col Name of combined valence-NA column (default "ValyNA")
#' @param sadcat_dict Pre-computed quanteda dictionary. If NULL, calls prepare_sadcat_dictionaries()
#' @return The input data with many new columns: dictionary counts, percentages,
#'   binary indicators, direction scores, per-dimension valence, etc.
#' @export match_dictionaries

match_dictionaries <- function(data,
                               text_col = "tv3",
                               response_col = "tv",
                               valence_col = "ValyNA",
                               sadcat_dict = NULL) {
  message("--- Stage 4: Matching dictionaries ---")

  # Get or prepare dictionaries
  if (is.null(sadcat_dict)) {
    sadcat_dict <- prepare_sadcat_dictionaries()
  }

  # ---- Prepare text for matching (tv3 -> tv4) ----
  data$tv4 <- enc2utf8(as.character(data[[text_col]]))
  data$tv4 <- tolower(data$tv4)
  data$tv4 <- gsub("-", " ", data$tv4)

  # Remove ending Ss
  delete_ending_Ss2_internal <- function(x) {
    if (is.na(x)) return(x)
    unlist(lapply(x, function(y) {
      paste(sapply(strsplit(y, ' '), delete_ending_Ss), collapse = ' ')
    }))
  }
  data$tv4 <- vapply(data$tv4, delete_ending_Ss2_internal, character(1))

  # ---- Tokenize and match ----
  toks <- quanteda::tokens(data$tv4, remove_numbers = FALSE,
                           remove_punct = TRUE, remove_symbols = TRUE)

  toks_dict_pre <- quanteda::tokens_lookup(toks, dictionary = sadcat_dict,
                                           nested_scope = "dictionary",
                                           exclusive = TRUE, levels = 1)
  toks_dict_df <- quanteda::convert(quanteda::dfm(toks_dict_pre), to = "data.frame")

  # Combine with token count and original data
  toks_dict_df$ntoken <- quanteda::ntoken(toks)
  toks_dict <- cbind(toks_dict_df, data)

  # ---- Identify dictionary column names (lowercase from quanteda output) ----
  # quanteda lowercases all dictionary names
  dict_cols_all <- tolower(names(sadcat_dict))

  # First and last dictionary columns for range-based operations
  first_dic <- dict_cols_all[1]
  last_dic <- dict_cols_all[length(dict_cols_all)]

  # ---- Percentages ----
  for (col in dict_cols_all) {
    toks_dict[[paste0(col, "_percent")]] <- toks_dict[[col]] / toks_dict$ntoken
  }

  # ---- Binary indicators ----
  for (col in dict_cols_all) {
    toks_dict[[paste0(col, "_binary")]] <- ifelse(toks_dict[[col]] > 0, 1, 0)
  }

  # ---- Direction scores (hi - lo for directional dimensions) ----
  for (dim in .SADCAT_DIR_DIMS) {
    hi_col <- paste0(tolower(dim), "_dic_hi_binary")
    lo_col <- paste0(tolower(dim), "_dic_lo_binary")
    dirx_col <- paste0(dim, "_dirx")
    if (hi_col %in% names(toks_dict) && lo_col %in% names(toks_dict)) {
      toks_dict[[dirx_col]] <- toks_dict[[hi_col]] - toks_dict[[lo_col]]
    }
  }

  # ---- Binary2: NA if response is NA ----
  # For all base and non-directional dimensions
  all_base_dims <- c(.SADCAT_DIR_DIMS, .SADCAT_NDIR_DIMS)
  for (dim in all_base_dims) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    binary2_col <- paste0(tolower(dim), "_dic_binary2")
    if (binary_col %in% names(toks_dict)) {
      toks_dict[[binary2_col]] <- ifelse(is.na(toks_dict[[response_col]]),
                                         NA, toks_dict[[binary_col]])
    }
  }

  # ---- None2y: no dictionary match ----
  binary2_cols <- grep("_dic_binary2$", names(toks_dict), value = TRUE)
  if (length(binary2_cols) > 0) {
    row_sums <- rowSums(toks_dict[, binary2_cols, drop = FALSE], na.rm = FALSE)
    toks_dict$None2y <- ifelse(is.na(row_sums), NA, ifelse(row_sums > 0, 0, 1))
  }

  # ---- Per-dimension ValyNA (NA if not in that dimension) ----
  for (dim in all_base_dims) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    valyna_dim <- paste0(dim, "_ValyNA")
    if (binary_col %in% names(toks_dict) && valence_col %in% names(toks_dict)) {
      toks_dict[[valyna_dim]] <- ifelse(toks_dict[[binary_col]] == 0,
                                        NA, toks_dict[[valence_col]])
    }
  }
  # NONE ValyNA
  if ("None2y" %in% names(toks_dict) && valence_col %in% names(toks_dict)) {
    toks_dict$NONE_ValyNA <- ifelse(toks_dict$None2y == 0, NA, toks_dict[[valence_col]])
  }

  # ---- Negation reversal for direction scores ----
  for (dim in .SADCAT_DIR_DIMS) {
    dirx_col <- paste0(dim, "_dirx")
    dirx2_col <- paste0(dim, "_dirx2")
    if (dirx_col %in% names(toks_dict)) {
      toks_dict[[dirx2_col]] <- ifelse(
        grepl(.NEGATION_PATTERN, toks_dict[[response_col]]),
        toks_dict[[dirx_col]] * -1,
        toks_dict[[dirx_col]]
      )
    }
  }

  # ---- Fix valy3 and dirx3: NA if binary==0 OR binary2 is NA ----
  # Directional dimensions: valy3 and dirx3
  for (dim in .SADCAT_DIR_DIMS) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    binary2_col <- paste0(tolower(dim), "_dic_binary2")
    valyna_dim <- paste0(dim, "_ValyNA")
    valy3_col <- paste0(dim, "_valy3")
    dirx_col <- paste0(dim, "_dirx")
    dirx3_col <- paste0(dim, "_dirx3")

    if (all(c(binary_col, binary2_col, valyna_dim) %in% names(toks_dict))) {
      toks_dict[[valy3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valyna_dim]])
    }
    if (all(c(binary_col, binary2_col, dirx_col) %in% names(toks_dict))) {
      toks_dict[[dirx3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[dirx_col]])
    }
  }

  # Non-directional dimensions: valy3 only (no dirx)
  ndir_for_valy3 <- c("Occupation", "Emotion", "Deviance", "Socialgroups",
                       "Geography", "Appearance", "Other", "OtherwFam")
  for (dim in ndir_for_valy3) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    binary2_col <- paste0(tolower(dim), "_dic_binary2")
    valyna_dim <- paste0(dim, "_ValyNA")
    valy3_col <- paste0(dim, "_valy3")

    if (all(c(binary_col, binary2_col, valyna_dim) %in% names(toks_dict))) {
      toks_dict[[valy3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valyna_dim]])
    }
  }

  # Replace NaN with NA
  toks_dict <- replace_nan_with_na(toks_dict)

  message("  Dictionary matching complete.")
  return(toks_dict)
}
