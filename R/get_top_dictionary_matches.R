#' @title Get Top SADCAT Dictionary Matches
#'
#' @description Inspect the most frequent words in \code{text_col} that match
#' each SADCAT dictionary dimension. Useful for quick sanity checks of what the
#' dictionary coding is capturing in your dataset.
#'
#' @param data A data.frame containing text responses.
#' @param text_col Column containing text to inspect (default \code{"tv3"}).
#' @param top_n Number of top words to return per dimension (default \code{20}).
#' @param sadcat_dict Optional SADCAT dictionary object from
#'   \code{\link{prepare_sadcat_dictionaries}}. If \code{NULL}, it is prepared
#'   automatically.
#' @param dictionary_names Optional vector of dictionary names to include (e.g.,
#'   \code{c("Warmth_dic", "Competence_dic")}). If \code{NULL}, uses base
#'   SADCAT dimensions (\code{*_dic}) only.
#' @param min_count Minimum token frequency to keep (default \code{1}).
#' @param verbose Print progress? (default \code{TRUE}).
#'
#' @return A data.frame with columns:
#' \itemize{
#'   \item \code{Dictionary}: SADCAT dimension label (without \code{_dic})
#'   \item \code{word}: Matched token
#'   \item \code{n}: Token frequency in the dataset
#'   \item \code{prop}: Proportion within that dictionary's matched tokens
#' }
#' Returns an empty data frame with these columns if no matches are found.
#' @export
get_top_dictionary_matches <- function(data,
                                       text_col = "tv3",
                                       top_n = 20L,
                                       sadcat_dict = NULL,
                                       dictionary_names = NULL,
                                       min_count = 1L,
                                       verbose = TRUE) {
  .require_data_columns(data, text_col, "get_top_dictionary_matches()")

  top_n <- as.integer(top_n)
  min_count <- as.integer(min_count)
  if (is.na(top_n) || top_n <= 0L) stop("`top_n` must be a positive integer.")
  if (is.na(min_count) || min_count <= 0L) stop("`min_count` must be a positive integer.")

  if (is.null(sadcat_dict)) {
    sadcat_dict <- prepare_sadcat_dictionaries()
  }

  dict_list <- as.list(sadcat_dict)
  all_dict_names <- names(dict_list)

  if (is.null(dictionary_names)) {
    base_dict_names <- paste0(.SADCAT_ALL_DIMS, "_dic")
    sel <- all_dict_names[tolower(all_dict_names) %in% tolower(base_dict_names)]
    # Fallback if dictionary names are non-standard.
    if (length(sel) == 0L) {
      sel <- all_dict_names[grepl("_dic$", all_dict_names, ignore.case = TRUE) &
                              !grepl("(_hi|_lo|_pos|_neg)_dic$|_dic_(hi|lo|pos|neg)$",
                                     all_dict_names, ignore.case = TRUE)]
    }
    dictionary_names <- sel
  } else {
    want <- unique(as.character(dictionary_names))
    idx <- match(tolower(want), tolower(all_dict_names))
    missing <- want[is.na(idx)]
    if (length(missing) > 0L) {
      warning("Ignoring unknown dictionary name(s): ", paste(missing, collapse = ", "))
    }
    dictionary_names <- all_dict_names[na.omit(idx)]
  }

  if (length(dictionary_names) == 0L) {
    warning("No dictionary names selected; returning empty result.")
    return(data.frame(
      Dictionary = character(0),
      word = character(0),
      n = integer(0),
      prop = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  if (verbose) {
    message("Computing top dictionary matches for ", length(dictionary_names), " dimensions...")
  }

  # Match preprocessing used by dictionary pipeline: lowercase, split hyphens,
  # remove trailing plural "s", then tokenize.
  txt <- enc2utf8(as.character(data[[text_col]]))
  txt <- tolower(txt)
  txt <- gsub("-", " ", txt)
  txt <- vapply(txt, function(x) {
    if (is.na(x)) return(NA_character_)
    paste(sapply(strsplit(x, " ")[[1]], delete_ending_Ss), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
  txt[is.na(txt)] <- ""

  toks <- .tokenize_quanteda_text(
    txt,
    prefix = "response",
    remove_numbers = FALSE,
    remove_punct = TRUE,
    remove_symbols = TRUE
  )

  out_parts <- vector("list", length(dictionary_names))
  names(out_parts) <- dictionary_names

  for (i in seq_along(dictionary_names)) {
    dict_name <- dictionary_names[[i]]
    patterns <- dict_list[[dict_name]]
    if (length(patterns) == 0L) next

    toks_match <- quanteda::tokens_select(
      toks,
      pattern = patterns,
      valuetype = "glob",
      case_insensitive = TRUE,
      selection = "keep",
      padding = FALSE
    )

    dfm_match <- quanteda::dfm(toks_match)
    if (quanteda::nfeat(dfm_match) == 0L) next

    freq_vec <- quanteda::topfeatures(dfm_match, n = quanteda::nfeat(dfm_match))
    if (length(freq_vec) == 0L) next
    freq <- data.frame(
      feature = names(freq_vec),
      frequency = as.numeric(freq_vec),
      stringsAsFactors = FALSE
    )
    freq <- freq[freq$frequency >= min_count, , drop = FALSE]
    if (nrow(freq) == 0L) next

    freq <- freq[order(-freq$frequency, freq$feature), , drop = FALSE]
    freq <- utils::head(freq, top_n)
    total_n <- sum(freq$frequency)

    out_parts[[i]] <- data.frame(
      Dictionary = sub("_dic$", "", dict_name, ignore.case = TRUE),
      word = freq$feature,
      n = as.integer(freq$frequency),
      prop = if (total_n > 0) freq$frequency / total_n else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  out_parts <- Filter(Negate(is.null), out_parts)
  if (length(out_parts) == 0L) {
    if (verbose) message("No dictionary word matches found.")
    return(data.frame(
      Dictionary = character(0),
      word = character(0),
      n = integer(0),
      prop = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, out_parts)
  rownames(out) <- NULL

  if (verbose) {
    message("Top matches complete. Rows returned: ", nrow(out))
  }
  out
}
