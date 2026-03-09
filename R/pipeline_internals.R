#' NaN-aware replacement for data frames
#' @param df A data frame
#' @return Data frame with NaN values replaced by NA
#' @keywords internal
replace_nan_with_na <- function(df) {
  is_nan_df <- do.call(cbind, lapply(df, is.nan))
  df[is_nan_df] <- NA
  df
}

#' Prepare character input for quanteda tokenization with stable docnames
#' @param x A character vector
#' @param prefix Prefix for generated docnames
#' @return Character vector with explicit unique names for quanteda
#' @keywords internal
.prepare_quanteda_text <- function(x, prefix = "doc") {
  x <- enc2utf8(as.character(x))
  x <- unname(x)
  names(x) <- paste0(prefix, "_", seq_along(x))
  x
}

#' Tokenize character input with stable row-based docnames
#' @param x A character vector
#' @param prefix Prefix for generated docnames
#' @param ... Passed to \code{quanteda::tokens()}
#' @return A quanteda tokens object
#' @keywords internal
.tokenize_quanteda_text <- function(x, prefix = "doc", ...) {
  quanteda::tokens(.prepare_quanteda_text(x, prefix = prefix), ...)
}

#' Build a quanteda sentiment dictionary from tidytext
#'
#' @param name One of "nrc", "bing", "afinn", "loughran"
#' @return A quanteda dictionary with positive and negative entries
#' @keywords internal
build_sentiment_dictionary <- function(name) {
  cache_key <- paste0("sentiment_dictionary_", name)
  if (exists(cache_key, envir = .negation_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .negation_cache, inherits = FALSE))
  }

  if (name == "nrc") {
    sent <- tidytext::get_sentiments("nrc") %>%
      dplyr::filter(sentiment == "positive" | sentiment == "negative")
    pos_col <- paste0("positive_", name)
    neg_col <- paste0("negative_", name)
    sent[[pos_col]] <- ifelse(sent$sentiment == "positive", tolower(sent$word), NA)
    sent[[neg_col]] <- ifelse(sent$sentiment == "negative", tolower(sent$word), NA)
    sent <- dplyr::select(sent, dplyr::all_of(c(pos_col, neg_col)))
  } else if (name == "bing") {
    sent <- tidytext::get_sentiments("bing")
    pos_col <- paste0("positive_", name)
    neg_col <- paste0("negative_", name)
    sent[[pos_col]] <- ifelse(sent$sentiment == "positive", tolower(sent$word), NA)
    sent[[neg_col]] <- ifelse(sent$sentiment == "negative", tolower(sent$word), NA)
    sent <- dplyr::select(sent, dplyr::all_of(c(pos_col, neg_col)))
  } else if (name == "afinn") {
    sent <- tidytext::get_sentiments("afinn")
    pos_col <- paste0("positive_", name)
    neg_col <- paste0("negative_", name)
    sent[[pos_col]] <- ifelse(sent$value > 0, tolower(sent$word), NA)
    sent[[neg_col]] <- ifelse(sent$value < 0, tolower(sent$word), NA)
    sent <- dplyr::select(sent, dplyr::all_of(c(pos_col, neg_col)))
  } else if (name == "loughran") {
    sent <- tidytext::get_sentiments("loughran") %>%
      dplyr::filter(sentiment == "positive" | sentiment == "negative")
    pos_col <- paste0("positive_", name)
    neg_col <- paste0("negative_", name)
    sent[[pos_col]] <- ifelse(sent$sentiment == "positive", tolower(sent$word), NA)
    sent[[neg_col]] <- ifelse(sent$sentiment == "negative", tolower(sent$word), NA)
    sent <- dplyr::select(sent, dplyr::all_of(c(pos_col, neg_col)))
  }
  sent_list <- lapply(sent, function(x) x[!is.na(x)])
  out <- quanteda::dictionary(sent_list)
  assign(cache_key, out, envir = .negation_cache)
  out
}

#' Apply a single sentiment dictionary and compute valence scores
#'
#' @param toksval quanteda tokens object
#' @param dict_obj quanteda dictionary object
#' @param name Dictionary name (for column naming)
#' @param is_lexicoder If TRUE, handle neg_positive and neg_negative sub-categories
#' @return Data frame with Val and ValNA columns for this dictionary
#' @keywords internal
apply_single_valence_dict <- function(toksval, dict_obj, name, is_lexicoder = FALSE) {
  toks_lookup <- quanteda::tokens_lookup(toksval, dict_obj,
                                         nested_scope = "dictionary", exclusive = TRUE)
  result <- quanteda::convert(quanteda::dfm(toks_lookup), to = "data.frame")

  if (is_lexicoder) {
    result$negative <- result$negative + result$neg_positive
    result$positive <- result$positive + result$neg_negative
  }

  # Binarize — find pos/neg columns from actual dfm result (case-insensitive)
  result_cols <- setdiff(colnames(result), "doc_id")
  if (is_lexicoder) {
    neg_col <- "negative"
    pos_col <- "positive"
  } else {
    neg_col <- grep("^negative", result_cols, value = TRUE, ignore.case = TRUE)[1]
    pos_col <- grep("^positive", result_cols, value = TRUE, ignore.case = TRUE)[1]
  }

  neg_binary <- ifelse(result[[neg_col]] > 0, 1, 0)
  pos_binary <- ifelse(result[[pos_col]] > 0, 1, 0)

  val_name <- paste0("Val_", name)
  valna_name <- paste0("Val_", name, "NA")

  out <- data.frame(
    val = pos_binary - neg_binary,
    valna = ifelse(pos_binary + neg_binary == 0, NA, pos_binary - neg_binary)
  )
  colnames(out) <- c(val_name, valna_name)
  out
}
#' Mask selected columns to NA where response text is missing
#' @param data A data frame
#' @param response_col Column indicating whether the response is missing
#' @param cols_or_patterns Character vector of exact column names and/or regex patterns
#' @return Data frame with selected columns masked to NA for missing responses
#' @keywords internal
.mask_missing_response_cols <- function(data, response_col, cols_or_patterns) {
  if (is.null(response_col) || !(response_col %in% names(data))) {
    return(data)
  }
  if (is.null(cols_or_patterns) || length(cols_or_patterns) == 0) {
    return(data)
  }

  missing_idx <- is.na(data[[response_col]])
  if (!any(missing_idx)) {
    return(data)
  }

  target_cols <- character(0)
  for (item in cols_or_patterns) {
    if (is.na(item) || !nzchar(item)) {
      next
    }
    if (item %in% names(data)) {
      target_cols <- c(target_cols, item)
      next
    }
    matches <- grep(item, names(data), value = TRUE)
    if (length(matches) > 0) {
      target_cols <- c(target_cols, matches)
    }
  }

  target_cols <- unique(target_cols)
  if (length(target_cols) == 0) {
    return(data)
  }

  data[missing_idx, target_cols] <- NA
  data
}

