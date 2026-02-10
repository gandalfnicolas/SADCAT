#' NaN-aware replacement for data frames
#' @param df A data frame
#' @return Data frame with NaN values replaced by NA
#' @keywords internal
replace_nan_with_na <- function(df) {
  is_nan_df <- do.call(cbind, lapply(df, is.nan))
  df[is_nan_df] <- NA
  df
}

#' Build a quanteda sentiment dictionary from tidytext
#'
#' @param name One of "nrc", "bing", "afinn", "loughran"
#' @return A quanteda dictionary with positive and negative entries
#' @keywords internal
build_sentiment_dictionary <- function(name) {
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
  quanteda::dictionary(sent_list)
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

  # Binarize
  neg_col <- if (is_lexicoder) "negative" else paste0("negative_", name)
  pos_col <- if (is_lexicoder) "positive" else paste0("positive_", name)

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
