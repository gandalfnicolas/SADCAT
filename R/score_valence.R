#' @title Score Text Valence
#'
#' @description Score text responses using 5 sentiment dictionaries.
#' Applies Lexicoder (LSD2015), NRC, Bing, AFINN, and Loughran sentiment
#' dictionaries. Produces per-dictionary valence scores and a combined score.
#' @param data A data.frame with text to score
#' @param text_col Column containing cleaned text for valence scoring (default "tv").
#'   Uses tv (not tv2/tv3) to preserve original response for LLMs with low spelling errors.
#'   Change to "tv2" for human participants.
#' @param response_col Column with original response for negation detection (default "response")
#' @return The input data with 12 new columns: Val_lexicoder, Val_NRC, Val_bing,
#'   Val_affin, Val_loughran, their NA variants, Valy, and ValyNA
#' @export score_valence

score_valence <- function(data,
                          text_col = "tv",
                          response_col = "response") {
  message("--- Stage 2: Scoring valence (5 dictionaries) ---")

  # Clean and tokenize
  valuex <- clean_large_text(data[[text_col]])
  toksval <- quanteda::tokens(valuex)

  # 1. Lexicoder (LSD2015) -- built-in quanteda dictionary
  lex_result <- apply_single_valence_dict(toksval, quanteda::data_dictionary_LSD2015,
                                          "lexicoder", is_lexicoder = TRUE)

  # 2-5. NRC, Bing, AFINN, Loughran
  nrc_dict <- build_sentiment_dictionary("nrc")
  nrc_result <- apply_single_valence_dict(toksval, nrc_dict, "NRC")

  bing_dict <- build_sentiment_dictionary("bing")
  bing_result <- apply_single_valence_dict(toksval, bing_dict, "bing")

  afinn_dict <- build_sentiment_dictionary("afinn")
  afinn_result <- apply_single_valence_dict(toksval, afinn_dict, "affin")  # "affin" not "afinn" for backward compat

  loughran_dict <- build_sentiment_dictionary("loughran")
  loughran_result <- apply_single_valence_dict(toksval, loughran_dict, "loughran")

  # Bind all valence columns
  data <- cbind(data,
                lex_result,
                nrc_result,
                bing_result,
                afinn_result,
                loughran_result)

  # Apply negation reversal for lexicoder (anti-, non- patterns)
  data <- dplyr::mutate(data,
    Val_lexicoder = ifelse(grepl(.NEGATION_PATTERN_VAL, .data[[response_col]]),
                           Val_lexicoder * -1, Val_lexicoder))

  # Combined valence
  val_cols <- c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")
  valna_cols <- c("Val_lexicoderNA", "Val_NRCNA", "Val_bingNA", "Val_affinNA", "Val_loughranNA")

  data$Valy <- rowMeans(data[, val_cols, drop = FALSE], na.rm = TRUE)
  data$ValyNA <- rowMeans(data[, valna_cols, drop = FALSE], na.rm = TRUE)

  # Replace NaN with NA
  data <- replace_nan_with_na(data)

  message("  Valence scoring complete. Columns added: ",
          paste(c(val_cols, valna_cols, "Valy", "ValyNA"), collapse = ", "))
  return(data)
}
