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

  valence_scores <- .score_valence_from_scoped_tokens(data[[text_col]])
  data <- cbind(data, valence_scores)

  # Combined valence
  val_cols <- c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")
  valna_cols <- c("Val_lexicoderNA", "Val_NRCNA", "Val_bingNA", "Val_affinNA", "Val_loughranNA")

  data$Valy <- rowMeans(data[, val_cols, drop = FALSE], na.rm = TRUE)
  data$ValyNA <- rowMeans(data[, valna_cols, drop = FALSE], na.rm = TRUE)

  # Ensure valence family columns are NA when the original response is missing
  data <- .mask_missing_response_cols(
    data,
    response_col,
    cols_or_patterns = c("^Val_", "^Valy$", "^ValyNA$")
  )

  # Replace NaN with NA
  data <- replace_nan_with_na(data)

  message("  Valence scoring complete. Columns added: ",
          paste(c(val_cols, valna_cols, "Valy", "ValyNA"), collapse = ", "))
  return(data)
}
