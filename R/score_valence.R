#' @title Score Text Valence
#'
#' @description Score text responses using 5 sentiment dictionaries.
#' Applies Lexicoder (LSD2015), NRC, Bing, AFINN, and Loughran sentiment
#' dictionaries. Produces per-dictionary valence scores and a combined score.
#' @param data A data.frame with text to score
#' @param text_col Column containing cleaned text for valence scoring (default "tv").
#'   Uses tv (not tv2/tv3) to preserve original response for LLMs with low spelling errors.
#'   Change to "tv2" for human participants.
#' @param response_col Column used for NA-gating (default: same as text_col).
#'   Only needed if your NA-indicator column differs from text_col.
#' @return The input data with 12 new columns: Val_lexicoder, Val_NRC, Val_bing,
#'   Val_affin, Val_loughran, their NA variants, Valence (NA when no dictionary
#'   matched, negation-aware), and ValenceNoNA (0 instead of NA)
#' @export score_valence

score_valence <- function(data,
                          text_col = "tv",
                          response_col = NULL) {
  if (is.null(response_col)) response_col <- text_col
  message("--- Stage 2: Scoring valence (5 dictionaries) ---")
  .require_data_columns(data, text_col, "score_valence()")
  .require_data_columns(data, response_col, "score_valence()")

  valence_scores <- .score_valence_from_scoped_tokens(data[[text_col]])

  # Separate internal negation-aware columns from output columns
  neg_valna_cols <- grep("^\\.neg_valna_", names(valence_scores), value = TRUE)
  neg_valna_avg <- rowMeans(valence_scores[, neg_valna_cols, drop = FALSE], na.rm = TRUE)

  # Add only the raw (non-negation) individual dictionary columns to output
  data <- cbind(data, valence_scores[, !names(valence_scores) %in% neg_valna_cols, drop = FALSE])

  # Combined valence (negation-aware, applied once to the average)
  # Valence: NA when no dictionary matched any sentiment words
  # ValenceNoNA: 0 instead of NA
  data$Valence <- neg_valna_avg
  data$ValenceNoNA <- ifelse(is.nan(neg_valna_avg), 0, neg_valna_avg)

  val_cols <- c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")
  valna_cols <- c("Val_lexicoderNA", "Val_NRCNA", "Val_bingNA", "Val_affinNA", "Val_loughranNA")

  # Ensure valence family columns are NA when the original response is missing
  data <- .mask_missing_response_cols(
    data,
    response_col,
    cols_or_patterns = c("^Val_", "^Valence$", "^ValenceNoNA$")
  )

  # Replace NaN with NA
  data <- replace_nan_with_na(data)

  message("  Valence scoring complete. Columns added: ",
          paste(c(val_cols, valna_cols, "Valence", "ValenceNoNA"), collapse = ", "))
  return(data)
}
