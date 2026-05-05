#' @title Score Text Valence
#'
#' @description Score text responses using up to 8 sentiment dictionaries.
#' Always applies Lexicoder (LSD2015), NRC, Bing, AFINN, and Loughran. By
#' default also applies SentiWordNet, Warriner et al. (2013) affective norms,
#' and the Jockers/Rinker lexicon. The Bing/NRC/AFINN/Loughran/Lexicoder/
#' SentiWordNet dictionaries score each text as -1, 0, or +1 (presence-based).
#' Warriner and Jockers are continuous-valued in [-1, 1] (mean of matched-word
#' valences; Warriner's 1-9 ratings are linearly rescaled, Jockers values
#' clipped to [-1, 1]). All per-dictionary scores are then averaged into the
#' combined negation-aware \code{ValenceYesNA} / \code{ValenceNoNA}.
#' @param data A data.frame with text to score
#' @param text_col Column containing cleaned text for valence scoring (default "tv").
#'   Uses tv (not tv2/tv3) to preserve original response for LLMs with low spelling errors.
#'   Change to "tv2" or "tv3" for human participants (a singularized text column
#'   matches more dictionary entries when responses contain plural/inflected forms).
#' @param response_col Column used for NA-gating (default: same as text_col).
#'   Only needed if your NA-indicator column differs from text_col.
#' @param include_sentiwn Logical (default \code{TRUE}). If \code{TRUE}, also
#'   apply SentiWordNet (presence-based, thresholded at \code{sentiwn_threshold})
#'   and add \code{Val_sentiwn} / \code{Val_sentiwnNA}.
#' @param sentiwn_threshold Numeric in [0, 1] (default 0.10). Minimum
#'   |Val_sentiwn| magnitude for a word to count as positive or negative.
#' @param include_warriner Logical (default \code{TRUE}). If \code{TRUE}, also
#'   apply Warriner et al. (2013) affective norms (continuous valence in
#'   [-1, 1] from 13,915 lemmas, rescaled from the original 1-9 Likert) and
#'   add \code{Val_warriner} / \code{Val_warrinerNA}.
#' @param include_jockers Logical (default \code{TRUE}). If \code{TRUE}, also
#'   apply the Jockers/Rinker lexicon (continuous valence clipped to [-1, 1]
#'   from 11,710 entries) and add \code{Val_jockers} / \code{Val_jockersNA}.
#' @return The input data with new columns. Always: Val_lexicoder, Val_NRC,
#'   Val_bing, Val_affin, Val_loughran, their NA variants, ValenceYesNA, and
#'   ValenceNoNA. Optionally also Val_sentiwn, Val_warriner, Val_jockers
#'   (and their NA variants) per the \code{include_*} flags.
#' @export score_valence

score_valence <- function(data,
                          text_col = "tv",
                          response_col = NULL,
                          include_sentiwn = TRUE,
                          sentiwn_threshold = 0.10,
                          include_warriner = TRUE,
                          include_jockers = TRUE) {
  if (is.null(response_col)) response_col <- text_col
  n_dicts <- 5L + sum(c(isTRUE(include_sentiwn),
                         isTRUE(include_warriner),
                         isTRUE(include_jockers)))
  message("--- Stage 2: Scoring valence (", n_dicts, " dictionaries) ---")
  .require_data_columns(data, text_col, "score_valence()")
  .require_data_columns(data, response_col, "score_valence()")

  # Deduplicate: score unique texts only, then map back
  texts <- data[[text_col]]
  uniq_mask <- !duplicated(texts)
  uniq_texts <- texts[uniq_mask]
  n_uniq <- length(uniq_texts)
  n_total <- length(texts)

  if (n_uniq < n_total) {
    message("  Deduplicating: ", n_total, " rows -> ", n_uniq, " unique texts")
    uniq_scores <- .score_valence_from_scoped_tokens(uniq_texts,
                                                      include_sentiwn = include_sentiwn,
                                                      sentiwn_threshold = sentiwn_threshold,
                                                      include_warriner = include_warriner,
                                                      include_jockers = include_jockers)
    map_idx <- match(texts, uniq_texts)
    valence_scores <- uniq_scores[map_idx, , drop = FALSE]
    rownames(valence_scores) <- NULL
  } else {
    valence_scores <- .score_valence_from_scoped_tokens(texts,
                                                         include_sentiwn = include_sentiwn,
                                                         sentiwn_threshold = sentiwn_threshold,
                                                         include_warriner = include_warriner,
                                                         include_jockers = include_jockers)
  }

  # Separate internal negation-aware columns from output columns
  neg_valna_cols <- grep("^\\.neg_valna_", names(valence_scores), value = TRUE)
  neg_valna_avg <- rowMeans(valence_scores[, neg_valna_cols, drop = FALSE], na.rm = TRUE)

  # Add only the raw (non-negation) individual dictionary columns to output
  data <- cbind(data, valence_scores[, !names(valence_scores) %in% neg_valna_cols, drop = FALSE])

  # Combined valence (negation-aware, applied once to the average)
  # ValenceYesNA: NA when no dictionary matched any sentiment words
  # ValenceNoNA: 0 instead of NA
  data$ValenceYesNA <- neg_valna_avg
  data$ValenceNoNA <- ifelse(is.nan(neg_valna_avg), 0, neg_valna_avg)

  val_cols <- c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")
  valna_cols <- c("Val_lexicoderNA", "Val_NRCNA", "Val_bingNA", "Val_affinNA", "Val_loughranNA")
  if (isTRUE(include_sentiwn)) {
    val_cols <- c(val_cols, "Val_sentiwn")
    valna_cols <- c(valna_cols, "Val_sentiwnNA")
  }
  if (isTRUE(include_warriner)) {
    val_cols <- c(val_cols, "Val_warriner")
    valna_cols <- c(valna_cols, "Val_warrinerNA")
  }
  if (isTRUE(include_jockers)) {
    val_cols <- c(val_cols, "Val_jockers")
    valna_cols <- c(valna_cols, "Val_jockersNA")
  }

  # Ensure valence family columns are NA when the original response is missing
  data <- .mask_missing_response_cols(
    data,
    response_col,
    cols_or_patterns = c("^Val_", "^ValenceYesNA$", "^ValenceNoNA$")
  )

  # Replace NaN with NA
  data <- replace_nan_with_na(data)

  message("  Valence scoring complete. Columns added: ",
          paste(c(val_cols, valna_cols, "ValenceYesNA", "ValenceNoNA"), collapse = ", "))
  return(data)
}
