#' @title Run the Full SADCAT Pipeline
#'
#' @description Run the full SADCAT LLM response processing pipeline.
#' Chains all pipeline stages in sequence: text preprocessing, valence scoring,
#' dictionary matching, embedding computation, seed similarity computation,
#' and aggregation. Each stage can be skipped via the \code{stages} parameter.
#' @param data A data.frame with raw LLM responses
#' @param text_col Column with raw text responses (default "responsex")
#' @param response_col Column with original response for NA-gating (default: same as text_col).
#'   Only needed if your NA-indicator column differs from text_col.
#' @param group_cols Grouping columns for aggregation (default c("Synonym.GroupX", "Group", "Level"))
#' @param stages Character vector of stages to run. Default: all stages.
#'   Options: "preprocess", "valence", "dictionaries", "socats", "embeddings", "seeds", "aggregate"
#' @param spellcheck Run spell-checking in preprocessing? (default TRUE)
#' @param spellcheck_method Method for spell-checking: "hunspell" (default) or
#'   "gemini" (LLM-based, requires API key). See \code{\link{preprocess_text}}.
#' @param singularize_text Run singularization in preprocessing? (default TRUE)
#' @param java_home Optional Java home path for spell-checking. If NULL or not found,
#' auto-detects from environment, PATH, and common install locations.
#' @param wordnet_dict Optional WordNet dictionary path. If NULL or not found,
#' auto-detects from environment and common install locations.
#' @param gemini_spellcheck_key Gemini API key for LLM spell-checking (default NULL).
#' @param gemini_spellcheck_model Gemini model for spell-checking (default "gemini-2.0-flash").
#' @param gemini_spellcheck_context Custom system prompt for Gemini spell-checking (default NULL).
#' @param valence_text_col Column to use for valence scoring (default "tv").
#'   Use "tv" for LLMs (preserve original), "tv2" for human participants.
#' @param embedding_methods Embedding methods: "sbert", "gemini", or both (default "sbert")
#' @param sbert_model SBERT model name (default "paraphrase-mpnet-base-v2")
#' @param gemini_api_key Gemini API key. If NULL, reads from environment variable
#' @param gemini_dims Gemini embedding dimensionality (default 2000)
#' @param gemini_batch_size Gemini batch size (default 10)
#' @param gemini_sleep Seconds between Gemini batches (default 63)
#' @param seed_method Seed similarity method: "correlation" or "cosine" (default "correlation")
#' @param extra_sum_cols Extra columns for sum aggregation
#' @param extra_mean_cols Extra columns for mean aggregation
#' @param extra_distinct_cols Extra columns for distinct aggregation
#' @param save_intermediates Save CSVs after each stage? (default FALSE)
#' @param save_prefix Prefix for intermediate CSV filenames (default "pipeline")
#' @param verbose Print progress? (default TRUE)
#' @return A named list with \code{$long} (full long-format data) and
#'   \code{$agg} (aggregated data, if "aggregate" stage was run)
#'
#' @details
#' \strong{Stage-by-stage usage.} Each stage can be called as a standalone
#' exported function. This is useful for inspecting intermediate results,
#' customizing parameters per stage, or integrating with external processing.
#' The functions must be called in order because each stage depends on columns
#' produced by earlier stages:
#'
#' \preformatted{
#' # Stage 1: Preprocess -> produces tv, tv2, tv3
#' dat <- preprocess_text(dat, text_col = "responsex", spellcheck = FALSE)
#'
#' # Stage 2: Valence -> produces Val_*, Valy, ValyNoNA
#' dat <- score_valence(dat, text_col = "tv")
#'
#' # Stage 3: SADCAT dictionaries -> produces *_dic_binary, *_dirx, *_Valy, etc.
#' dat <- match_dictionaries(dat, text_col = "tv3",
#'                           valence_col = "Valy", valence_nona_col = "ValyNoNA")
#'
#' # Stage 3b (optional): SOCATS social category dictionaries
#' dat <- match_dictionaries(dat, text_col = "tv3",
#'                           valence_col = "Valy", valence_nona_col = "ValyNoNA",
#'                           sadcat_dict = FALSE, socats = TRUE)
#'
#' # Stage 4 (optional): Sentence embeddings (requires Python via reticulate)
#' dat <- compute_embeddings(dat, text_col = "tv", methods = "sbert")
#'
#' # Stage 5 (optional): Seed similarities (requires Stage 4)
#' dat <- compute_seed_similarities(dat, embedding_prefix = "SBERT")
#'
#' # Stage 6: Aggregate to group level
#' agg <- aggregate_responses(dat,
#'           group_cols = c("Synonym.GroupX", "Group", "Level"))
#' }
#'
#' \strong{Valence columns.}
#' \itemize{
#'   \item \code{Valy}: Mean of negation-aware scores across 5 sentiment
#'     dictionaries. NA when no dictionary matched. Negation is applied once
#'     to this combined score, not to individual dictionary scores.
#'   \item \code{ValyNoNA}: Same as \code{Valy}, but 0 instead of NA.
#'   \item \code{Val_lexicoder}, \code{Val_NRC}, etc.: Per-dictionary raw
#'     presence scores (-1, 0, or 1) without negation flipping.
#'   \item \code{{Dim}_Valy}: Global \code{Valy} gated by dimension prevalence
#'     (NA if dimension not present in the response).
#'   \item \code{{Dim}_ValyNoNA}: Global \code{ValyNoNA} gated by dimension
#'     prevalence (0 if dimension not present).
#' }
#'
#' @seealso \code{\link{preprocess_text}}, \code{\link{score_valence}},
#'   \code{\link{match_dictionaries}}, \code{\link{compute_embeddings}},
#'   \code{\link{compute_seed_similarities}}, \code{\link{aggregate_responses}}
#' @export process_responses

process_responses <- function(data,
                              text_col = "responsex",
                              response_col = NULL,
                              group_cols = c("Synonym.GroupX", "Group", "Level"),
                              stages = c("preprocess", "valence", "dictionaries",
                                         "socats", "embeddings", "seeds", "aggregate"),
                              # Preprocessing params
                              spellcheck = TRUE,
                              spellcheck_method = "hunspell",
                              singularize_text = TRUE,
                              java_home = NULL,
                              wordnet_dict = NULL,
                              gemini_spellcheck_key = NULL,
                              gemini_spellcheck_model = "gemini-2.0-flash",
                              gemini_spellcheck_context = NULL,
                              # Valence params
                              valence_text_col = "tv",
                              # Embedding params
                              embedding_methods = c("sbert"),
                              sbert_model = "paraphrase-mpnet-base-v2",
                              gemini_api_key = NULL,
                              gemini_dims = 2000L,
                              gemini_batch_size = 10L,
                              gemini_sleep = 63,
                              # Seed similarity params
                              seed_method = "correlation",
                              # Aggregation params
                              extra_sum_cols = NULL,
                              extra_mean_cols = NULL,
                              extra_distinct_cols = NULL,
                              # Output params
                              save_intermediates = FALSE,
                              save_prefix = "pipeline",
                              verbose = TRUE) {

  if (is.null(response_col)) response_col <- text_col

  message("========================================")
  message("SADCAT Pipeline: process_responses()")
  message("Stages: ", paste(stages, collapse = " -> "))
  message("========================================")

  result <- list(long = NULL, agg = NULL)

  # ---- Stage 1: Preprocess ----
  if ("preprocess" %in% stages) {
    data <- preprocess_text(data,
                            text_col = text_col,
                            spellcheck = spellcheck,
                            spellcheck_method = spellcheck_method,
                            singularize = singularize_text,
                            java_home = java_home,
                            wordnet_dict = wordnet_dict,
                            gemini_spellcheck_key = gemini_spellcheck_key,
                            gemini_spellcheck_model = gemini_spellcheck_model,
                            gemini_spellcheck_context = gemini_spellcheck_context,
                            verbose = verbose)
    if (save_intermediates) {
      utils::write.csv(data, paste0(save_prefix, "_1_preprocessed.csv"), row.names = FALSE)
    }
  }

  # ---- Stage 2: Valence ----
  if ("valence" %in% stages) {
    data <- score_valence(data,
                          text_col = valence_text_col,
                          response_col = response_col)
    if (save_intermediates) {
      utils::write.csv(data, paste0(save_prefix, "_2_valence.csv"), row.names = FALSE)
    }
  }

  # ---- Stage 3+4: Dictionaries ----
  if ("dictionaries" %in% stages || "socats" %in% stages) {
    do_sadcat <- "dictionaries" %in% stages
    do_socats <- "socats" %in% stages

    sadcat_dict_obj <- if (do_sadcat) prepare_sadcat_dictionaries() else FALSE
    socats_dict_obj <- if (do_socats) prepare_socats_dictionaries() else NULL

    data <- match_dictionaries(data,
                               text_col = "tv3",
                               response_col = response_col,
                               valence_col = "Valy",
                               valence_nona_col = "ValyNoNA",
                               sadcat_dict = sadcat_dict_obj,
                               socats_dict = socats_dict_obj,
                               socats = do_socats)
    if (save_intermediates) {
      utils::write.csv(data, paste0(save_prefix, "_3_dictionaries.csv"), row.names = FALSE)
    }
  }

  # ---- Stage 5: Embeddings ----
  if ("embeddings" %in% stages) {
    data <- compute_embeddings(data,
                               text_col = "tv",
                               methods = embedding_methods,
                               sbert_model = sbert_model,
                               gemini_api_key = gemini_api_key,
                               gemini_dims = gemini_dims,
                               gemini_batch_size = gemini_batch_size,
                               gemini_sleep = gemini_sleep,
                               response_col = response_col,
                               verbose = verbose)
    if (save_intermediates) {
      utils::write.csv(data, paste0(save_prefix, "_4_embeddings.csv"), row.names = FALSE)
    }
  }

  # ---- Stage 6: Seed similarities ----
  if ("seeds" %in% stages) {
    for (prefix in embedding_methods) {
      prefix_upper <- toupper(substr(prefix, 1, 1))
      prefix_name <- paste0(prefix_upper, substr(prefix, 2, nchar(prefix)))
      # SBERT -> "SBERT", gemini -> "Gemini"
      if (tolower(prefix) == "sbert") prefix_name <- "SBERT"
      if (tolower(prefix) == "gemini") prefix_name <- "Gemini"

      data <- compute_seed_similarities(data,
                                        embedding_prefix = prefix_name,
                                        method = seed_method,
                                        response_col = response_col,
                                        verbose = verbose)
    }
    if (save_intermediates) {
      utils::write.csv(data, paste0(save_prefix, "_5_seeds.csv"), row.names = FALSE)
    }
  }

  # Final targeted masking pass for missing responses
  data <- .mask_missing_response_cols(
    data,
    response_col,
    cols_or_patterns = c(
      "^Val_", "^Valy$", "^ValyNoNA$",
      "_Valy$", "_ValyNoNA$", "_valy3$", "_valyNoNA3$",
      "_dirx$", "_dirx2$", "_dirx3$",
      "^SBERT_\\d+$", "^Gemini_\\d+$", "\\.seed$"
    )
  )

  result$long <- data

  # ---- Stage 7: Aggregate ----
  if ("aggregate" %in% stages) {
    result$agg <- aggregate_responses(data,
                                      group_cols = group_cols,
                                      extra_sum_cols = extra_sum_cols,
                                      extra_mean_cols = extra_mean_cols,
                                      extra_distinct_cols = extra_distinct_cols,
                                      verbose = verbose)
    if (save_intermediates) {
      utils::write.csv(result$agg, paste0(save_prefix, "_6_aggregated.csv"), row.names = FALSE)
    }
  }

  message("========================================")
  message("Pipeline complete!")
  if (!is.null(result$long)) message("  Long data: ", nrow(result$long), " rows x ", ncol(result$long), " cols")
  if (!is.null(result$agg)) message("  Aggregated: ", nrow(result$agg), " rows x ", ncol(result$agg), " cols")
  message("========================================")

  return(result)
}


