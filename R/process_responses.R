#' @title Run the Full SADCAT Pipeline
#'
#' @description Run the full SADCAT LLM response processing pipeline.
#' Chains all pipeline stages in sequence: text preprocessing, valence scoring,
#' dictionary matching, embedding computation, seed similarity computation,
#' and aggregation. Each stage can be skipped via the \code{stages} parameter.
#' @param data A data.frame with raw LLM responses
#' @param text_col Column with raw text responses (default "responsex")
#' @param response_col Column with original response for negation/NA checks (default "response")
#' @param group_cols Grouping columns for aggregation (default c("Synonym.GroupX", "Group", "Level"))
#' @param stages Character vector of stages to run. Default: all stages.
#'   Options: "preprocess", "valence", "dictionaries", "embeddings", "seeds", "aggregate"
#' @param spellcheck Run spell-checking in preprocessing? (default TRUE)
#' @param singularize_text Run singularization in preprocessing? (default TRUE)
#' @param java_home Path to Java JRE for spell-checking
#' @param wordnet_dict Path to WordNet dictionary files
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
#' @export process_responses

process_responses <- function(data,
                              text_col = "responsex",
                              response_col = "response",
                              group_cols = c("Synonym.GroupX", "Group", "Level"),
                              stages = c("preprocess", "valence", "dictionaries",
                                         "embeddings", "seeds", "aggregate"),
                              # Preprocessing params
                              spellcheck = TRUE,
                              singularize_text = TRUE,
                              java_home = "C:\\Program Files\\Java\\jre-1.8",
                              wordnet_dict = "C:\\dict",
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
                            singularize = singularize_text,
                            java_home = java_home,
                            wordnet_dict = wordnet_dict,
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
  if ("dictionaries" %in% stages) {
    sadcat_dict <- prepare_sadcat_dictionaries()
    data <- match_dictionaries(data,
                               text_col = "tv3",
                               response_col = "tv",
                               valence_col = "ValyNA",
                               sadcat_dict = sadcat_dict)
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
                                        verbose = verbose)
    }
    if (save_intermediates) {
      utils::write.csv(data, paste0(save_prefix, "_5_seeds.csv"), row.names = FALSE)
    }
  }

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
