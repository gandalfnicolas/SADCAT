#' @title Compute Sentence Embeddings
#'
#' @description Compute sentence embeddings using SBERT and/or Gemini via reticulate.
#' Generates embeddings for unique text values and joins them back to the data.
#' SBERT uses the sentence-transformers Python library; Gemini uses the Google
#' genai Python library with batched API calls and rate limiting.
#' @param data A data.frame with a text column
#' @param text_col Column containing text to encode (default "tv")
#' @param methods Character vector: one or both of "sbert" and "gemini" (default "sbert")
#' @param sbert_model SBERT model name (default "paraphrase-mpnet-base-v2")
#' @param sbert_dims Number of dimensions in SBERT output (default 768)
#' @param gemini_api_key Gemini API key. If NULL, reads from Sys.getenv("GEMINI_API_KEY")
#' @param gemini_model Gemini model name (default "gemini-embedding-exp-03-07")
#' @param gemini_dims Embedding dimensionality for Gemini (default 2000)
#' @param gemini_batch_size Batch size for Gemini API calls (default 10)
#' @param gemini_sleep Seconds to sleep between Gemini batches (default 63)
#' @param gemini_task_type Gemini task type (default "SEMANTIC_SIMILARITY")
#' @param response_col Column used to set embeddings to NA where response is missing (default "response")
#' @param verbose Print progress? (default TRUE)
#' @return The input data with embedding columns appended (SBERT_1:SBERT_N and/or Gemini_1:Gemini_N)
#' @export compute_embeddings

compute_embeddings <- function(data,
                               text_col = "tv",
                               methods = c("sbert"),
                               sbert_model = "paraphrase-mpnet-base-v2",
                               sbert_dims = 768L,
                               gemini_api_key = NULL,
                               gemini_model = "gemini-embedding-exp-03-07",
                               gemini_dims = 2000L,
                               gemini_batch_size = 10L,
                               gemini_sleep = 63,
                               gemini_task_type = "SEMANTIC_SIMILARITY",
                               response_col = "response",
                               verbose = TRUE) {
  .require_data_columns(data, text_col, "compute_embeddings()")
  if (!is.null(response_col)) {
    .require_data_columns(data, response_col, "compute_embeddings()")
  }

  message("--- Stage 5: Computing embeddings ---")

  # Get unique non-NA text values
  unique_text <- unique(data[[text_col]])
  unique_text <- unique_text[!is.na(unique_text) & unique_text != ""]
  if (verbose) message("  Unique text values to encode: ", length(unique_text))
  if (length(unique_text) == 0) {
    message("  No non-missing text values to encode. Skipping embedding computation.")
    return(data)
  }

  if (!.has_namespace("reticulate")) {
    stop("Package 'reticulate' is required for compute_embeddings(). Install with: install.packages('reticulate')")
  }

  # ---- SBERT ----
  if ("sbert" %in% tolower(methods)) {
    if (verbose) message("  Computing SBERT embeddings (model: ", sbert_model, ")...")

    st <- .reticulate_import("sentence_transformers")
    model_sbert <- st$SentenceTransformer(sbert_model)

    encoding_matrix <- model_sbert$encode(unique_text)
    encoding_df <- as.data.frame(.reticulate_py_to_r(encoding_matrix))
    colnames(encoding_df) <- paste0("SBERT_", seq_len(ncol(encoding_df)))

    sbert_vecs <- data.frame(word = unique_text, encoding_df, stringsAsFactors = FALSE)

    # Merge with data
    join_by <- stats::setNames("word", text_col)
    data <- dplyr::left_join(data, sbert_vecs, by = join_by)

    # Set embeddings to NA where response is NA
    sbert_cols <- paste0("SBERT_", seq_len(ncol(encoding_df)))
    data <- .mask_missing_response_cols(
      data,
      response_col,
      cols_or_patterns = c("^SBERT_\\d+$")
    )

    if (verbose) message("  SBERT complete. Added ", length(sbert_cols), " columns.")
  }

  # ---- Gemini ----
  if ("gemini" %in% tolower(methods)) {
    # Resolve API key
    api_key <- gemini_api_key
    if (is.null(api_key) || api_key == "") {
      api_key <- Sys.getenv("GEMINI_API_KEY")
    }
    if (is.null(api_key) || api_key == "") {
      stop("Gemini API key not found. Provide via gemini_api_key parameter or set GEMINI_API_KEY environment variable.")
    }

    if (verbose) message("  Computing Gemini embeddings (model: ", gemini_model,
                         ", dims: ", gemini_dims, ")...")

    genai <- .reticulate_import("google.genai")
    types <- .reticulate_import("google.genai.types")

    client <- genai$Client(api_key = api_key)

    config <- types$EmbedContentConfig(
      output_dimensionality = as.integer(gemini_dims),
      task_type = gemini_task_type
    )

    text_list <- as.list(unique_text)
    all_embeddings <- list()
    embedded_text <- character(0)
    n_batches <- ceiling(length(text_list) / gemini_batch_size)

    for (i in seq(1, length(text_list), by = gemini_batch_size)) {
      batch_num <- ceiling(i / gemini_batch_size)
      batch_end <- min(i + gemini_batch_size - 1, length(text_list))
      batch <- text_list[i:batch_end]

      if (verbose) message("    Batch ", batch_num, " / ", n_batches,
                           " (items ", i, "-", batch_end, ")")

      result <- tryCatch({
        client$models$embed_content(
          model = gemini_model,
          contents = batch,
          config = config
        )
      }, error = function(e) {
        warning("  Batch ", batch_num, " failed: ", conditionMessage(e))
        NULL
      })

      if (!is.null(result)) {
        all_embeddings <- c(all_embeddings, list(result))
        embedded_text <- c(embedded_text, unlist(batch, use.names = FALSE))
      }

      # Sleep to avoid rate limit (skip after last batch)
      if (batch_end < length(text_list)) {
        if (verbose) message("    Sleeping ", gemini_sleep, " seconds (rate limit)...")
        .sleep_seconds(gemini_sleep)
      }
    }

    # Flatten embeddings
    flat_embeddings <- list()
    for (batch_result in all_embeddings) {
      for (emb in batch_result$embeddings) {
        flat_embeddings <- c(flat_embeddings, list(emb$values))
      }
    }
    if (!length(flat_embeddings)) {
      warning("  Gemini embedding failed for all batches; no Gemini columns were added.")
      message("  Embedding computation complete.")
      return(data)
    }

    # Build data frame
    gemini_matrix <- do.call(rbind, flat_embeddings)
    gemini_df <- as.data.frame(gemini_matrix)
    colnames(gemini_df) <- paste0("Gemini_", seq_len(ncol(gemini_df)))

    gemini_vecs <- data.frame(word = embedded_text[seq_len(nrow(gemini_df))],
                              gemini_df, stringsAsFactors = FALSE)

    # Merge with data
    join_by <- stats::setNames("word", text_col)
    data <- dplyr::left_join(data, gemini_vecs, by = join_by)

    # Set embeddings to NA where response is NA
    gemini_cols <- paste0("Gemini_", seq_len(ncol(gemini_df)))
    data <- .mask_missing_response_cols(
      data,
      response_col,
      cols_or_patterns = c("^Gemini_\\d+$")
    )

    if (verbose) message("  Gemini complete. Added ", length(gemini_cols), " columns.")
  }

  message("  Embedding computation complete.")
  return(data)
}

