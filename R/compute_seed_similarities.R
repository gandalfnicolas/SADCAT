#' @title Compute Seed Similarities
#'
#' @description Compute similarity between response embeddings and SADCAT seed vectors.
#' For each response, computes the correlation (or cosine similarity) between
#' its embedding vector and each SADCAT seed vector. Works for any embedding
#' prefix (SBERT, Gemini, etc.) as long as matching columns exist in both the
#' data and seed vectors.
#' @param data A data.frame with embedding columns (e.g., SBERT_1:SBERT_768)
#' @param embedding_prefix Prefix (or vector of prefixes) identifying embedding
#'   columns (default "SBERT"), e.g., \code{c("SBERT", "Gemini")}.
#' @param seed_vectors Seed vector data. Default: Seed_Vectors_Avg
#' @param method "correlation" (default, uses cor()) or "cosine" (uses lsa::cosine())
#' @param response_col Column used for NA-gating (default: same as text_col, where
#'   text_col is inferred as the embedding source column). Only needed if your
#'   NA-indicator column differs from the embedding source.
#' @param verbose Print progress? (default TRUE)
#' @return The input data with new columns: \code{prefix_SeedName.seed}
#' @export compute_seed_similarities

compute_seed_similarities <- function(data,
                                      embedding_prefix = "SBERT",
                                      seed_vectors = Seed_Vectors_Avg,
                                      method = "correlation",
                                      response_col = NULL,
                                      verbose = TRUE) {
  prefixes <- unique(as.character(embedding_prefix))
  prefixes <- prefixes[!is.na(prefixes) & nzchar(prefixes)]
  if (length(prefixes) == 0) {
    stop("embedding_prefix must contain at least one non-empty prefix")
  }

  if (!method %in% c("correlation", "cosine")) {
    stop("method must be 'correlation' or 'cosine'")
  }

  message("--- Stage 6: Computing seed similarities (", paste(prefixes, collapse = ", "), ") ---")

  total_added <- 0L

  for (prefix in prefixes) {
    # Check if seed vectors have columns matching the prefix
    seed_emb_cols <- grep(paste0("^", prefix, "_"), names(seed_vectors), value = TRUE)
    if (length(seed_emb_cols) == 0) {
      message("  No columns matching prefix '", prefix,
              "' found in seed_vectors. Skipping this prefix.")
      next
    }

    # Check if data has columns matching the prefix
    data_emb_cols <- grep(paste0("^", prefix, "_\\d+$"), names(data), value = TRUE)
    if (length(data_emb_cols) == 0) {
      message("  No embedding columns matching prefix '", prefix,
              "' found in data. Skipping this prefix.")
      next
    }

    # Extract seed info
    Seed_Vectors_names <- paste0(as.character(seed_vectors$Dictionary), ".seed")

    # Extract seed embedding matrix (dims x seeds)
    Seed_Matrix <- t(dplyr::select(seed_vectors, dplyr::all_of(seed_emb_cols)))
    colnames(Seed_Matrix) <- Seed_Vectors_names

    if (verbose) message("  [", prefix, "] Computing ", method, " between ",
                         length(data_emb_cols), " dims and ",
                         ncol(Seed_Matrix), " seed vectors...")

    # Extract response embedding matrix
    response_matrix <- data[, data_emb_cols, drop = FALSE]

    # Compute similarities row-by-row
    if (method == "correlation") {
      sim_matrix <- t(apply(response_matrix, 1, function(z) {
        stats::cor(as.numeric(z), Seed_Matrix)
      }))
    } else {
      sim_matrix <- t(apply(response_matrix, 1, function(z) {
        cosinize(z, Seed_Matrix)
      }))
    }

    # Name columns
    colnames(sim_matrix) <- paste0(prefix, "_", Seed_Vectors_names)
    sim_df <- as.data.frame(sim_matrix)

    # Overwrite existing columns with same names (avoid duplicate names from cbind)
    existing <- intersect(names(sim_df), names(data))
    if (length(existing) > 0) {
      data <- data[, setdiff(names(data), existing), drop = FALSE]
    }

    # Bind to data
    data <- cbind(data, sim_df)

    # Ensure seed columns are NA when the original response is missing
    data <- .mask_missing_response_cols(
      data,
      response_col,
      cols_or_patterns = names(sim_df)
    )

    total_added <- total_added + ncol(sim_df)
  }

  if (verbose) message("  Seed similarities complete. Added ", total_added, " columns.")
  return(data)
}
