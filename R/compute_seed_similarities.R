#' @title Compute Seed Similarities
#'
#' @description Compute similarity between response embeddings and SADCAT seed vectors.
#' For each response, computes the correlation (or cosine similarity) between
#' its embedding vector and each SADCAT seed vector. Works for any embedding
#' prefix (SBERT, Gemini, etc.) as long as matching columns exist in both the
#' data and seed vectors.
#' @param data A data.frame with embedding columns (e.g., SBERT_1:SBERT_768)
#' @param embedding_prefix Prefix identifying embedding columns (default "SBERT")
#' @param seed_vectors Seed vector data. Default: Seed_Vectors_Avg
#' @param method "correlation" (default, uses cor()) or "cosine" (uses lsa::cosine())
#' @param verbose Print progress? (default TRUE)
#' @return The input data with new columns: \code{prefix_SeedName.seed}
#' @export compute_seed_similarities

compute_seed_similarities <- function(data,
                                      embedding_prefix = "SBERT",
                                      seed_vectors = Seed_Vectors_Avg,
                                      method = "correlation",
                                      verbose = TRUE) {
  message("--- Stage 6: Computing seed similarities (", embedding_prefix, ") ---")

  # Check if seed vectors have columns matching the prefix
  seed_emb_cols <- grep(paste0("^", embedding_prefix, "_"), names(seed_vectors), value = TRUE)
  if (length(seed_emb_cols) == 0) {
    message("  No columns matching prefix '", embedding_prefix,
            "' found in seed_vectors. Skipping seed similarity computation.")
    return(data)
  }

  # Check if data has columns matching the prefix
  data_emb_cols <- grep(paste0("^", embedding_prefix, "_\\d+$"), names(data), value = TRUE)
  if (length(data_emb_cols) == 0) {
    message("  No embedding columns matching prefix '", embedding_prefix,
            "' found in data. Skipping seed similarity computation.")
    return(data)
  }

  # Extract seed info
  Seed_Vectors_names <- paste0(as.character(seed_vectors$Dictionary), ".seed")

  # Extract seed embedding matrix (dims x seeds)
  Seed_Matrix <- seed_vectors %>%
    dplyr::select(dplyr::all_of(seed_emb_cols)) %>%
    t()

  colnames(Seed_Matrix) <- Seed_Vectors_names

  if (verbose) message("  Computing ", method, " between ",
                       length(data_emb_cols), " dims and ",
                       ncol(Seed_Matrix), " seed vectors...")

  # Extract response embedding matrix
  response_matrix <- data[, data_emb_cols, drop = FALSE]

  # Compute similarities row-by-row
  if (method == "correlation") {
    sim_matrix <- t(apply(response_matrix, 1, function(z) {
      stats::cor(as.numeric(z), Seed_Matrix)
    }))
  } else if (method == "cosine") {
    sim_matrix <- t(apply(response_matrix, 1, function(z) {
      cosinize(z, Seed_Matrix)
    }))
  } else {
    stop("method must be 'correlation' or 'cosine'")
  }

  # Name columns
  colnames(sim_matrix) <- paste0(embedding_prefix, "_", Seed_Vectors_names)
  sim_df <- as.data.frame(sim_matrix)

  # Bind to data
  data <- cbind(data, sim_df)

  if (verbose) message("  Seed similarities complete. Added ", ncol(sim_df), " columns.")
  return(data)
}
