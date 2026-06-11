# Compute Seed Similarities

Compute similarity between response embeddings and SADCAT seed vectors.
For each response, computes the correlation (or cosine similarity)
between its embedding vector and each SADCAT seed vector. Works for any
embedding prefix (SBERT, Gemini, etc.) as long as matching columns exist
in both the data and seed vectors.

## Usage

``` r
compute_seed_similarities(
  data,
  embedding_prefix = "SBERT",
  seed_vectors = Seed_Vectors_Avg,
  method = "correlation",
  response_col = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame with embedding columns (e.g., SBERT_1:SBERT_768)

- embedding_prefix:

  Prefix (or vector of prefixes) identifying embedding columns (default
  "SBERT"), e.g., `c("SBERT", "Gemini")`.

- seed_vectors:

  Seed vector data. Default: Seed_Vectors_Avg

- method:

  "correlation" (default, uses cor()) or "cosine" (uses lsa::cosine())

- response_col:

  Column used for NA-gating (default: same as text_col, where text_col
  is inferred as the embedding source column). Only needed if your
  NA-indicator column differs from the embedding source.

- verbose:

  Print progress? (default TRUE)

## Value

The input data with new columns: `prefix_SeedName.seed`
