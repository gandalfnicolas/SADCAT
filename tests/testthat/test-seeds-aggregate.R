test_that("seed similarities compute expected correlation and cosine outputs", {
  emb <- simple_embedding_fixture()
  seeds <- simple_seed_vectors()

  cor_out <- suppressWarnings(compute_seed_similarities(emb, seed_vectors = seeds, verbose = FALSE))
  expect_true(all(c("SBERT_Warmth.seed", "SBERT_Competence.seed") %in% names(cor_out)))
  expect_equal(cor_out$SBERT_Warmth.seed[1], 1)
  expect_equal(cor_out$SBERT_Competence.seed[1], -1)
  expect_equal(cor_out$SBERT_Competence.seed[2], 1)
  expect_true(is.na(cor_out$SBERT_Warmth.seed[3]))

  cos_out <- compute_seed_similarities(emb, seed_vectors = seeds, method = "cosine", verbose = FALSE)
  expect_equal(cos_out$SBERT_Warmth.seed[1], 1)
  expect_equal(cos_out$SBERT_Competence.seed[2], 1)
  expect_equal(round(cos_out$SBERT_Warmth.seed[4], 6), round(sqrt(0.5), 6))
})

test_that("seed similarities skip cleanly when embedding columns are missing", {
  dat <- data.frame(response = "warm", Other_1 = 1, stringsAsFactors = FALSE)
  out <- compute_seed_similarities(dat, embedding_prefix = "SBERT", seed_vectors = simple_seed_vectors(), verbose = FALSE)
  expect_identical(names(out), names(dat))
})

test_that("aggregate_responses computes sums, means, and noNA columns", {
  out <- aggregate_responses(pipeline_aggregate_fixture(), verbose = FALSE)
  rownames(out) <- out$Synonym.GroupX

  expect_equal(out["A", "warmth_dic_binary2_Sum"], 1)
  expect_equal(out["A", "ValyNoNA"], 0)
  expect_equal(out["A", "Warmth_Valy"], 1)
  expect_equal(out["A", "Warmth_dirx3"], 1)
  expect_equal(out["A", "Warmth_dirx3noNA"], 1)
  expect_true(is.na(out["B", "Warmth_valy3"]))
  expect_true(is.na(out["B", "Warmth_valy3noNA"]) || out["B", "Warmth_valy3noNA"] == 0)
})

test_that("aggregate_responses validates missing grouping columns", {
  expect_error(
    aggregate_responses(data.frame(x = 1, stringsAsFactors = FALSE), verbose = FALSE),
    "Group columns not found in data"
  )
})
