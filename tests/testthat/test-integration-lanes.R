test_that("WordNet and Java integration lane is opt-in", {
  skip_if(Sys.getenv("SADCAT_RUN_WORDNET_TESTS") != "true", "Set SADCAT_RUN_WORDNET_TESTS=true to run WordNet integration tests")
  skip_if_not(has_wordnet_test_env(), "WordNet dictionary path not configured")
  skip_if_not(has_treetagger_test_env(), "TreeTagger path not configured")
  skip_if(!nzchar(Sys.getenv("JAVA_HOME")), "JAVA_HOME is not configured")

  wordnet::setDict(wordnet_test_path())

  expect_true(is.character(Spellcheck2("warm", rawlist = c("warm"))))

  dat <- data.frame(
    responsex = c("friendly", "warm"),
    response = c("friendly", "warm"),
    stringsAsFactors = FALSE
  )

  out <- preprocess_text(
    dat,
    spellcheck = TRUE,
    singularize = FALSE,
    java_home = Sys.getenv("JAVA_HOME"),
    wordnet_dict = wordnet_test_path(),
    verbose = FALSE
  )

  expect_true(all(c("tv", "tv2", "tv3") %in% names(out)))
})

test_that("SBERT integration lane is opt-in", {
  skip_if(Sys.getenv("SADCAT_RUN_SBERT_TESTS") != "true", "Set SADCAT_RUN_SBERT_TESTS=true to run SBERT integration tests")
  skip_if_not_installed("reticulate")

  dat <- data.frame(
    response = c("a", "a", "b"),
    tv = c("warm", "warm", "cold"),
    stringsAsFactors = FALSE
  )

  out <- compute_embeddings(dat, methods = "sbert", response_col = "response", verbose = FALSE)
  sbert_cols <- grep("^SBERT_\\d+$", names(out), value = TRUE)

  expect_true(length(sbert_cols) > 0)
  expect_equal(out[1, sbert_cols], out[2, sbert_cols])
})

test_that("Gemini integration lane is opt-in", {
  skip_if(Sys.getenv("SADCAT_RUN_GEMINI_TESTS") != "true", "Set SADCAT_RUN_GEMINI_TESTS=true to run Gemini integration tests")
  skip_if_not_installed("reticulate")
  skip_if(!nzchar(Sys.getenv("GEMINI_API_KEY")), "GEMINI_API_KEY is not configured")

  dat <- data.frame(
    response = c("a", "b"),
    tv = c("warm", "cold"),
    stringsAsFactors = FALSE
  )

  out <- compute_embeddings(
    dat,
    methods = "gemini",
    gemini_api_key = Sys.getenv("GEMINI_API_KEY"),
    gemini_batch_size = 1L,
    gemini_sleep = 0,
    response_col = "response",
    verbose = FALSE
  )

  gemini_cols <- grep("^Gemini_\\d+$", names(out), value = TRUE)
  expect_true(length(gemini_cols) > 0)
})
