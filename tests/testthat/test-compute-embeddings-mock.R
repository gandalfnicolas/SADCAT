test_that("mocked SBERT embeddings deduplicate text and rejoin by value", {
  fake_model <- list(
    encode = function(text) {
      matrix(seq_len(length(text) * 2), ncol = 2, byrow = TRUE)
    }
  )
  fake_st <- list(
    SentenceTransformer = function(model) {
      fake_model
    }
  )

  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .reticulate_import = function(module) {
      if (module == "sentence_transformers") {
        return(fake_st)
      }
      stop("Unexpected module: ", module)
    },
    .reticulate_py_to_r = function(x) x,
    .env = asNamespace("SADCAT")
  )

  dat <- data.frame(
    row_id = 1:4,
    response = c("a", "b", "a", NA),
    tv = c("hello", "world", "hello", NA),
    stringsAsFactors = FALSE
  )

  out <- compute_embeddings(dat, methods = "sbert", response_col = "response", verbose = FALSE)
  expect_equal(out$SBERT_1[1:3], c(1, 3, 1))
  expect_equal(out$SBERT_2[1:3], c(2, 4, 2))
  expect_true(is.na(out$SBERT_1[4]))
})

test_that("mocked Gemini embeddings stay aligned after partial batch failure", {
  batch_counter <- 0L
  sleep_calls <- numeric(0)

  fake_client <- list(
    models = list(
      embed_content = function(model, contents, config) {
        batch_counter <<- batch_counter + 1L
        if (batch_counter == 1L) {
          stop("simulated batch failure")
        }
        list(
          embeddings = lapply(contents, function(text) {
            list(values = c(nchar(text), nchar(text) + 1))
          })
        )
      }
    )
  )

  fake_genai <- list(Client = function(api_key) fake_client)
  fake_types <- list(
    EmbedContentConfig = function(output_dimensionality, task_type) {
      list(output_dimensionality = output_dimensionality, task_type = task_type)
    }
  )

  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .reticulate_import = function(module) {
      switch(
        module,
        "google.genai" = fake_genai,
        "google.genai.types" = fake_types,
        stop("Unexpected module: ", module)
      )
    },
    .sleep_seconds = function(seconds) {
      sleep_calls <<- c(sleep_calls, seconds)
      invisible(NULL)
    },
    .env = asNamespace("SADCAT")
  )

  dat <- data.frame(
    row_id = 1:4,
    response = c("alpha", "beta", "alpha", "gamma"),
    tv = c("alpha", "beta", "alpha", "gamma"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    out <- compute_embeddings(
      dat,
      methods = "gemini",
      gemini_api_key = "key",
      gemini_batch_size = 1L,
      gemini_sleep = 7,
      verbose = FALSE
    ),
    "simulated batch failure"
  )

  expect_true(all(c("Gemini_1", "Gemini_2") %in% names(out)))
  expect_true(all(is.na(out[out$tv == "alpha", "Gemini_1"])))
  expect_equal(out[out$tv == "beta", "Gemini_1"], 4)
  expect_equal(out[out$tv == "gamma", "Gemini_1"], 5)
  expect_equal(sleep_calls, c(7, 7))
})

test_that("embedding computation skips cleanly when there is no text to encode", {
  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .env = asNamespace("SADCAT")
  )

  dat <- data.frame(response = c(NA, NA), tv = c(NA, ""), stringsAsFactors = FALSE)
  out <- compute_embeddings(dat, methods = "sbert", response_col = "response", verbose = FALSE)
  expect_identical(out, dat)
})
