library(stringr)

scoped_tokens <- function(text) {
  spec <- SADCAT:::.get_sadcat_direction_negation_spec(SADCAT::prepare_sadcat_dictionaries())
  profile <- SADCAT:::.build_scoped_token_profile(
    text = text,
    normalizer = SADCAT:::.normalize_sadcat_text,
    cue_tokens = spec$cue_tokens,
    compound_patterns = spec$compound_patterns,
    singularize_words = TRUE
  )

  keep_idx <- !vapply(profile$tokens[[1]], SADCAT:::.is_punctuation_token, logical(1))
  data.frame(
    token = profile$tokens[[1]][keep_idx],
    negated = profile$negated[[1]][keep_idx],
    stringsAsFactors = FALSE
  )
}

test_that("scoped negation marks only the intended local tokens", {
  toks <- scoped_tokens("not warm")
  expect_true(toks$negated[toks$token == "warm"])

  toks <- scoped_tokens("non-friendly")
  expect_true(toks$negated[toks$token == "friendly"])

  toks <- scoped_tokens("warm but not competent")
  expect_false(toks$negated[toks$token == "warm"])
  expect_true(toks$negated[toks$token == "competent"])

  toks <- scoped_tokens("not warm and friendly")
  expect_true(toks$negated[toks$token == "warm"])
  expect_true(toks$negated[toks$token == "friendly"])

  toks <- scoped_tokens("limited competence but warm")
  expect_true(toks$negated[toks$token == "competence"])
  expect_false(toks$negated[toks$token == "warm"])
})

test_that("score_valence flips negated sentiment locally across dictionaries", {
  dat <- data.frame(
    response = c(
      "friendly", "not friendly",
      "immoral", "not immoral",
      "incompetent", "not incompetent",
      "warm", "not warm",
      "warm but not competent"
    ),
    tv = c(
      "friendly", "not friendly",
      "immoral", "not immoral",
      "incompetent", "not incompetent",
      "warm", "not warm",
      "warm but not competent"
    ),
    stringsAsFactors = FALSE
  )

  scored <- SADCAT::score_valence(dat, text_col = "tv", response_col = "response")
  rownames(scored) <- scored$response

  expect_equal(unname(as.numeric(scored["friendly", c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")])), rep(1, 5))
  expect_equal(unname(as.numeric(scored["not friendly", c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")])), rep(-1, 5))

  expect_equal(unname(as.numeric(scored["immoral", c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")])), c(-1, -1, -1, 0, -1))
  expect_equal(unname(as.numeric(scored["not immoral", c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")])), c(1, 1, 1, 0, 1))

  expect_equal(unname(as.numeric(scored["incompetent", c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")])), rep(-1, 5))
  expect_equal(unname(as.numeric(scored["not incompetent", c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")])), rep(1, 5))

  expect_equal(unname(as.numeric(scored["warm", c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")])), c(1, 0, 1, 1, 0))
  expect_equal(unname(as.numeric(scored["not warm", c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")])), c(-1, 0, -1, -1, 0))

  expect_equal(scored["warm but not competent", "Val_NRC"], -1)
  expect_equal(scored["warm but not competent", "Val_bing"], 1)
  expect_lt(scored["warm but not competent", "Valy"], scored["warm", "Valy"])
})

test_that("match_dictionaries keeps negation local in directional outputs", {
  dat <- data.frame(
    response = c(
      "not warm",
      "non-friendly",
      "not immoral",
      "not incompetent",
      "warm but not competent"
    ),
    tv = c(
      "not warm",
      "non-friendly",
      "not immoral",
      "not incompetent",
      "warm but not competent"
    ),
    tv3 = c(
      "not warm",
      "non-friendly",
      "not immoral",
      "not incompetent",
      "warm but not competent"
    ),
    stringsAsFactors = FALSE
  )

  scored <- SADCAT::score_valence(dat, text_col = "tv", response_col = "response")
  matched <- SADCAT::match_dictionaries(
    scored,
    text_col = "tv3",
    response_col = "response",
    valence_col = "ValyNA",
    valence_raw_col = "Valy"
  )
  rownames(matched) <- matched$response

  expect_equal(unname(as.numeric(matched["not warm", c("Warmth_dirx", "Warmth_dirx2", "Warmth_dirx3")])), rep(-1, 3))
  expect_equal(unname(as.numeric(matched["non-friendly", c("Warmth_dirx", "Warmth_dirx2", "Warmth_dirx3")])), rep(-1, 3))

  expect_equal(unname(as.numeric(matched["not immoral", c("Morality_dirx", "Morality_dirx2", "Morality_dirx3")])), rep(1, 3))
  expect_equal(unname(as.numeric(matched["not incompetent", c("Ability_dirx", "Ability_dirx2", "Ability_dirx3")])), rep(1, 3))

  expect_equal(matched["warm but not competent", "Warmth_dirx"], 1)
  expect_equal(matched["warm but not competent", "Warmth_dirx2"], 1)
  expect_equal(matched["warm but not competent", "Warmth_dirx3"], 1)
  expect_equal(matched["warm but not competent", "Competence_dirx"], -1)
  expect_equal(matched["warm but not competent", "Competence_dirx2"], -1)
  expect_equal(matched["warm but not competent", "Competence_dirx3"], -1)
})

test_that("quanteda tokenization uses safe docnames for duplicate text", {
  toks <- SADCAT:::.tokenize_quanteda_text(
    c("warm", "warm", "warm!", "Warm"),
    prefix = "dup",
    remove_numbers = FALSE,
    remove_punct = TRUE,
    remove_symbols = TRUE
  )

  expect_equal(names(quanteda::as.list(toks)), paste0("dup_", 1:4))
})

test_that("match_dictionaries handles identical and normalized-duplicate responses", {
  dat <- data.frame(
    row_id = 1:4,
    response = c("warm", "warm", "warm!", "Warm"),
    tv = c("warm", "warm", "warm!", "Warm"),
    tv3 = c("warm", "warm", "warm!", "Warm"),
    stringsAsFactors = FALSE
  )

  scored <- SADCAT::score_valence(dat, text_col = "tv", response_col = "response")
  matched <- NULL
  expect_no_error({
    matched <- SADCAT::match_dictionaries(
      scored,
      text_col = "tv3",
      response_col = "response",
      valence_col = "ValyNA",
      valence_raw_col = "Valy"
    )
  })

  expect_equal(matched$row_id, dat$row_id)
  expect_equal(nrow(matched), nrow(dat))
  expect_equal(anyDuplicated(matched$doc_id), 0)
  expect_equal(matched$Warmth_dirx, rep(1, 4))
})

test_that("process_responses completes through dictionary matching with duplicate texts", {
  dat <- data.frame(
    row_id = 1:4,
    responsex = c("warm", "warm", "warm!", "Warm"),
    response = c("warm", "warm", "warm!", "Warm"),
    stringsAsFactors = FALSE
  )

  result <- NULL
  expect_no_error({
    result <- SADCAT::process_responses(
      dat,
      stages = c("preprocess", "valence", "dictionaries"),
      spellcheck = FALSE,
      singularize_text = FALSE,
      verbose = FALSE
    )
  })

  expect_equal(result$long$row_id, dat$row_id)
  expect_equal(nrow(result$long), nrow(dat))
  expect_equal(anyDuplicated(result$long$doc_id), 0)
  expect_equal(result$long$Warmth_dirx, rep(1, 4))
})
