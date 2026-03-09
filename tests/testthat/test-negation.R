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
