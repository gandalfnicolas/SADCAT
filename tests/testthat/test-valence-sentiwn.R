test_that("score_valence default now includes sentiwn, warriner, jockers", {
  dat <- data.frame(
    response = c("warm", "kind", "sad", "neutral_zzz"),
    tv       = c("warm", "kind", "sad", "neutral_zzz"),
    stringsAsFactors = FALSE
  )
  out <- suppressWarnings(
    score_valence(dat, text_col = "tv", response_col = "response")
  )

  expect_true("Val_sentiwn"   %in% names(out))
  expect_true("Val_sentiwnNA" %in% names(out))
  expect_true("Val_warriner"  %in% names(out))
  expect_true("Val_warrinerNA" %in% names(out))
  expect_true("Val_jockers"   %in% names(out))
  expect_true("Val_jockersNA" %in% names(out))
})

test_that("score_valence with include_sentiwn=FALSE drops sentiwn columns", {
  dat <- data.frame(
    response = c("warm", "kind"),
    tv       = c("warm", "kind"),
    stringsAsFactors = FALSE
  )
  out <- suppressWarnings(
    score_valence(dat, text_col = "tv", response_col = "response",
                  include_sentiwn = FALSE)
  )

  expect_false("Val_sentiwn"   %in% names(out))
  expect_false("Val_sentiwnNA" %in% names(out))
  expect_true("Val_warriner" %in% names(out))
  expect_true("Val_jockers"  %in% names(out))
})

test_that("score_valence with all extras off matches the prior 5-dict shape", {
  dat <- data.frame(
    response = c("warm", "kind"),
    tv       = c("warm", "kind"),
    stringsAsFactors = FALSE
  )
  out <- suppressWarnings(
    score_valence(dat, text_col = "tv", response_col = "response",
                  include_sentiwn = FALSE,
                  include_warriner = FALSE,
                  include_jockers = FALSE)
  )

  expect_false("Val_sentiwn"   %in% names(out))
  expect_false("Val_warriner"  %in% names(out))
  expect_false("Val_jockers"   %in% names(out))
  for (col in c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin",
                "Val_loughran", "ValenceYesNA", "ValenceNoNA")) {
    expect_true(col %in% names(out))
  }
})

test_that("Warriner and Jockers are continuous in [-1, 1]", {
  dat <- data.frame(
    response = c("kind", "lazy", "cultured", "religious", "zzzz_unknown"),
    tv       = c("kind", "lazy", "cultured", "religious", "zzzz_unknown"),
    stringsAsFactors = FALSE
  )
  out <- suppressWarnings(
    score_valence(dat, text_col = "tv", response_col = "response")
  )

  for (col in c("Val_warriner", "Val_jockers")) {
    v <- out[[col]]
    expect_true(all(is.na(v) | (v >= -1 - 1e-12 & v <= 1 + 1e-12)),
                info = paste("col:", col))
  }
  warr_finite <- out$Val_warriner[!is.na(out$Val_warriner) &
                                    out$Val_warriner != 0]
  expect_true(any(abs(warr_finite) > 0 & abs(warr_finite) < 1))
})

test_that("Warriner / Jockers / SentiWordNet rescue otherwise-no-match texts", {
  dat <- data.frame(
    response = c("driven", "cultured", "zzzz_unknown_word"),
    tv       = c("driven", "cultured", "zzzz_unknown_word"),
    stringsAsFactors = FALSE
  )

  out_off <- suppressWarnings(
    score_valence(dat, text_col = "tv", response_col = "response",
                  include_sentiwn = FALSE,
                  include_warriner = FALSE,
                  include_jockers = FALSE)
  )
  out_on <- suppressWarnings(
    score_valence(dat, text_col = "tv", response_col = "response")
  )

  expect_true(is.na(out_off$ValenceYesNA[1]))
  expect_true(is.na(out_off$ValenceYesNA[2]))
  expect_false(is.na(out_on$ValenceYesNA[1]))
  expect_false(is.na(out_on$ValenceYesNA[2]))
  expect_true(is.na(out_off$ValenceYesNA[3]))
  expect_true(is.na(out_on$ValenceYesNA[3]))
})

test_that("sentiwn_threshold actually filters which words count", {
  dat <- data.frame(
    response = c("religious"),
    tv       = c("religious"),
    stringsAsFactors = FALSE
  )

  out_high <- suppressWarnings(
    score_valence(dat, text_col = "tv", response_col = "response",
                  include_sentiwn = TRUE, sentiwn_threshold = 0.25,
                  include_warriner = FALSE, include_jockers = FALSE)
  )
  out_low <- suppressWarnings(
    score_valence(dat, text_col = "tv", response_col = "response",
                  include_sentiwn = TRUE, sentiwn_threshold = 0.01,
                  include_warriner = FALSE, include_jockers = FALSE)
  )

  expect_true(is.na(out_high$ValenceYesNA[1]))
  expect_false(is.na(out_low$ValenceYesNA[1]))
})

test_that("Negation flips Warriner continuous values in the combined score", {
  dat <- data.frame(
    response = c("kind", "not kind"),
    tv       = c("kind", "not kind"),
    stringsAsFactors = FALSE
  )
  out <- suppressWarnings(
    score_valence(dat, text_col = "tv", response_col = "response",
                  include_sentiwn = FALSE,
                  include_warriner = TRUE, include_jockers = FALSE)
  )

  # Val_warriner is the RAW (no-negation) per-dict score: positive for both.
  expect_gt(out$Val_warriner[1], 0)
  expect_gt(out$Val_warriner[2], 0)
  # ValenceYesNA uses the negation-aware value: positive for 'kind',
  # negative for 'not kind'.
  expect_gt(out$ValenceYesNA[1], 0)
  expect_lt(out$ValenceYesNA[2], 0)
})
