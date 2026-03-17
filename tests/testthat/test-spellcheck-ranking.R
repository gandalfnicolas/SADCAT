test_that("ranking demotes short ties at minimum edit distance", {
  idx <- SADCAT:::.rank_spellcheck_candidates(
    word = "rde",
    suggestions = c("rd", "rude"),
    dists = c(1L, 1L),
    bg_scores = c(0L, 0L),
    in_rawlist = c(FALSE, FALSE)
  )

  expect_equal(idx[1], 2L)
})

test_that("ranking keeps short candidate when it is uniquely closest", {
  idx <- SADCAT:::.rank_spellcheck_candidates(
    word = "rde",
    suggestions = c("rd", "rude"),
    dists = c(1L, 2L),
    bg_scores = c(0L, 0L),
    in_rawlist = c(FALSE, FALSE)
  )

  expect_equal(idx[1], 1L)
})
