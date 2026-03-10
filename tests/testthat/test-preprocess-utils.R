test_that("clean_large_text expands contractions and trims spacing", {
  out <- clean_large_text(" It's warm.  ")
  expect_match(out, "it is", ignore.case = TRUE)
  expect_false(grepl("'", out, fixed = TRUE))
})

test_that("clean_naresponses preserves character type for missing sentinels", {
  out <- clean_naresponses(c("warm", "NA", "  no  "))
  expect_type(out, "character")
  expect_equal(out[1], "warm")
  expect_true(is.na(out[2]))
  expect_true(is.na(out[3]))
})

test_that("basic text preprocessing preserves duplicates and missing rows offline", {
  dat <- data.frame(
    row_id = 1:4,
    responsex = c("Warm", "Warm", "", NA_character_),
    stringsAsFactors = FALSE
  )

  out <- preprocess_text(
    dat,
    spellcheck = FALSE,
    singularize = FALSE,
    verbose = FALSE
  )

  expect_equal(out$row_id, dat$row_id)
  expect_equal(out$tv, c("warm", "warm", NA, NA))
  expect_equal(out$tv2, out$tv)
  expect_equal(out$tv3, out$tv)
})

test_that("preprocess and valence functions validate missing columns clearly", {
  expect_error(
    preprocess_text(data.frame(other = "warm", stringsAsFactors = FALSE), verbose = FALSE),
    "preprocess_text\\(\\) requires missing column\\(s\\): responsex"
  )

  expect_error(
    score_valence(data.frame(response = "warm", stringsAsFactors = FALSE), text_col = "tv", response_col = "response"),
    "score_valence\\(\\) requires missing column\\(s\\): tv"
  )
})

test_that("symbol cleaning and singularization helpers stay stable", {
  expect_equal(clean_symbols("warm!"), "warm")
  expect_equal(delete_ending_Ss("dogs"), "dog")
  expect_equal(singularize2("dogs", dictionary = FALSE), "dog")
})
