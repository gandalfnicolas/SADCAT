test_that("score_valence produces ValenceYesNA and ValenceNoNA, not Valence", {
  dat <- data.frame(
    response = c("warm", "zzzzword", NA_character_),
    tv = c("warm", "zzzzword", NA_character_),
    stringsAsFactors = FALSE
  )
  out <- suppressWarnings(score_valence(dat, text_col = "tv", response_col = "response"))

  expect_true("ValenceYesNA" %in% names(out))
  expect_true("ValenceNoNA" %in% names(out))
  expect_false("Valence" %in% names(out))
  expect_true(is.na(out$ValenceYesNA[3]))
  expect_true(is.na(out$ValenceNoNA[3]))
})

test_that("match_dictionaries three-column valence semantics are correct", {
  # Use valence_semantics_fixture: "warm" matches Warmth dictionary.
  # Row 1: warm, ValenceYesNA=0.5, ValenceNoNA=0.5  -> tagged, has sentiment
  # Row 2: warm, ValenceYesNA=NA,  ValenceNoNA=0    -> tagged, no sentiment words
  # Row 3: neutral word, ValenceYesNA=NA, ValenceNoNA=0 -> NOT tagged
  # Row 4: response=NA                               -> missing
  dat <- valence_semantics_fixture()

  matched <- suppressWarnings(match_dictionaries(
    dat,
    text_col = "tv3",
    response_col = "response",
    valence_col = "ValenceYesNA",
    valence_nona_col = "ValenceNoNA"
  ))

  # _Valence: NA iff binary==0 (not tagged) or response is NA
  expect_equal(matched$Warmth_Valence[1], 0.5)   # tagged + sentiment
  expect_equal(matched$Warmth_Valence[2], 0)      # tagged, no sentiment -> ValenceNoNA=0
  expect_true(is.na(matched$Warmth_Valence[3]))   # not tagged
  expect_true(is.na(matched$Warmth_Valence[4]))   # NA response

  # _valenceStrictNA: NA iff not tagged OR ValenceYesNA is NA
  expect_equal(matched$Warmth_valenceStrictNA[1], 0.5)  # tagged + ValenceYesNA=0.5
  expect_true(is.na(matched$Warmth_valenceStrictNA[2])) # tagged BUT ValenceYesNA=NA
  expect_true(is.na(matched$Warmth_valenceStrictNA[3])) # not tagged
  expect_true(is.na(matched$Warmth_valenceStrictNA[4])) # NA response

  # _valenceNoNA: 0 iff not tagged; NA only for missing response
  expect_equal(matched$Warmth_valenceNoNA[1], 0.5) # tagged + sentiment
  expect_equal(matched$Warmth_valenceNoNA[2], 0)   # tagged, no sentiment -> ValenceNoNA=0
  expect_equal(matched$Warmth_valenceNoNA[3], 0)   # not tagged -> 0
  expect_true(is.na(matched$Warmth_valenceNoNA[4]))  # NA response
})

test_that("_Valence aggregate mean treats sentiment-less tagged responses as 0", {
  dat <- valence_semantics_fixture()

  matched <- suppressWarnings(match_dictionaries(
    dat,
    text_col = "tv3",
    response_col = "response",
    valence_col = "ValenceYesNA",
    valence_nona_col = "ValenceNoNA"
  ))

  tagged_rows <- !is.na(matched$Warmth_Valence)
  tagged_vals <- matched$Warmth_Valence[tagged_rows]
  # Rows 1 and 2 are tagged (row 3 not tagged = NA, row 4 NA response = NA)
  # Values: 0.5 and 0.0
  expect_equal(mean(matched$Warmth_Valence, na.rm = TRUE), mean(c(0.5, 0)))
})

test_that("all three Warmth valence columns exist after match_dictionaries", {
  dat <- valence_semantics_fixture()
  matched <- suppressWarnings(match_dictionaries(
    dat,
    text_col = "tv3",
    response_col = "response",
    valence_col = "ValenceYesNA",
    valence_nona_col = "ValenceNoNA"
  ))

  expect_true("Warmth_Valence" %in% names(matched))
  expect_true("Warmth_valenceStrictNA" %in% names(matched))
  expect_true("Warmth_valenceNoNA" %in% names(matched))
  expect_false("Warmth_valence" %in% names(matched))
  expect_false("Warmth_Valy" %in% names(matched))
})
