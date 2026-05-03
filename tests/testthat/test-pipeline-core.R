test_that("dictionary preparation returns stable category names", {
  sadcat_dict <- prepare_sadcat_dictionaries(cache = FALSE)
  socats_dict <- prepare_socats_dictionaries(cache = FALSE)

  expect_true(all(c("Warmth_dic", "Warmth_dic_hi", "Warmth_dic_lo") %in% names(sadcat_dict)))
  expect_true(all(c("Age_dic", "Women_dic", "Black_dic") %in% names(socats_dict)))
})

test_that("canonical fixture stays row-aligned through preprocess, valence, and dictionaries", {
  dat <- canonical_response_fixture()

  prepped <- suppressWarnings(preprocess_text(dat, spellcheck = FALSE, singularize = FALSE, verbose = FALSE))
  scored <- suppressWarnings(score_valence(prepped, text_col = "tv", response_col = "response"))
  matched <- suppressWarnings(match_dictionaries(
    scored,
    text_col = "tv3",
    response_col = "response",
    valence_col = "ValenceYesNA",
    valence_nona_col = "ValenceNoNA"
  ))
  rownames(matched) <- matched$row_id

  expect_equal(matched$row_id, dat$row_id)
  expect_equal(nrow(matched), nrow(dat))
  expect_equal(matched["1", "Warmth_direction"], 1)
  expect_equal(matched["2", "Warmth_direction"], -1)
  expect_equal(matched["8", "Morality_direction"], 1)
  expect_equal(matched["9", "Warmth_direction"], 1)
  expect_equal(matched["9", "Competence_direction"], -1)
  expect_equal(matched["25", "Warmth_direction"], 1)
  expect_equal(matched["26", "Warmth_direction"], 1)
})

test_that("SOCATS matching runs offline and applies the age heuristic", {
  dat <- data.frame(
    response = c("25 year old woman", "black engineer"),
    tv = c("25 year old woman", "black engineer"),
    tv3 = c("25 year old woman", "black engineer"),
    stringsAsFactors = FALSE
  )

  scored <- suppressWarnings(score_valence(dat, text_col = "tv", response_col = "response"))
  matched <- suppressWarnings(match_dictionaries(
    scored,
    text_col = "tv3",
    response_col = "response",
    valence_col = "ValenceYesNA",
    valence_nona_col = "ValenceNoNA",
    socats = TRUE
  ))

  expect_true(all(c("age_dic", "women_dic", "black_dic", "jobs_dic") %in% names(matched)))
  expect_equal(matched$age_dic[1], 1)
  expect_true(matched$jobs_dic[2] >= 0)
})

test_that("adversarial fixtures do not break the offline core pipeline", {
  dat <- adversarial_response_fixture()

  prepped <- suppressWarnings(preprocess_text(dat, spellcheck = FALSE, singularize = FALSE, verbose = FALSE))
  scored <- suppressWarnings(score_valence(prepped, text_col = "tv", response_col = "response"))
  matched <- suppressWarnings(match_dictionaries(
    scored,
    text_col = "tv3",
    response_col = "response",
    valence_col = "ValenceYesNA",
    valence_nona_col = "ValenceNoNA"
  ))

  expect_equal(matched$row_id, dat$row_id)
  expect_equal(nrow(matched), nrow(dat))
})

test_that("process_responses handles the offline core stages and validates prerequisites", {
  dat <- canonical_response_fixture()[, c("row_id", "responsex", "response")]

  out <- suppressWarnings(process_responses(
    dat,
    stages = c("preprocess", "valence", "dictionaries"),
    spellcheck = FALSE,
    singularize_text = FALSE,
    verbose = FALSE
  ))

  expect_equal(out$long$row_id, dat$row_id)
  expect_true(all(c("tv", "tv2", "tv3", "ValenceYesNA", "ValenceNoNA", "Warmth_direction") %in% names(out$long)))

  expect_error(
    process_responses(
      data.frame(response = "warm", stringsAsFactors = FALSE),
      stages = c("dictionaries"),
      verbose = FALSE
    ),
    "match_dictionaries\\(\\) requires missing column\\(s\\): tv3"
  )
})

test_that("valenceNoNA keeps 0 for dimension-absent responses and NA for missing responses", {
  dat <- data.frame(
    responsex = c("zzzzword", "warm", NA_character_),
    response = c("zzzzword", "warm", NA_character_),
    stringsAsFactors = FALSE
  )

  prepped <- suppressWarnings(preprocess_text(dat, spellcheck = FALSE, singularize = FALSE, verbose = FALSE))
  scored <- suppressWarnings(score_valence(prepped, text_col = "tv", response_col = "response"))
  matched <- suppressWarnings(match_dictionaries(
    scored,
    text_col = "tv3",
    response_col = "response",
    valence_col = "ValenceYesNA",
    valence_nona_col = "ValenceNoNA"
  ))

  expect_equal(matched$Warmth_valenceNoNA[1], 0)
  expect_true(!is.na(matched$Warmth_valenceNoNA[2]))
  expect_true(is.na(matched$Warmth_valenceNoNA[3]))
  expect_true(is.na(matched$Warmth_Valence[1]))   # row 1: "zzzzword" not tagged with Warmth
  expect_false(is.na(matched$Warmth_Valence[2]))  # row 2: "warm" tagged with Warmth
  expect_true(is.na(matched$Warmth_Valence[3]))   # row 3: NA response
})
