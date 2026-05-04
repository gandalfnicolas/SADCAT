test_that("Spellcheck2 short-circuits NA-string and NA value", {
  expect_equal(Spellcheck2("NA", dict_tv = character(), rawlist = character()), "na")
  expect_equal(Spellcheck2(NA_character_, dict_tv = character(), rawlist = character()), "na")
})

test_that("Spellcheck2 treats n/a variants as 'na' (no hunspell mangling)", {
  expect_equal(Spellcheck2("n/a",   dict_tv = character(), rawlist = character()), "na")
  expect_equal(Spellcheck2("N/A",   dict_tv = character(), rawlist = character()), "na")
  expect_equal(Spellcheck2(" n/a ", dict_tv = character(), rawlist = character()), "na")
  expect_equal(Spellcheck2("n.a.",  dict_tv = character(), rawlist = character()), "na")
  expect_equal(Spellcheck2("#N/A",  dict_tv = character(), rawlist = character()), "na")
})

test_that("Spellcheck2 treats letterless/punctuation-only inputs as 'na'", {
  expect_equal(Spellcheck2("?",   dict_tv = character(), rawlist = character()), "na")
  expect_equal(Spellcheck2("--",  dict_tv = character(), rawlist = character()), "na")
  expect_equal(Spellcheck2("...", dict_tv = character(), rawlist = character()), "na")
  expect_equal(Spellcheck2("!?",  dict_tv = character(), rawlist = character()), "na")
})
