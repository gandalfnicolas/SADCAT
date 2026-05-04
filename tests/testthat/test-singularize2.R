test_that("singularize2 handles -es plural forms", {
  expect_equal(singularize2("ladies"), "lady")
  expect_equal(singularize2("valves"), "valve")
  expect_equal(singularize2("boxes"),  "box")
})

test_that("singularize2 handles irregular plurals via the switch", {
  expect_equal(singularize2("men"),   "man")
  expect_equal(singularize2("geese"), "goose")
})

test_that("singularize2 handles -ses / -zes plurals", {
  expect_equal(singularize2("freezes"), "freeze")
  expect_equal(singularize2("noses"),   "nose")
  expect_equal(singularize2("houses"),  "house")
  expect_equal(singularize2("roses"),   "rose")
  expect_equal(singularize2("phases"),  "phase")
  expect_equal(singularize2("buses"),   "bus")
  expect_equal(singularize2("gases"),   "gas")
  expect_equal(singularize2("kisses"),  "kiss")
  expect_equal(singularize2("classes"), "class")
  expect_equal(singularize2("quizzes"), "quiz")
})

test_that("singularize2 leaves dictionary singulars alone", {
  expect_equal(singularize2("dog"), "dog")
})

test_that("singularize2 does not mangle very short inputs", {
  expect_equal(singularize2("a"),  "a")
  expect_equal(singularize2("i"),  "i")
  expect_equal(singularize2("as"), "as")
  expect_equal(singularize2("is"), "is")
  expect_equal(singularize2(""),   "")
})

test_that("singularize2 does not strip final s from -ss singulars", {
  expect_equal(singularize2("mass"), "mass")
  expect_equal(singularize2("pass"), "pass")
  expect_equal(singularize2("boss"), "boss")
  expect_equal(singularize2("kiss"), "kiss")
})

test_that("singularize2 falls back to original for over-short candidates", {
  expect_equal(singularize2("bus"),  "bus")
  expect_equal(singularize2("axes"), "axes")
})
