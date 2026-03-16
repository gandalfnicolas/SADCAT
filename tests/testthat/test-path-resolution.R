test_that("java resolver accepts home, bin, and executable-like inputs", {
  java_home <- tempfile("sadcat-java-home-")
  dir.create(file.path(java_home, "bin"), recursive = TRUE, showWarnings = FALSE)
  java_exe <- file.path(java_home, "bin", if (.Platform$OS.type == "windows") "java.exe" else "java")
  file.create(java_exe)
  on.exit(unlink(java_home, recursive = TRUE, force = TRUE), add = TRUE)

  expected <- normalizePath(java_home, winslash = "/", mustWork = FALSE)

  expect_equal(SADCAT:::.resolve_java_home(java_home), expected)
  expect_equal(SADCAT:::.resolve_java_home(file.path(java_home, "bin")), expected)
  expect_equal(SADCAT:::.resolve_java_home(java_exe), expected)
})

test_that("wordnet resolver accepts dict path and parent path", {
  wn_root <- tempfile("sadcat-wordnet-root-")
  wn_dict <- file.path(wn_root, "dict")
  dir.create(wn_dict, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(wn_dict, "index.noun"))
  on.exit(unlink(wn_root, recursive = TRUE, force = TRUE), add = TRUE)

  expected <- normalizePath(wn_dict, winslash = "/", mustWork = FALSE)

  expect_equal(SADCAT:::.resolve_wordnet_dict(wn_dict), expected)
  expect_equal(SADCAT:::.resolve_wordnet_dict(wn_root), expected)
})

test_that("invalid explicit paths fall back to environment variables", {
  java_home <- tempfile("sadcat-java-env-")
  dir.create(file.path(java_home, "bin"), recursive = TRUE, showWarnings = FALSE)
  java_exe <- file.path(java_home, "bin", if (.Platform$OS.type == "windows") "java.exe" else "java")
  file.create(java_exe)

  wn_root <- tempfile("sadcat-wordnet-env-")
  wn_dict <- file.path(wn_root, "dict")
  dir.create(wn_dict, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(wn_dict, "index.noun"))

  on.exit(unlink(java_home, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(wn_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_java_home <- Sys.getenv("JAVA_HOME", unset = NA)
  old_wordnet <- Sys.getenv("SADCAT_WORDNET_DICT", unset = NA)
  on.exit({
    if (is.na(old_java_home)) {
      Sys.unsetenv("JAVA_HOME")
    } else {
      Sys.setenv(JAVA_HOME = old_java_home)
    }
    if (is.na(old_wordnet)) {
      Sys.unsetenv("SADCAT_WORDNET_DICT")
    } else {
      Sys.setenv(SADCAT_WORDNET_DICT = old_wordnet)
    }
  }, add = TRUE)

  Sys.setenv(JAVA_HOME = java_home)
  Sys.setenv(SADCAT_WORDNET_DICT = wn_dict)

  missing_path <- tempfile("sadcat-missing-path-")
  expected_java <- normalizePath(java_home, winslash = "/", mustWork = FALSE)
  expected_wordnet <- normalizePath(wn_dict, winslash = "/", mustWork = FALSE)

  expect_equal(SADCAT:::.resolve_java_home(missing_path), expected_java)
  expect_equal(SADCAT:::.resolve_wordnet_dict(missing_path), expected_wordnet)
})
