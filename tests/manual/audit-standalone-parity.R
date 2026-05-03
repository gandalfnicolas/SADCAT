library(pkgload)
library(dplyr)
library(magrittr)
library(stringr)

pkgload::load_all(".")
source(file.path("tests", "testthat", "helper-fixtures.R"))

script_env <- new.env(parent = globalenv())
sys.source("sadcat_pipeline_functions.R", envir = script_env)

fixture <- canonical_response_fixture()[, c("row_id", "responsex", "response")]

pkg_long <- suppressWarnings(
  process_responses(
    fixture,
    stages = c("preprocess", "valence", "dictionaries"),
    spellcheck = FALSE,
    singularize_text = FALSE,
    verbose = FALSE
  )$long
)

script_long <- tryCatch(
  suppressWarnings(
    script_env$process_responses(
      fixture,
      stages = c("preprocess", "valence", "dictionaries"),
      spellcheck = FALSE,
      singularize_text = FALSE,
      verbose = FALSE
    )$long
  ),
  error = function(e) e
)

if (inherits(script_long, "error")) {
  cat("Standalone script failed before parity comparison:\n")
  cat(conditionMessage(script_long), "\n")
  quit(status = 1)
}

compare_cols <- c(
  "tv", "tv2", "tv3", "ValenceYesNA", "ValenceNoNA",
  "Warmth_dirx", "Warmth_dirx2", "Warmth_dirx3",
  "Competence_dirx", "Morality_dirx"
)

mismatches <- character(0)
for (col in compare_cols) {
  if (!(col %in% names(pkg_long)) || !(col %in% names(script_long))) {
    mismatches <- c(mismatches, paste0(col, ": missing from one output"))
    next
  }
  same <- isTRUE(all.equal(pkg_long[[col]], script_long[[col]], check.attributes = FALSE))
  if (!same) {
    mismatches <- c(mismatches, paste0(col, ": values differ"))
  }
}

if (!length(mismatches)) {
  cat("No package-vs-script mismatches found for the audited columns.\n")
} else {
  cat("Package-vs-script mismatches:\n")
  cat(paste0("- ", mismatches, collapse = "\n"))
  cat("\n")
}
