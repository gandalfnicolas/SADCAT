#' @title Preprocess Text Responses
#'
#' @description Preprocess text responses: lowercase, spell-check, singularize.
#' Takes a data frame with a raw text column and produces cleaned versions:
#' \code{tv} (lowercased), \code{tv2} (spell-checked), \code{tv3} (singularized).
#' @param data A data.frame containing text responses
#' @param text_col Name of the column with raw text (default "responsex")
#' @param spellcheck Logical. Run spell-checking? (default TRUE)
#' @param singularize Logical. Run singularization? (default TRUE)
#' @param java_home Path to Java JRE (needed for WordNet spell-checking)
#' @param wordnet_dict Path to WordNet dictionary files
#' @param verbose Print progress? (default TRUE)
#' @return The input data with columns \code{tv}, \code{tv2}, \code{tv3} added
#' @export preprocess_text

preprocess_text <- function(data,
                            text_col = "responsex",
                            spellcheck = TRUE,
                            singularize = TRUE,
                            java_home = "C:\\Program Files\\Java\\jre-1.8",
                            wordnet_dict = "C:\\dict",
                            verbose = TRUE) {
  if (verbose) message("--- Stage 1: Preprocessing text ---")

  # Step 1: Lowercase
  data$tv <- tolower(data[[text_col]])
  data$tv <- ifelse(data$tv == "", NA, data$tv)

  # Step 2: Spell-check on unique values
  if (spellcheck) {
    if (verbose) message("  Spell-checking (requires Java + WordNet)...")
    Sys.setenv(JAVA_HOME = java_home)
    wordnet::setDict(wordnet_dict)

    unique_vals <- unique(data$tv)
    result_sc <- vector("character", length(unique_vals))
    for (i in seq_along(unique_vals)) {
      if (verbose && i %% 500 == 0) message("    Spell-checked ", i, " / ", length(unique_vals))
      result_sc[i] <- Spellcheck2(raw = unique_vals[i], rawlist = unique_vals)
    }
    lookup_sc <- data.frame(tv = unique_vals, tv2 = result_sc, stringsAsFactors = FALSE)
    data <- dplyr::left_join(data, lookup_sc, by = "tv")
  } else {
    data$tv2 <- data$tv
  }

  # Step 3: Singularize on unique values of tv2
  if (singularize) {
    if (verbose) message("  Singularizing...")
    unique_vals2 <- unique(data$tv2)
    result_sg <- vector("character", length(unique_vals2))
    for (i in seq_along(unique_vals2)) {
      if (verbose && i %% 500 == 0) message("    Singularized ", i, " / ", length(unique_vals2))
      result_sg[i] <- singularize2(unique_vals2[i])
    }
    lookup_sg <- data.frame(tv2 = unique_vals2, tv3 = result_sg, stringsAsFactors = FALSE)
    data <- dplyr::left_join(data, lookup_sg, by = "tv2")
  } else {
    data$tv3 <- data$tv2
  }

  # Step 4: Replace errors with NA
  data$tv2 <- ifelse(data$tv2 == "*******ERROR", NA, data$tv2)
  data$tv3 <- ifelse(data$tv3 == "*******ERROR", NA, data$tv3)

  if (verbose) message("  Preprocessing complete. Columns added: tv, tv2, tv3")
  return(data)
}
