#' @title Spell-check Text with SADCAT Dictionary
#'
#' @description Spell-check text using SADCAT dictionary, WordNet, and hunspell.
#' Checks if word is in dictionary, correctly spelled, or in WordNet.
#' If not, uses hunspell suggestions. Requires rJava, wordnet, hunspell.
#' @param raw A single character string to spell-check
#' @param dict_tv Character vector of known dictionary words
#' @param rawlist Character vector of all raw values (for matching suggestions)
#' @return Spell-checked version of the input
#' @export Spellcheck2

Spellcheck2 <- function(raw, dict_tv = All.steps_Dictionaries$tv, rawlist) {
  toreturn <- raw
  changed <- 0
  tryCatch({
    if (raw %in% dict_tv) {
      return(raw)
    }
    if (hunspell::hunspell_check(raw)) {
      return(raw)
    }
    if (grepl(" ", raw) | grepl("-", raw)) {
      return(raw)
    }
    if (raw == "NA" | is.na(raw)) {
      return("na")
    }
    filter <- wordnet::getTermFilter("ExactMatchFilter", raw, TRUE)
    if (is.null(wordnet::getIndexTerms("NOUN", 5, filter)) &
        is.null(wordnet::getIndexTerms("ADJECTIVE", 5, filter)) &
        is.null(wordnet::getIndexTerms("VERB", 5, filter))) {
      suggestions <- hunspell::hunspell_suggest(raw)
      for (s in suggestions[[1]][1:5]) {
        if (is.na(s)) {
          break
        } else if (s == raw) {
          toreturn <- s
          changed <- 1
          break
        } else if (s %in% rawlist) {
          toreturn <- s
          changed <- 1
          break
        }
      }
      if (changed == 0) {
        toreturn <- suggestions[[1]][1]
      }
    } else {
      return(raw)
    }
    toreturn <- clean_naresponses(toreturn)
    toreturn <- tolower(toreturn)
    return(toreturn)
  }, error = function(s) {
    return("*******ERROR")
  })
}
