#' @title Singularize a Word
#'
#' @description Singularize a word, respecting SADCAT dictionary.
#' Converts plural forms to singular. Skips words already in the SADCAT
#' dictionary and words ending in "es" (to avoid incorrect singularization).
#' Handles irregular plurals via switch cases.
#' @param word A single character string to singularize
#' @param dictionary Logical. If TRUE, check result against SemNetDictionaries
#' @return Singularized version of the word
#' @export singularize2

singularize2 <- function(word, dictionary = TRUE) {
  if (word %in% All.steps_Dictionaries$tv) {
    return(word)
  }
  if (is.na(word)) {
    return(word)
  }
  if (stringr::str_detect(word, 'es$')) {
    return(word)
  }
  spl <- unlist(strsplit(word, " "))
  if (length(spl) > 1) {
    word <- spl[length(spl)]
    multiple <- TRUE
  } else {
    multiple <- FALSE
  }
  orig.word <- word

  if (!requireNamespace("SemNetDictionaries", quietly = TRUE)) {
    stop("Package 'SemNetDictionaries' is required for singularize2(). Install with: install.packages('SemNetDictionaries')")
  }
  checker <- SemNetDictionaries::general.dictionary

  chn <- FALSE
  word <- switch(word,
    children = "child", geese = "goose", men = "man", women = "woman",
    teeth = "tooth", feet = "foot", mice = "mouse", people = "person",
    lice = "louse", valves = "valve", trees = "tree", scribbles = "scribble",
    peduncles = "peduncle", mallees = "mallee", panicles = "panicle",
    ridges = "ridge", petioles = "petiole", angles = "angle",
    bristles = "bristle", edges = "edge", fissures = "fissure",
    sutures = "suture", occurrences = "occurrence", bees = "bee"
  )
  if (is.null(word)) {
    word <- orig.word
  } else {
    chn <- TRUE
  }
  last.lets <- substr(word, nchar(word) - 2, nchar(word))
  if (!chn) {
    if (any(last.lets == c("ves", "ies", "zes", "ses"))) {
      if (last.lets == "ves") {
        word <- substr(word, 1, nchar(word) - 3)
        word <- paste0(word, "f")
        if (!word %in% checker) word <- paste0(word, "e")
        chn <- TRUE
      } else if (last.lets == "ies") {
        word <- substr(word, 1, nchar(word) - 3)
        word <- paste0(word, "y")
        if (!word %in% checker) {
          word <- substr(word, 1, nchar(word) - 1)
          word <- paste0(word, "ie")
        }
        chn <- TRUE
      } else if (any(last.lets == c("zes", "ses"))) {
        word <- substr(word, 1, nchar(word) - 3)
        chn <- TRUE
        if (!word %in% checker) {
          word <- orig.word
          chn <- FALSE
        }
      }
    }
  }
  last.lets <- substr(word, nchar(word) - 1, nchar(word))
  if (!chn) {
    if (last.lets == "es") {
      word <- substr(word, 1, nchar(word) - 2)
      if (!word %in% checker) word <- paste0(word, "is")
      chn <- TRUE
    }
  }
  last.lets <- substr(word, nchar(word), nchar(word))
  if (!chn) {
    if (any(last.lets == c("s", "i", "a"))) {
      if (last.lets == "s") {
        word <- substr(word, 1, nchar(word) - 1)
      } else if (last.lets == "i") {
        word <- substr(word, 1, nchar(word) - 1)
        word <- paste0(word, "us")
      } else if (last.lets == "a") {
        word <- substr(word, 1, nchar(word) - 1)
        word <- paste0(word, "on")
      }
    }
  }
  if (isTRUE(dictionary)) {
    if (!word %in% checker) {
      if (isTRUE(multiple)) orig.word <- paste(spl, collapse = " ")
      return(orig.word)
    } else {
      if (multiple) {
        spl[length(spl)] <- word
        word <- paste(spl, collapse = " ")
      }
      return(word)
    }
  } else {
    if (multiple) {
      spl[length(spl)] <- word
      word <- paste(spl, collapse = " ")
    }
    return(word)
  }
}
