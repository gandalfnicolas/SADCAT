#' @title Clean na responses
#'
#' @description This function allows you to change responses that indicate lack of knowledge into NA.
#' @param word word to clean, if multiple use loop/apply/dplyr
#' @return responses changed to NA when indicating lack of knowledge
#' @export clean_naresponses


clean_naresponses = function(word){
  word <- as.character(word)
  normalized <- gsub("\t", "", trimws(word), fixed = TRUE)
  missing_like <- normalized %in% c(
    "",
    "NA",
    "#N/A",
    "n/a",
    "N/A",
    "na",
    "an",
    "no",
    "idk",
    "nas",
    "dont know",
    "don't know",
    "?"
  )
  word[missing_like] <- NA_character_
  word
}
