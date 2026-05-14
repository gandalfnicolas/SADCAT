#' @title Wordnet antonym retriever
#'
#' @description Gets antonyms for nouns or adjectives of a term's synset.
#' @param synsets synsets to obtain antonyms for. May be a single synset or a
#'   list of synsets.
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the
#'   various words that make up the synsets.
#' @return antonym expansion
#' @export get_Antonyms


get_Antonyms = function(synsets, Syns = TRUE){
  tryCatch({
    if (length(synsets) == 0L) {
      return(if (isFALSE(Syns)) character(0) else list())
    }
    syn_list <- if (is.list(synsets)) synsets else list(synsets)
    antonyms <- unlist(lapply(syn_list, function(s) {
      tryCatch(wordnet::getRelatedSynsets(s, pointerSymbol = "!"),
               error = function(e) list())
    }))
    if (isFALSE(Syns)) {
      return(unlist(lapply(antonyms, wordnet::getWord)))
    }
    antonyms
  },
  error = function(e) {
    message("ERROR in get_Antonyms: ", conditionMessage(e))
    NA
  })
}
