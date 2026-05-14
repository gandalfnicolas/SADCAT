#' @title Wordnet part holonym retriever
#'
#' @description Gets part holonyms for a term's synset.
#' @param synsets synsets to obtain part holonyms for. May be a single synset
#'   or a list of synsets.
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the
#'   various words that make up the synsets.
#' @return part holonyms of words
#' @export get_Part_holonym


get_Part_holonym = function(synsets, Syns = TRUE){
  tryCatch({
    if (length(synsets) == 0L) {
      return(if (isFALSE(Syns)) character(0) else list())
    }
    syn_list <- if (is.list(synsets)) synsets else list(synsets)
    result <- unlist(lapply(syn_list, function(s) {
      tryCatch(wordnet::getRelatedSynsets(s, pointerSymbol = "%p"),
               error = function(e) list())
    }))
    if (isFALSE(Syns)) {
      return(unlist(lapply(result, wordnet::getWord)))
    }
    result
  },
  error = function(e) {
    message("ERROR in get_Part_holonym: ", conditionMessage(e))
    NA
  })
}
