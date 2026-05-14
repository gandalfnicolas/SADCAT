#' @title Wordnet part meronyms retriever
#'
#' @description Gets part meronyms for a term's synset.
#' @param synsets synsets to obtain part meronyms for. May be a single synset
#'   or a list of synsets.
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the
#'   various words that make up the synsets.
#' @return part meronyms of the words
#' @export get_part_meronyms


get_part_meronyms = function(synsets, Syns = TRUE){
  tryCatch({
    if (length(synsets) == 0L) {
      return(if (isFALSE(Syns)) character(0) else list())
    }
    syn_list <- if (is.list(synsets)) synsets else list(synsets)
    result <- unlist(lapply(syn_list, function(s) {
      tryCatch(wordnet::getRelatedSynsets(s, pointerSymbol = "#p"),
               error = function(e) list())
    }))
    if (isFALSE(Syns)) {
      return(unlist(lapply(result, wordnet::getWord)))
    }
    result
  },
  error = function(e) {
    message("ERROR in get_part_meronyms: ", conditionMessage(e))
    NA
  })
}
