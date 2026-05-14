#' @title Wordnet adjective expansion
#'
#' @description Gets see also, similar, and attribute related synsets/words
#'   for adjectives.
#' @param synsets synsets to obtain the expansion for. May be a single synset
#'   or a list of synsets.
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the
#'   various words that make up the synsets.
#' @return adjective expansion
#' @export get_Adj_Expansion


get_Adj_Expansion = function(synsets, Syns = TRUE){
  tryCatch({
    if (length(synsets) == 0L) {
      return(if (isFALSE(Syns)) character(0) else list())
    }
    syn_list <- if (is.list(synsets)) synsets else list(synsets)
    collect <- function(sym) {
      unlist(lapply(syn_list, function(s) {
        tryCatch(wordnet::getRelatedSynsets(s, pointerSymbol = sym),
                 error = function(e) list())
      }))
    }
    seealso   <- collect("^")
    similar   <- collect("&")
    attribute <- collect("\\=")
    Expansion <- c(seealso, similar, attribute)
    if (isFALSE(Syns)) {
      return(unlist(lapply(Expansion, wordnet::getWord)))
    }
    Expansion
  },
  error = function(e) {
    message("ERROR in get_Adj_Expansion: ", conditionMessage(e))
    NA
  })
}
