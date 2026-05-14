#' Full Wordnet Expansion
#'
#' Gets see also, similar, attributes, hyponyms, antonyms, derivationally
#' related forms, to expand a list of seed words.
#'
#' @param datax dataframe with three columns, named: term, PoS, and sense.
#' @param antonym Obtain antonyms? Default is TRUE.
#' @param syns return synsets? Default is FALSE. IF FALSE, returns words.
#' @param max_hypo_depth Maximum hyponym recursion depth, passed through to
#'   \code{\link{get_Hypos}}. Default \code{Inf} (expand until natural
#'   termination, bounded by the visited-set deduplication inside
#'   \code{get_Hypos}). Set to a finite integer (e.g., 6) to cap worst-case
#'   runtime when expanding seed lists that may contain very general nouns.
#' @param verbose If TRUE, prints per-level progress from the hyponym
#'   expansion step. Default FALSE.
#' @export

Full_Expand = function(datax, antonym = TRUE, syns = FALSE,
                       max_hypo_depth = Inf, verbose = FALSE){
  WL = purrr::pmap(datax, get_Syns)
  if (isTRUE(antonym)) {
    Antonym_List = get_Antonyms(WL)
    WL = c(WL, Antonym_List)
  }
  derivrelto_List = get_derivrelto(WL)
  WL = c(WL, derivrelto_List)
  Adj_Expansion_List = get_Adj_Expansion(WL)
  Hypos_List = get_Hypos(WL, max_depth = max_hypo_depth, verbose = verbose)
  WL = c(WL, Adj_Expansion_List, Hypos_List)
  if (isTRUE(antonym)) {
    Hypos_Antonym_List = get_Antonyms(Hypos_List)
    WL = c(WL, Hypos_Antonym_List)
  }
  if (isFALSE(syns)) {
    WL = unique(unlist(sapply(WL, wordnet::getWord)))
  } else {
    WL = unique(unlist(WL))
  }
  return(WL)
}
