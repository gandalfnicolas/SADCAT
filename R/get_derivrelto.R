#' @title Wordnet derivationally related terms retriever
#'
#' @description Gets derivationally related forms for a term's synset
#' @param synsets synsets to obtain part meronyms for
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the various words that make up the synsets.
#' @return derivetally related forms of words
#' @export get_derivrelto


get_derivrelto = function(synsets, Syns=T){
  tryCatch({
    if(length(synsets)==1){
      result = wordnet::getRelatedSynsets(synsets, pointerSymbol = "+")}
    else{
      result = sapply(synsets,getRelatedSynsets, pointerSymbol = "+")}
    if(Syns == F){
      return(sapply(unlist(result),getWord))}
    else{
      return(unlist(result))}},
    error = function(s){
      message(paste0("ERROR in get_derivrelto"))
      return(NA)})
}
