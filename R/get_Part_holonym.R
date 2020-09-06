#' @title Wordnet part holonym retriever
#'
#' @description Gets part holonyms for a term's synset
#' @param synsets synsets to obtain part holonyms for
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the various words that make up the synsets.
#' @return part holonyms of words
#' @export get_Part_holonym


get_Part_holonym = function(synsets, Syns=T){
  tryCatch({
    if(length(synsets)==1){
      result = wordnet::getRelatedSynsets(synsets, pointerSymbol = "%p")}
    else{
      result = sapply(synsets,getRelatedSynsets, pointerSymbol = "%p")}
    if(Syns == F){
      return(sapply(unlist(result),getWord))}
    else{
      return(unlist(result))}},
    error = function(s){
      message(paste0("ERROR in get_Part_holonym"))
      return(NA)})
}
