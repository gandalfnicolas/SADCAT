#' @title Wordnet member holonym retriever
#'
#' @description Gets member holonyms for a term's synset
#' @param synsets synsets to obtain member holonyms  for
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the various words that make up the synsets.
#' @return member holonyms of the words
#' @export get_Member_holonym


get_Member_holonym= function(synsets, Syns=T){
  tryCatch({
    if(length(synsets)==1){
      result = wordnet::getRelatedSynsets(synsets, pointerSymbol = "%m")}
    else{
      result = sapply(synsets,getRelatedSynsets, pointerSymbol = "%m")}
    if(Syns == F){
      return(sapply(unlist(result),getWord))}
    else{
      return(unlist(result))}},
    error = function(s){
      message(paste0("ERROR in get_Member_holonym"))
      return(NA)})
}
