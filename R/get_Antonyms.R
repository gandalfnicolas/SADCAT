#' @title Wordnet antonym retriever
#'
#' @description Gets antonyms for nouns or adjectives of a term's synset
#' @param synsets synsets to obtain antonyms for
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the various words that make up the synsets.
#' @return antonym expansion
#' @export get_Antonyms


get_Antonyms = function(synsets, Syns=T){
  tryCatch({
    if(length(synsets)==1){
      antonyms = wordnet::getRelatedSynsets(synsets, pointerSymbol = "!")}
    else{
      antonyms = sapply(synsets,getRelatedSynsets, pointerSymbol = "!")}
    if(Syns == F){
      return(sapply(unlist(antonyms),getWord))}
    else{
      return(unlist(antonyms))}},
    error = function(s){
      message(paste0("ERROR in get_Antonyms"))
      return(NA)})
}
