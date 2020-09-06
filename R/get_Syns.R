#' @title Wordnet Synset retriever
#'
#' @description Provides the Wordnet synset for a word.
#' @param term word to obtain the synset for
#' @param PoS Part of speech of the word. Defaults to "NOUN". Other options are "VERB", and "ADJECTIVE"
#' @param sense sense of the term for which the synset is desired. Defaults to NULL.
#' @param Syns Return the synset? Defaults to TRUE. If FALSE, returns the various words that make up the synset.
#' @return wordnet synsets
#' @export get_Syns


get_Syns = function(term, PoS = "NOUN",sense = NULL, Syns=T){
  tryCatch({
    filter <- wordnet::getTermFilter("ExactMatchFilter", term, TRUE)
    terms <- wordnet::getIndexTerms(PoS, 5, filter)
    synsets <- wordnet::getSynsets(terms[[1]])
    if(!is.null(sense)){
      synsets = synsets[[sense]]
    }
    ifelse(Syns == F,
           return(sapply(synsets,wordnet::getWord)),
           return(synsets))},
    error = function(s){
      message(paste0("ERROR in get_Syns: '", term ,"', PoS: '", PoS, "', sense: '", sense, "' not found"))
      return(NA)})
}
