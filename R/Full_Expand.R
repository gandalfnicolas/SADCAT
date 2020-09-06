#' Full Wordnet Expansion
#'
#' Gets see also, similar, attributes, hyponyms, antonyms, derivationally related forms, to expand a list of seed words.
#' @param datax dataframe with three columns, named: term, PoS, and sense.
#' @param antonym Obtain antonyms? Default is TRUE.
#' @param syns return synsets? Default is FALSE. IF FALSE, returns words.
#' @export

Full_Expand = function(datax, antonym = T, syns = F){
  WL = purrr::pmap(datax,get_Syns)
  if(antonym == T) {
    Antonym_List = get_Antonyms(WL)
    WL = c(WL,Antonym_List)}
  derivrelto_List = get_derivrelto(WL)
  WL = c(WL,derivrelto_List)
  Adj_Expansion_List = get_Adj_Expansion(WL)
  Hypos_List = get_Hypos(WL)
  WL = c(WL,Adj_Expansion_List,Hypos_List)
  if(antonym == T) {
    Hypos_Antonym_List = get_Antonyms(Hypos_List)
    WL = c(WL,Hypos_Antonym_List)}
  if(syns == F){
    WL = unique(unlist(sapply(WL,wordnet::getWord)))}
  else {
    WL = unique(unlist(WL))}
  return(WL)
}
