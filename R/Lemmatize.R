#' @title Lemmatizer
#'
#' @description This function allows you lemmatize words using the treetag lemmatizer
#' @param word word to lemmatize, if multiple use loop/apply/dplyr
#' @param print Whether to print word being lemmatized. Useful for long lists of words. Defaults to TRUE
#' @return lemmatized words
#' @export Lemmatize


Lemmatize = function(word, print =T, debug = F){
  if (print == T){
    print(word)}
  if(!is.na(word)){
    if(word == ""){
      return("")}}
    lemmax = koRpus::treetag(as.character(word), treetagger="manual", format="obj", debug = debug, TT.tknz=T, lang="en", TT.options=list(path="C:\\treetagger", preset="en"))
  if(lemmax@tokens[["lemma"]] == "<unknown>"){
    if (print == T){
    print(lemmax@tokens[["token"]])}
    return (lemmax@tokens[["token"]])}
  else{
    if (print == T){
    print(lemmax@tokens[["lemma"]])}
    return(lemmax@tokens[["lemma"]])
}}

