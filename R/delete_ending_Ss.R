#' @title Singular transform
#'
#' @description This function deletes the letter s from the input whenever it trails the word
#' @param word word to transform, if multiple use loop/apply/dplyr
#' @return word vector without the last trailing S
#' @export delete_ending_Ss


delete_ending_Ss = function(word){
  isS = stringi::stri_sub(word,-1,-1)
  return(ifelse(isS == "s" | isS == "S", stringi::stri_sub(word,1,-2),word))
}
