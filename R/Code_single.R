#' @title Code single-word responses
#'
#' @description This function allows you to code a variable of (fully preprocessed, see "preprocess_words") text responses into the different dictionaries
#' @param data dataframe including the text to code
#' @param text string name of a variable (vector) of text to code into the dictionaries
#' @param more2na if text has more than 2 words, transform to na
#' @return Original dataframe with additional columns for codings
#' @export Code_single

Code_single = function(data, text = "word", more2na = T){
  res = merge(x = data, y = Dictionaries, by.x = text, by.y = "word", all.x =T)
  res= dplyr::mutate_at(res,vars(contains("_dict")),funs(ifelse(is.na(.),0,.)))
  Dicts_v3pre = unique(Dictionaries$word)
  res$NONE = as.numeric(!(as.matrix(res[[text]]) %in% as.matrix(Dicts_v3pre)))
  res$NONE2 = ifelse(stringr::str_count(res[[text]], "\\S+") > 2,NA,as.numeric(!(res[[text]]) %in% as.matrix(Dicts_v3pre)))
  if(more2na == F){
    return(res)}
  else{
    data.table::setDT(res)
    nm1 <- grep('_dict', names(res), value=TRUE)
    for(j in nm1){
      data.table::set(res, i=NULL, j=j, value= ifelse(stringr::str_count(res[[text]], "\\S+") > 2,NA, res[[j]]))}
    return(res)}
}
