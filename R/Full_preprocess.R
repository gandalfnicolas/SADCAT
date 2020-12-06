#' @title Full preprocess
#'
#' @description This function applies all response preprocessing functions included in the package. (includes lowering, trimming whitespace, eliminating spaces and symbols, lemmatizing, and spellcheking; some of these only work on single words)
#' @param words text to preprocess
#' @param parallelize use parallel processors?
#' @return fully preprocessed words
#' @export Full_preprocess


Full_preprocess = function(words, parallelize = T, print =T, debug=F){
  res = sapply(words,clean_naresponses)
  res = sapply(res,trimws) #removes whitespace
  res = sapply(res,tolower)
  res = sapply(res,clean_symbols)
  if(parallelize == T){
    print("parallelizing")
    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(koRpus.lang.en)})
    parallel::clusterExport(cl=cl, varlist=c("Lemmatize", "res"), envir = environment())
    res = parallel::parSapply(cl,res, Lemmatize)
    parallel::stopCluster(cl)}
  else{
    res = sapply(res,Lemmatize)
  }
  res = sapply(res,delete_ending_Ss)
  res = mapply(Spellcheck,raw = words, cleaned = res, MoreArgs = list(rawlist = words, dict_cleaned= SADCAT::Dictionaries$word))
  res = as.character(res)
  res = sapply(res,tolower)
  res = sapply(res,trimws) #removes whitespace
  res = sapply(res,tolower)
  res = sapply(res,clean_symbols)
  return(res)
}
