#' @title Code single-word unprocessed responses
#'
#' @description This function allows you to code a variable of unprocessed text responses into the different dictionaries and sentiments
#' @param data dataframe including the text to code
#' @param text string name of a variable (vector) of text to code into the dictionaries
#' @param more2na if text has more than 2 words, transform to na
#' @return Original dataframe with additional columns for codings
#' @export Code_words

Code_words = function(data, text = "word", more2na = T, parallelize = T, print =T, debug=F){
  dataunique = unique(data[[text]])
  dataunique = data.frame(values = dataunique, stringsAsFactors = F)
  message("preprocessing...")
  dataunique$preproc_text = Full_preprocess(dataunique$values, parallelize = parallelize)
  message("coding...")
  data2 = merge(x = data, y = dataunique, by.x = text, by.y = "values", all.x =T)
  res = Code_single(data = data2, text = "preproc_text", more2na = more2na)
  res2 = Code_sent(data = res, raw_text = text, preproc_text = "preproc_text")
  res2$Val_SWN = res2$PosScore - res2$NegScore
  is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
  res2[is.nan(res2)] <- NA
  res2 = dplyr::mutate(res2,
                        Val = ifelse(!(is.na(Val_SWN)),
                                     rowMeans(dplyr::select(res2,Val_SWN,Val_bing,Val_NRC,Val_afinn,Val_loughran), na.rm = T),
                                     ifelse(is.na(rowMeans(dplyr::select(res2,Val_bing,Val_NRC,Val_afinn,Val_loughran,Val_sentiwn), na.rm = T)),
                                            rowMeans(dplyr::select(res2,Val_bing2,Val_NRC2,Val_afinn2,Val_loughran2,Val_sentiwn2), na.rm = T),
                                            rowMeans(dplyr::select(res2,Val_bing,Val_NRC,Val_afinn,Val_loughran,Val_sentiwn), na.rm = T))))
  res2[is.nan(res2)] <- NA
  return(res2)
}
