#' @title Code single-word responses into dictionaries and sentiment
#'
#' @description This function allows you to code a variable of text responses into different sentiment scores. If available, sentiments are matched to raw words, otherwise they are matched to preprocessed words.Average the different valence scores. If a SWN score is available for the correct sense, then average that with other valence scores, if not, then check if version one of the valence scores means is na, if so, get the average of the second versions, if not get the average of the first versions.
#' @param data dataframe including the text to code
#' @param raw_text string name of a variable (vector) of text that has not been preprocessed
#' @param preproc_text string name of a variable (vector) of text that has been preprocessed using Full_preprocess()
#' @return Original dataframe with additional columns for codings
#' @export Code_sent

Code_sent = function(data, raw_text = "rawword", preproc_text = "word"){
  SENT_dictv1 = Sentiments[,-c(8,9,10)]
  SENT_dictv2 = Sentiments %>%
    dplyr::select(word = word4,Val_bing2=Val_bing,Val_NRC2=Val_NRC,Val_afinn2=Val_afinn,Val_loughran2 =Val_loughran,Val_sentiwn2 =Val_sentiwn)%>%
    dplyr::group_by(word)%>%
    dplyr::summarize_all(funs(mean(.,na.rm=T)))
  data$tv = sapply(data[[raw_text]], tolower)
  data$tv = sapply(data$tv, trimws)
  data2 = merge(x = data, y =SENT_dictv1, by.x = "tv",by.y = "word", all.x = TRUE) #Find first if there is a match with original response - different lemmas (next code) might have different sentiment, so this is ideal
  data3 = merge(x = data2, y = SENT_dictv2, by.x = preproc_text,by.y = "word", all.x = TRUE)
  return(data3)
}
