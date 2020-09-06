#' @title Spellchecker
#'
#' @description This function spellchecks a word, works well with mapply. It is specific to a process in which we have a list of uncleaned (raw) and cleaned (e.g., preprocessed with steps such as the lemmatizer or symbol removal). We also need a dictionary. The Spellchecker works this way: First, if the cleaned input word is in the dictionaries, it returns the word: we have found a word that can be coded into a dictionary and thus does not need further spellchecking. If the word is not in the dictionaries, and the uncleaned response has spaces or dashes, return the uncleaned response: the spellchecker can only correct single words. If word is NA, transform to the string "na" and return. If the cleaned word is correctly spelled, but not in the dictionaries, return cleaned word. If none of the above, then proceed to spellcheck. Use the 5 top suggestions provided by hunspell for the correct spelling. If there are no suggestions, return the incorrectly spelled preprocessed response. If there are suggestions, iterate over them. If the current iteration is the same as the uncleaned response, return it, preprocessed. If the current iteration is a word provided by another participant (rawlist parameter), return that word, preprocessed. The logic of this step is that a response provided by another participant is more likely to be the spelling meant for the current response, since it was given in a similar context. If none of the suggestions meet those requirements, return the first suggestion, which tends to be hunspells' best suggestion.
#' @param raw uncleaned word response to spellcheck, if multiple use loop/apply/dplyr
#' @param cleaned cleaned word response to spellcheck, if multiple use loop/apply/dplyr
#' @param dict_cleaned a list of dictionary words that have been preprocessed
#' @param rawlist a list of words that were provided by participants, from which uncleaned responses are being obtained
#' @return spellchecked words
#' @export Spellcheck

Spellcheck = function(raw, cleaned, dict_cleaned, rawlist){
  toreturn=raw
  changed = 0
  tryCatch({
    print(raw)
    if(cleaned %in% dict_cleaned){
      print(paste0("IN DICT:",cleaned))
      return(cleaned)}
    if(grepl(" ", raw)|grepl("-", raw)){
      print(paste0("Has Space or -:",raw))
      return(raw)
    }
    if(raw == 'NA'| is.na(raw)){ #Returns character na to code into appropriate dictionary (lack of knowledge), based on cleaned1.
      print(paste0("NA"))
      return('na')
    }
    filter <- wordnet::getTermFilter("ExactMatchFilter", raw, TRUE)
    if(is.null(wordnet::getIndexTerms("NOUN", 5, filter)) &
       is.null(wordnet::getIndexTerms("ADJECTIVE",	5, filter)) &
       is.null(wordnet::getIndexTerms("VERB", 5, filter))){
      suggestions = hunspell::hunspell_suggest(raw)
      for(s in suggestions[[1]][1:5]){
        if(is.na(s)) {
          break}
        else if(s == raw){
          toreturn = s
          changed = 1
          break}
        else if(s %in% rawlist){
          toreturn = s
          changed = 1
          break}}
      if(changed == 0) {
        toreturn = suggestions[[1]][1]}}
    else {
      print(paste0("CORRECT BUT NOT ON DICT:",cleaned))
      return(cleaned)}
    toreturn = clean_naresponses(toreturn)
    toreturn = trimws(toreturn)
    toreturn = tolower(toreturn)
    toreturn = clean_symbols(toreturn)
    toreturn = Lemmatize(toreturn)
    toreturn = delete_ending_Ss(toreturn)
    print(paste0("INCORRECT & NOT IN DICT:",toreturn))
    return(toreturn)},
    error = function(s){
      print(paste("MY_ERROR:  ",s))
      return('*******ERROR')})}
