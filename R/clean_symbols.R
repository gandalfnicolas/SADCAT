#' @title Remove symbols
#'
#' @description This function allows you to remove symbols from responses.
#' @param word word to clean, if multiple use loop/apply/dplyr
#' @return word vector with no spaces or symbols
#' @export clean_symbols


clean_symbols = function(word){
  cleaned = gsub(" ","",word,fixed = T)
  cleaned = gsub("-","",cleaned,fixed = T)
  cleaned = gsub("'","",cleaned,fixed = T)
  cleaned = gsub('"',"",cleaned,fixed = T)
  cleaned = gsub(',',"",cleaned,fixed = T)
  cleaned = gsub('/',"",cleaned,fixed = T)
  cleaned = gsub('#',"",cleaned,fixed = T)
  cleaned = gsub('?',"",cleaned,fixed = T)
  cleaned = gsub('!',"",cleaned,fixed = T)
  cleaned = gsub('.',"",cleaned,fixed = T)
  cleaned = gsub('%',"",cleaned,fixed = T)
  cleaned = gsub('$',"",cleaned,fixed = T)
  cleaned = gsub(';',"",cleaned,fixed = T)
  cleaned = gsub(':',"",cleaned,fixed = T)
  cleaned = gsub('(',"",cleaned,fixed = T)
  cleaned = gsub(')',"",cleaned,fixed = T)
  cleaned = gsub('&',"",cleaned,fixed = T)
  cleaned = gsub('`',"",cleaned,fixed = T)
  return(cleaned)
}
