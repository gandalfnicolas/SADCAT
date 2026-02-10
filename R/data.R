#' Unprocessed Full dictionary codings shortened with Fasttext
#'
#' Unprocessed Full dictionary codings, shortened with Fasttext, including the vector codings. May be used for different degrees of preprocessing. (e.g., use if text too extensive to preprocess)
#'
#' @format A data frame with 11291 rows and 1539 variables. Key column groups:
#' \itemize{
#'   \item \code{values}, \code{values0}-\code{values3}: Words at different preprocessing stages
#'   \item Columns ending in \code{_dict}: Binary (0/1) dictionary membership. With \code{_lo}/\code{_hi} suffixes for low/high variants
#'   \item Columns ending in \code{_dir}: Direction coding (1=high, 0=neutral, -1=low). NA if word not in dictionary
#'   \item \code{fasttext.*}: Fasttext word embedding dimensions (Common Crawl)
#'   \item \code{Glove.*}: GloVe word embedding dimensions (Common Crawl 840B)
#'   \item \code{Word2vec.*}: Word2vec embedding dimensions (Google News)
#' }
"All.steps_Dictionaries_FT"


#' Unprocessed Full dictionary codings
#'
#' Unprocessed Full dictionary codings, including the vector codings. May be used for different degrees of preprocessing. (e.g., use if text too extensive to preprocess)
#'
#' @format A data frame with 14449 rows. Key column groups:
#' \itemize{
#'   \item \code{values}, \code{values0}-\code{values3}: Words at different preprocessing stages
#'   \item Columns ending in \code{_dict}: Binary (0/1) dictionary membership. With \code{_lo}/\code{_hi} suffixes for low/high variants
#'   \item Columns ending in \code{_dir}: Direction coding (1=high, 0=neutral, -1=low). NA if word not in dictionary
#'   \item \code{fasttext.*}: Fasttext word embedding dimensions (Common Crawl)
#'   \item \code{Glove.*}: GloVe word embedding dimensions (Common Crawl 840B)
#'   \item \code{Word2vec.*}: Word2vec embedding dimensions (Google News)
#'   \item \code{USE.*}: Universal Sentence Encoder embedding dimensions
#' }
"All.steps_Dictionaries"


#' Full dictionary codings shortened with fasttext
#'
#' Full dictionary codings, shortened with fasttext, including the vector codings
#'
#' @format A data frame with 13930 rows. Key column groups:
#' \itemize{
#'   \item \code{word}: A word in one or more of the dictionaries
#'   \item Columns ending in \code{_dict}: Binary (0/1) dictionary membership. With \code{_lo}/\code{_hi} suffixes for low/high variants
#'   \item Columns ending in \code{_dir}: Direction coding (1=high, 0=neutral, -1=low). NA if word not in dictionary
#'   \item \code{fasttext.*}: Fasttext word embedding dimensions (Common Crawl)
#'   \item \code{Glove.*}: GloVe word embedding dimensions (Common Crawl 840B)
#'   \item \code{Word2vec.*}: Word2vec embedding dimensions (Google News)
#'   \item \code{USE.*}: Universal Sentence Encoder embedding dimensions
#' }
"Dictionaries_FT"


#' Full dictionary codings
#'
#' Full dictionary codings, including the vector codings
#'
#' @format A data frame with 13930 rows. Key column groups:
#' \itemize{
#'   \item \code{word}: A word in one or more of the dictionaries
#'   \item Columns ending in \code{_dict}: Binary (0/1) dictionary membership. With \code{_lo}/\code{_hi} suffixes for low/high variants
#'   \item Columns ending in \code{_dir}: Direction coding (1=high, 0=neutral, -1=low). NA if word not in dictionary
#'   \item \code{fasttext.*}: Fasttext word embedding dimensions (Common Crawl)
#'   \item \code{Glove.*}: GloVe word embedding dimensions (Common Crawl 840B)
#'   \item \code{Word2vec.*}: Word2vec embedding dimensions (Google News)
#'   \item \code{USE.*}: Universal Sentence Encoder embedding dimensions
#' }
"Dictionaries"


#' Average Word embeddings for expanded dictionaries
#'
#' Average word embeddings for expanded dictionaries
#'
#' @format A data frame with 56 rows. Key column groups:
#' \itemize{
#'   \item \code{Dictionary}: Name of the dictionary the embeddings represent
#'   \item \code{ft.*}: Fasttext word embedding dimensions (Common Crawl)
#'   \item \code{Glove.*}: GloVe word embedding dimensions (Common Crawl 840B)
#'   \item \code{w2v.*}: Word2vec embedding dimensions (Google News)
#'   \item \code{USE.*}: Universal Sentence Encoder embedding dimensions
#' }
"Full_Vectors_Avg"


#' Average Word embeddings for seed dictionaries
#'
#' Average Word embeddings for seed dictionaries
#'
#' @format A data frame with seed vectors. Key column groups:
#' \itemize{
#'   \item \code{Dictionary}: Name of the dictionary the embeddings represent
#'   \item \code{ft.*}: Fasttext word embedding dimensions
#'   \item \code{Glove.*}: GloVe word embedding dimensions
#'   \item \code{w2v.*}: Word2vec embedding dimensions
#'   \item \code{USE.*}: Universal Sentence Encoder embedding dimensions
#'   \item \code{SBERT.*}: SBERT embedding dimensions (if available)
#' }
"Seed_Vectors_Avg"


#' Full spanish dictionaries
#'
#' Full spanish dictionaries. Word embedding values are based on english data.
#'
#' @format A data frame. Key column groups:
#' \itemize{
#'   \item \code{Palabra}: Spanish word, not stemmed but some preprocessing (no symbols, spaces, accents)
#'   \item \code{Palabra_stem}: Stemmed version of Palabra
#'   \item \code{values}, \code{values0}-\code{values3}: Words at different preprocessing stages
#'   \item Columns ending in \code{_dict}: Binary (0/1) dictionary membership. With \code{_lo}/\code{_hi} suffixes for low/high variants
#'   \item Columns ending in \code{_dir}: Direction coding (1=high, 0=neutral, -1=low). NA if word not in dictionary
#'   \item \code{fasttext.*}: Fasttext word embedding dimensions (Common Crawl)
#'   \item \code{Glove.*}: GloVe word embedding dimensions (Common Crawl 840B)
#'   \item \code{Word2vec.*}: Word2vec embedding dimensions (Google News)
#' }
"Spanishdicts"


#' Example obituary data for text coding
#'
#' Example obituary data for use with the text coding pipeline
#'
#' @format A data frame with obituary text responses
"Obituary_data"


#' Average word embeddings for seed dictionaries (all words)
#'
#' Average word embeddings for seed dictionaries using all words
#'
#' @format A data frame with seed vectors computed from all words
"Seed_Vectors_allwords_Avg"


#' Average word embeddings for seed dictionaries (person context)
#'
#' Average word embeddings for seed dictionaries using person context words
#'
#' @format A data frame with seed vectors computed from person context words
"Seed_Vectors_personcontext_Avg"


#' Sentiment lexicon data
#'
#' Combined sentiment lexicon data used for valence scoring
#'
#' @format A data frame with sentiment words and their valence scores
"Sentiments"
