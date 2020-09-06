#' Unprocessed Full dictionary codings shortened with Fasttext
#'
#' Unprocessed Full dictionary codings, shortened with Fasttext, including the vector codings. May be used for different degrees of preprocessing. (e.g., use if text too extensive to preprocess)
#'
#'
#' @format A data frame with 11291 rows and 1539 variables:
#' \describe{
#'   \item{values}{Words as obtained from the literature or Wordnet. No preprocessing}
#'   \item{values0}{lower-case word values}
#'   \item{values1}{lower-case word values, no spaces or symbols}
#'   \item{values2}{lower-case word values, no spaces or symbols, lemmatized}
#'   \item{values3}{lower-case word values, no spaces or symbols, lemmatized, with no ending Ss (not real words. These are the values averaged over in the final dictionaries)}
#'   \item{_dict}{variables ending in _dict indicate if the word is (1) or not (0) in the dictionary. If accompanied by a _lo it is coding if the word is low & in the dictionary, and if accompanied by a _hi it is coding if the word is high & in the dictionary (i.e., it combines the _dict and _dir variables)}
#'   \item{_dir}{variables ending in _dir indicate if the word is high (1), neutral (0) or low (-1) in the dictionary; e.g., friendly is high for sociability; unfriendly is low. Coded as NA if word not in the corresponding dictionary}
#'   \item{fasttext}{variables starting in fasttext are the word embedding dimensions for Fasttext trained on 2 million word vectors trained with subword information on Common Crawl (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Glove}{variables starting in Glove are the word embedding dimensions for Glove trained on Common Crawl (840B tokens, 2.2M vocab, cased, 300d vectors; https://nlp.stanford.edu/projects/glove/) (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Word2vec}{variables starting in Word2vec are the word embedding dimensions for Word2vec trained Google News (https://code.google.com/archive/p/word2vec/)}
#'   ...
#' }
"All.steps_Dictionaries_FT"


#' Unprocessed Full dictionary codings
#'
#' Unprocessed Full dictionary codings, including the vector codings. May be used for different degrees of preprocessing. (e.g., use if text too extensive to preprocess)
#'
#'
#' @format A data frame with 14449 rows and 1499 variables:
#' \describe{
#'   \item{values}{Words as obtained from the literature or Wordnet. No preprocessing}
#'   \item{values0}{lower-case word values}
#'   \item{values1}{lower-case word values, no spaces or symbols}
#'   \item{values2}{lower-case word values, no spaces or symbols, lemmatized}
#'   \item{values3}{lower-case word values, no spaces or symbols, lemmatized, with no ending Ss (not real words. These are the values averaged over in the final dictionaries)}
#'   \item{_dict}{variables ending in _dict indicate if the word is (1) or not (0) in the dictionary. If accompanied by a _lo it is coding if the word is low & in the dictionary, and if accompanied by a _hi it is coding if the word is high & in the dictionary (i.e., it combines the _dict and _dir variables)}
#'   \item{_dir}{variables ending in _dir indicate if the word is high (1), neutral (0) or low (-1) in the dictionary; e.g., friendly is high for sociability; unfriendly is low. Coded as NA if word not in the corresponding dictionary}
#'   \item{fasttext}{variables starting in fasttext are the word embedding dimensions for Fasttext trained on 2 million word vectors trained with subword information on Common Crawl (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Glove}{variables starting in Glove are the word embedding dimensions for Glove trained on Common Crawl (840B tokens, 2.2M vocab, cased, 300d vectors; https://nlp.stanford.edu/projects/glove/) (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Word2vec}{variables starting in Word2vec are the word embedding dimensions for Word2vec trained Google News (https://code.google.com/archive/p/word2vec/)}
#'   \item{USE}{variables starting in W2v are the word embedding dimensions for Universal Sentence Encoder trained on Common Crawl (https://arxiv.org/abs/1803.11175)}
#'   ...
#' }
"All.steps_Dictionaries"


#' Full dictionary codings shortened with fasttext
#'
#' Full dictionary codings, shortened with fasttext, including the vector codings
#'
#'
#' @format A data frame with 13930 rows and 973 variables:
#' \describe{
#'   \item{word}{a word in one or more of the dictionaries}
#'   \item{_dict}{variables ending in _dict indicate if the word is (1) or not (0) in the dictionary. If accompanied by a _lo it is coding if the word is low & in the dictionary, and if accompanied by a _hi it is coding if the word is high & in the dictionary (i.e., it combines the _dict and _dir variables)}
#'   \item{_dir}{variables ending in _dir indicate if the word is high (1), neutral (0) or low (-1) in the dictionary; e.g., friendly is high for sociability; unfriendly is low. Coded as NA if word not in the corresponding dictionary}
#'   \item{fasttext}{variables starting in fasttext are the word embedding dimensions for Fasttext trained on 2 million word vectors trained with subword information on Common Crawl (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Glove}{variables starting in Glove are the word embedding dimensions for Glove trained on Common Crawl (840B tokens, 2.2M vocab, cased, 300d vectors; https://nlp.stanford.edu/projects/glove/) (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Word2vec}{variables starting in Word2vec are the word embedding dimensions for Word2vec trained Google News (https://code.google.com/archive/p/word2vec/)}
#'   \item{USE}{variables starting in W2v are the word embedding dimensions for Universal Sentence Encoder trained on Common Crawl (https://arxiv.org/abs/1803.11175)}
#'   ...
#' }
"Dictionaries_FT"


#' Full dictionary codings
#'
#' Full dictionary codings, including the vector codings
#'
#'
#' @format A data frame with 13930 rows and 973 variables:
#' \describe{
#'   \item{word}{a word in one or more of the dictionaries}
#'   \item{_dict}{variables ending in _dict indicate if the word is (1) or not (0) in the dictionary. If accompanied by a _lo it is coding if the word is low & in the dictionary, and if accompanied by a _hi it is coding if the word is high & in the dictionary (i.e., it combines the _dict and _dir variables)}
#'   \item{_dir}{variables ending in _dir indicate if the word is high (1), neutral (0) or low (-1) in the dictionary; e.g., friendly is high for sociability; unfriendly is low. Coded as NA if word not in the corresponding dictionary}
#'   \item{fasttext}{variables starting in fasttext are the word embedding dimensions for Fasttext trained on 2 million word vectors trained with subword information on Common Crawl (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Glove}{variables starting in Glove are the word embedding dimensions for Glove trained on Common Crawl (840B tokens, 2.2M vocab, cased, 300d vectors; https://nlp.stanford.edu/projects/glove/) (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Word2vec}{variables starting in Word2vec are the word embedding dimensions for Word2vec trained Google News (https://code.google.com/archive/p/word2vec/)}
#'   \item{USE}{variables starting in W2v are the word embedding dimensions for Universal Sentence Encoder trained on Common Crawl (https://arxiv.org/abs/1803.11175)}
#'   ...
#' }
"Dictionaries"


#' Average Word embeddings for expanded dictionaries
#'
#' Average word embeddings for expanded dictionaries
#'
#'
#' @format A data frame with 56 rows and 901 variables:
#' \describe{
#'   \item{Dictionary}{Name of the dictionary that the word embeddings are representing.}
#'   \item{fasttext}{variables starting in fasttext are the word embedding dimensions for Fasttext trained on 2 million word vectors trained with subword information on Common Crawl (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Glove}{variables starting in Glove are the word embedding dimensions for Glove trained on Common Crawl (840B tokens, 2.2M vocab, cased, 300d vectors; https://nlp.stanford.edu/projects/glove/) (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Word2vec}{variables starting in Word2vec are the word embedding dimensions for Word2vec trained Google News (https://code.google.com/archive/p/word2vec/)}
#'   \item{USE}{variables starting in W2v are the word embedding dimensions for Universal Sentence Encoder trained on Common Crawl (https://arxiv.org/abs/1803.11175)}
#'   ...
#' }
"Full_Vectors_Avg"


#' Average Word embeddings for seed dictionaries
#'
#' Average Word embeddings for seed dictionaries
#'
#'
#' @format A data frame with 30 rows and 901 variables:
#' \describe{
#'   \item{Dictionary}{Name of the dictionary that the word embeddings are representing.}
#'   \item{fasttext}{variables starting in fasttext are the word embedding dimensions for Fasttext trained on 2 million word vectors trained with subword information on Common Crawl (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Glove}{variables starting in Glove are the word embedding dimensions for Glove trained on Common Crawl (840B tokens, 2.2M vocab, cased, 300d vectors; https://nlp.stanford.edu/projects/glove/) (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{W2v}{variables starting in W2v are the word embedding dimensions for Word2vec trained Google News (https://code.google.com/archive/p/word2vec/)}
#'   \item{USE}{variables starting in W2v are the word embedding dimensions for Universal Sentence Encoder trained on Common Crawl (https://arxiv.org/abs/1803.11175)}
#'   ...
#' }
"Seed_Vectors_Avg"


#' Full spanish dictionaries
#'
#' Full spanish dictionaries. Word embedding values are based on english data.
#'
#'
#' @format A data frame:
#' \describe{
#'   \item{Palabra}{Spanish word, not stemmed but some preprocessing (e.g., no symbols, spaces, accents)}
#'   \item{Palabra_stem}{Stemmed version of Palabra}
#'   \item{values}{Words as obtained from the literature or Wordnet. No preprocessing}
#'   \item{values0}{lower-case word values}
#'   \item{values1}{lower-case word values, no spaces or symbols}
#'   \item{values2}{lower-case word values, no spaces or symbols, lemmatized}
#'   \item{values3}{lower-case word values, no spaces or symbols, lemmatized, with no ending Ss (not real words. These are the values averaged over in the final dictionaries)}
#'   \item{_dict}{variables ending in _dict indicate if the word is (1) or not (0) in the dictionary. If accompanied by a _lo it is coding if the word is low & in the dictionary, and if accompanied by a _hi it is coding if the word is high & in the dictionary (i.e., it combines the _dict and _dir variables)}
#'   \item{_dir}{variables ending in _dir indicate if the word is high (1), neutral (0) or low (-1) in the dictionary; e.g., friendly is high for sociability; unfriendly is low. Coded as NA if word not in the corresponding dictionary}
#'   \item{fasttext}{variables starting in fasttext are the word embedding dimensions for Fasttext trained on 2 million word vectors trained with subword information on Common Crawl (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Glove}{variables starting in Glove are the word embedding dimensions for Glove trained on Common Crawl (840B tokens, 2.2M vocab, cased, 300d vectors; https://nlp.stanford.edu/projects/glove/) (https://fasttext.cc/docs/en/english-vectors.html)}
#'   \item{Word2vec}{variables starting in Word2vec are the word embedding dimensions for Word2vec trained Google News (https://code.google.com/archive/p/word2vec/)}
#'   ...
#' }
"Spanishdicts"

