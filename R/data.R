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
#' Average seed embeddings for SADCAT dictionaries rebuilt from curated seed words.
#'
#' @format A data frame with seed vectors. Key column groups:
#' \itemize{
#'   \item \code{Dictionary}: Dictionary row label (base, \code{_hi}, \code{_lo}, or \code{_dir})
#'   \item \code{SBERT_1}...\code{SBERT_768}: SBERT embedding dimensions
#'   \item \code{Gemini_1}...\code{Gemini_768}: Gemini embedding dimensions
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
#' Average seed embeddings for SADCAT dictionaries using term-level seed words.
#'
#' @format A data frame with columns \code{Dictionary}, \code{SBERT_1}...\code{SBERT_768},
#'   and \code{Gemini_1}...\code{Gemini_768}.
"Seed_Vectors_allwords_Avg"


#' Average word embeddings for seed dictionaries (person context)
#'
#' Average seed embeddings for SADCAT dictionaries using person-context seed text.
#'
#' @format A data frame with columns \code{Dictionary}, \code{SBERT_1}...\code{SBERT_768},
#'   and \code{Gemini_1}...\code{Gemini_768}.
"Seed_Vectors_personcontext_Avg"


#' Sentiment lexicon data
#'
#' Combined sentiment lexicon data used for valence scoring
#'
#' @format A data frame with sentiment words and their valence scores
"Sentiments"


#' Warriner et al. (2013) affective valence norms
#'
#' Mean valence ratings for 13,905 English lemmas from Warriner, Kuperman, &
#' Brysbaert (2013), rescaled to the [-1, 1] range. The original 1-9 Likert
#' ratings (where 5 = neutral) are linearly transformed via
#' \code{(V.Mean.Sum - 5) / 4}, so 1 (very negative) maps to -1, 5 to 0, and
#' 9 (very positive) to +1. Used by \code{score_valence(include_warriner = TRUE)}.
#'
#' @source Warriner, A. B., Kuperman, V., & Brysbaert, M. (2013). Norms of
#'   valence, arousal, and dominance for 13,915 English lemmas. \emph{Behavior
#'   Research Methods, 45}, 1191-1207. \doi{10.3758/s13428-012-0314-x}.
#'   Supplementary material redistributed under the journal's supplementary
#'   terms; see \code{data-raw/build_warriner_jockers.R} for the build recipe.
#'
#' @format A data frame with 13,905 rows and 2 columns:
#' \itemize{
#'   \item \code{word}: lowercased lemma (character)
#'   \item \code{valence}: rescaled mean valence in [-1, 1] (numeric)
#' }
"Warriner_norms"


#' Jockers/Rinker sentiment lexicon
#'
#' The Jockers/Rinker hand-curated sentiment lexicon (originally distributed
#' as \code{lexicon::hash_sentiment_jockers_rinker}, MIT-licensed). Values
#' are continuous in [-1, 1]. Of the 11,710 entries in the upstream source,
#' 13 fell outside [-1, 1] (e.g., \code{overly} = -2, \code{could have} =
#' -1.05, \code{too much} = -2). These are shifter / amplifier / counterfactual
#' coefficients used by sentimentr's sliding-window aggregator to override
#' neighboring sentiment, not independently sentiment-bearing words; they
#' have been dropped here to avoid artificially biasing per-text means.
#' Used by \code{score_valence(include_jockers = TRUE)}.
#'
#' @source Originally compiled by Matthew Jockers for narrative/literary
#'   sentiment analysis (\code{syuzhet} package); the variant used here is
#'   the Rinker-extended version distributed via the \code{lexicon} R package.
#'   Note: validated primarily on narrative/fiction text rather than
#'   person-perception research specifically. See
#'   \code{data-raw/build_warriner_jockers.R} for the build recipe.
#'
#' @format A data frame with 11,697 rows and 2 columns:
#' \itemize{
#'   \item \code{word}: lowercased term (character)
#'   \item \code{valence}: sentiment in [-1, 1] (numeric)
#' }
"Jockers_norms"


#' Raw SOCATS social category dictionary
#'
#' Source data used by \code{prepare_socats_dictionaries()} to build the
#' quanteda dictionary for SOCATS social category matching.
#'
#' @format A data frame with SOCATS category labels and raw dictionary terms.
#' Key column groups:
#' \itemize{
#'   \item \code{word}: Raw social category term
#'   \item One binary indicator column per SOCATS category used by the matcher
#'   \item Category families spanning race/ethnicity, age, sexuality, gender, jobs, and geography
#' }
"SOCATS_Dictionaries"
