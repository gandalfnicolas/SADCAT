# Wordnet Synset retriever

Provides the Wordnet synset for a word.

## Usage

``` r
get_Syns(term, PoS = "NOUN", sense = NULL, Syns = T)
```

## Arguments

- term:

  word to obtain the synset for

- PoS:

  Part of speech of the word. Defaults to "NOUN". Other options are
  "VERB", and "ADJECTIVE"

- sense:

  sense of the term for which the synset is desired. Defaults to NULL.

- Syns:

  Return the synset? Defaults to TRUE. If FALSE, returns the various
  words that make up the synset.

## Value

wordnet synsets
