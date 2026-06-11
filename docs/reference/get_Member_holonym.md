# Wordnet member holonym retriever

Gets member holonyms for a term's synset.

## Usage

``` r
get_Member_holonym(synsets, Syns = TRUE)
```

## Arguments

- synsets:

  synsets to obtain member holonyms for. May be a single synset or a
  list of synsets.

- Syns:

  Return the synsets? Defaults to TRUE. If FALSE, returns the various
  words that make up the synsets.

## Value

member holonyms of the words
