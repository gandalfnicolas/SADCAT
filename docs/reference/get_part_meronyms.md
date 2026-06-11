# Wordnet part meronyms retriever

Gets part meronyms for a term's synset.

## Usage

``` r
get_part_meronyms(synsets, Syns = TRUE)
```

## Arguments

- synsets:

  synsets to obtain part meronyms for. May be a single synset or a list
  of synsets.

- Syns:

  Return the synsets? Defaults to TRUE. If FALSE, returns the various
  words that make up the synsets.

## Value

part meronyms of the words
