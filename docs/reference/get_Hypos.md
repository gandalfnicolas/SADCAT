# Wordnet hyponym retriever

Gets the synsets/words (depending on `Syns`) of a term's hyponym
hierarchy. Expansion runs level by level, and synsets visited at earlier
levels are skipped on subsequent passes, so no synset is expanded twice.
This deduplication prevents the combinatorial blow-up that earlier
versions could exhibit when seed terms sat near the top of WordNet's
hyponym graph (e.g., general or abstract nouns whose subtrees contain
many thousands of synsets reachable via multiple inheritance paths).

## Usage

``` r
get_Hypos(synsets, Syns = TRUE, max_depth = Inf, verbose = FALSE)
```

## Arguments

- synsets:

  synsets to obtain hyponyms for. May be a single synset or a list of
  synsets.

- Syns:

  Return the synsets? Defaults to TRUE. If FALSE, returns the various
  words that make up the synsets.

- max_depth:

  Maximum levels of hyponym recursion. Default `Inf` (expand until
  natural termination, still bounded by the visited-set deduplication).
  Set to a finite integer to cap worst-case runtime when working with
  very general seed terms.

- verbose:

  If TRUE, prints one progress message per expansion level showing the
  depth and number of new hyponym synsets retrieved. Default FALSE.

## Value

hyponym hierarchy (list of synsets if `Syns=TRUE`, otherwise the words
drawn from those synsets)
