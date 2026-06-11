# Full Wordnet Expansion

Gets see also, similar, attributes, hyponyms, antonyms, derivationally
related forms, to expand a list of seed words.

## Usage

``` r
Full_Expand(
  datax,
  antonym = TRUE,
  syns = FALSE,
  max_hypo_depth = Inf,
  verbose = FALSE
)
```

## Arguments

- datax:

  dataframe with three columns, named: term, PoS, and sense.

- antonym:

  Obtain antonyms? Default is TRUE.

- syns:

  return synsets? Default is FALSE. IF FALSE, returns words.

- max_hypo_depth:

  Maximum hyponym recursion depth, passed through to
  [`get_Hypos`](https://gandalfnicolas.github.io/SADCAT/reference/get_Hypos.md).
  Default `Inf` (expand until natural termination, bounded by the
  visited-set deduplication inside `get_Hypos`). Set to a finite integer
  (e.g., 6) to cap worst-case runtime when expanding seed lists that may
  contain very general nouns.

- verbose:

  If TRUE, prints per-level progress from the hyponym expansion step.
  Default FALSE.
