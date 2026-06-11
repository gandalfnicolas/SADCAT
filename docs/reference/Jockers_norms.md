# Jockers/Rinker sentiment lexicon

The Jockers/Rinker hand-curated sentiment lexicon (originally
distributed as
[`lexicon::hash_sentiment_jockers_rinker`](https://rdrr.io/pkg/lexicon/man/hash_sentiment_jockers_rinker.html),
MIT-licensed). Values are continuous in \[-1, 1\]. Of the 11,710 entries
in the upstream source, 13 fell outside \[-1, 1\] (e.g., `overly` = -2,
`could have` = -1.05, `too much` = -2). These are shifter / amplifier /
counterfactual coefficients used by sentimentr's sliding-window
aggregator to override neighboring sentiment, not independently
sentiment-bearing words; they have been dropped here to avoid
artificially biasing per-text means. Used by
`score_valence(include_jockers = TRUE)`.

## Usage

``` r
Jockers_norms
```

## Format

A data frame with 11,697 rows and 2 columns:

- `word`: lowercased term (character)

- `valence`: sentiment in \[-1, 1\] (numeric)

## Source

Originally compiled by Matthew Jockers for narrative/literary sentiment
analysis (`syuzhet` package); the variant used here is the
Rinker-extended version distributed via the `lexicon` R package. Note:
validated primarily on narrative/fiction text rather than
person-perception research specifically. See
`data-raw/build_warriner_jockers.R` for the build recipe.
