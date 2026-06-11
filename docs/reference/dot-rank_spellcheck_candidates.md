# Rank spell-check suggestions using SADCAT heuristics

Rank spell-check suggestions using SADCAT heuristics

## Usage

``` r
.rank_spellcheck_candidates(word, suggestions, dists, bg_scores, in_rawlist)
```

## Arguments

- word:

  Original misspelled word

- suggestions:

  Candidate corrections

- dists:

  Edit distances from \`word\` to each candidate

- bg_scores:

  Bigram context scores

- in_rawlist:

  Logical vector indicating candidate presence in rawlist

## Value

Integer indices of suggestions in best-to-worst order
