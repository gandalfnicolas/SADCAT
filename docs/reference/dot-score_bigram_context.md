# Score suggestion candidates by bigram co-occurrence with neighbors

Score suggestion candidates by bigram co-occurrence with neighbors

## Usage

``` r
.score_bigram_context(candidates, left_word, right_word, bigram_freqs)
```

## Arguments

- candidates:

  Character vector of candidate words

- left_word:

  Word to the left, or NULL

- right_word:

  Word to the right, or NULL

- bigram_freqs:

  Environment from .build_bigram_freqs()

## Value

Integer vector of scores (higher = better context fit)
