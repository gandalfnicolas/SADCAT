# Spell-check a single word using edit distance and bigram context

Spell-check a single word using edit distance and bigram context

## Usage

``` r
.spellcheck_single_word(
  word,
  dict_tv,
  rawlist,
  left_word = NULL,
  right_word = NULL,
  bigram_freqs = NULL
)
```

## Arguments

- word:

  A single word

- dict_tv:

  Character vector of known dictionary words

- rawlist:

  Character vector of all raw values

- left_word:

  Word to the left (for bigram context), or NULL

- right_word:

  Word to the right (for bigram context), or NULL

- bigram_freqs:

  Bigram frequency environment, or NULL

## Value

Corrected word
