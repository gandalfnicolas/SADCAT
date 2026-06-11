# Spell-check Text with SADCAT Dictionary

Spell-check text using SADCAT dictionary, WordNet, and hunspell. Checks
if word is in dictionary, correctly spelled, or in WordNet. If not, uses
hunspell suggestions ranked by edit distance and bigram context. Handles
multi-word inputs by spell-checking each word individually. Requires
rJava, wordnet, hunspell.

## Usage

``` r
Spellcheck2(
  raw,
  dict_tv = All.steps_Dictionaries$tv,
  rawlist,
  bigram_freqs = NULL
)
```

## Arguments

- raw:

  A single character string to spell-check

- dict_tv:

  Character vector of known dictionary words

- rawlist:

  Character vector of all raw values (for matching suggestions)

- bigram_freqs:

  Bigram frequency environment from .build_bigram_freqs() (optional)

## Value

Spell-checked version of the input
