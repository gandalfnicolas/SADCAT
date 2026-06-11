# Full preprocess

This function applies all response preprocessing functions included in
the package. (includes lowering, trimming whitespace, eliminating spaces
and symbols, lemmatizing, and spellcheking; some of these only work on
single words)

## Usage

``` r
Full_preprocess(
  words,
  parallelize = T,
  print = T,
  debug = F,
  treetagger_path = NULL
)
```

## Arguments

- words:

  text to preprocess

- parallelize:

  use parallel processors?

- print:

  Whether to print progress. Defaults to TRUE

- debug:

  Whether to pass debug flag to treetag. Defaults to FALSE

- treetagger_path:

  Optional TreeTagger installation directory. If NULL, auto-discovery is
  used.

## Value

fully preprocessed words
