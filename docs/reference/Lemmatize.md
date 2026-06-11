# Lemmatizer

This function allows you lemmatize words using the treetag lemmatizer

## Usage

``` r
Lemmatize(word, print = T, debug = F, treetagger_path = NULL)
```

## Arguments

- word:

  word to lemmatize, if multiple use loop/apply/dplyr

- print:

  Whether to print word being lemmatized. Useful for long lists of
  words. Defaults to TRUE

- debug:

  Whether to pass debug flag to treetag. Defaults to FALSE

- treetagger_path:

  Optional TreeTagger installation directory. If NULL, tries environment
  variables, PATH, and common install locations across
  Windows/macOS/Linux.

## Value

lemmatized words
