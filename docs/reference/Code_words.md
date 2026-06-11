# Code single-word unprocessed responses

This function allows you to code a variable of unprocessed text
responses into the different dictionaries and sentiments

## Usage

``` r
Code_words(
  data,
  text = "word",
  more2na = T,
  parallelize = T,
  print = T,
  debug = F,
  treetagger_path = NULL
)
```

## Arguments

- data:

  dataframe including the text to code

- text:

  string name of a variable (vector) of text to code into the
  dictionaries

- more2na:

  if text has more than 2 words, transform to na

- parallelize:

  use parallel processors? Defaults to TRUE

- print:

  Whether to print progress. Defaults to TRUE

- debug:

  Whether to pass debug flag to treetag. Defaults to FALSE

- treetagger_path:

  Optional TreeTagger installation directory. If NULL, auto-discovery is
  used.

## Value

Original dataframe with additional columns for codings
