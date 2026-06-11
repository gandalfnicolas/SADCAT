# Code single-word responses

This function allows you to code a variable of (fully preprocessed, see
"preprocess_words") text responses into the different dictionaries

## Usage

``` r
Code_single(data, text = "word", more2na = T)
```

## Arguments

- data:

  dataframe including the text to code

- text:

  string name of a variable (vector) of text to code into the
  dictionaries

- more2na:

  if text has more than 2 words, transform to na

## Value

Original dataframe with additional columns for codings
