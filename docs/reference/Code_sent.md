# Code single-word responses into dictionaries and sentiment

This function allows you to code a variable of text responses into
different sentiment scores. If available, sentiments are matched to raw
words, otherwise they are matched to preprocessed words.Average the
different valence scores. If a SWN score is available for the correct
sense, then average that with other valence scores, if not, then check
if version one of the valence scores means is na, if so, get the average
of the second versions, if not get the average of the first versions.

## Usage

``` r
Code_sent(data, raw_text = "rawword", preproc_text = "word")
```

## Arguments

- data:

  dataframe including the text to code

- raw_text:

  string name of a variable (vector) of text that has not been
  preprocessed

- preproc_text:

  string name of a variable (vector) of text that has been preprocessed
  using Full_preprocess()

## Value

Original dataframe with additional columns for codings
