# Unprocessed Full dictionary codings shortened with Fasttext

Unprocessed Full dictionary codings, shortened with Fasttext, including
the vector codings. May be used for different degrees of preprocessing.
(e.g., use if text too extensive to preprocess)

## Usage

``` r
All.steps_Dictionaries_FT
```

## Format

A data frame with 11291 rows and 1533 variables. Key column groups:

- `values0`-`values3`: Words at different preprocessing stages

- Columns ending in `_dict`: Binary (0/1) dictionary membership. With
  `_lo`/`_hi` suffixes for low/high variants

- Columns ending in `_dir`: Direction coding (1=high, 0=neutral,
  -1=low). NA if word not in dictionary

- `fasttext.*`: Fasttext word embedding dimensions (Common Crawl)

- `Glove.*`: GloVe word embedding dimensions (Common Crawl 840B)

- `Word2vec.*`: Word2vec embedding dimensions (Google News)
