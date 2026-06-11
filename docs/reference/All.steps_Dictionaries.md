# Unprocessed Full dictionary codings

Unprocessed Full dictionary codings, including the vector codings. May
be used for different degrees of preprocessing. (e.g., use if text too
extensive to preprocess)

## Usage

``` r
All.steps_Dictionaries
```

## Format

A data frame with 15678 rows. Key column groups:

- `tv`: Lower-cased source word; the matching pipeline derives its keys
  from this column

- `values`: Word as obtained from the literature or WordNet
  (unprocessed)

- `values2`: Lemmatized form: lower-case, no spaces or symbols

- `values3`: `values2` with at most one trailing "s" stripped (not
  necessarily real words)

- Columns ending in `_dict`: Binary (0/1) dictionary membership. With
  `_lo`/`_hi` suffixes for low/high variants

- Columns ending in `_dir`: Direction coding (1=high, 0=neutral,
  -1=low). NA if word not in dictionary

- `fasttext.*`: Fasttext word embedding dimensions (Common Crawl)

- `Glove.*`: GloVe word embedding dimensions (Common Crawl 840B)

- `Word2vec.*`: Word2vec embedding dimensions (Google News)

- `USE.*`: Universal Sentence Encoder embedding dimensions
