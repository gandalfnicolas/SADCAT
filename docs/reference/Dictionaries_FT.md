# Full dictionary codings shortened with fasttext

Full dictionary codings, shortened with fasttext, including the vector
codings

## Usage

``` r
Dictionaries_FT
```

## Format

A data frame with 13930 rows. Key column groups:

- `word`: A word in one or more of the dictionaries

- Columns ending in `_dict`: Binary (0/1) dictionary membership. With
  `_lo`/`_hi` suffixes for low/high variants

- Columns ending in `_dir`: Direction coding (1=high, 0=neutral,
  -1=low). NA if word not in dictionary

- `fasttext.*`: Fasttext word embedding dimensions (Common Crawl)

- `Glove.*`: GloVe word embedding dimensions (Common Crawl 840B)

- `Word2vec.*`: Word2vec embedding dimensions (Google News)

- `USE.*`: Universal Sentence Encoder embedding dimensions
