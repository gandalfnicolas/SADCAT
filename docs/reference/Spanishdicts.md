# Full spanish dictionaries

Full spanish dictionaries. Word embedding values are based on english
data.

## Usage

``` r
Spanishdicts
```

## Format

A data frame. Key column groups:

- `Palabra`: Spanish word, not stemmed but some preprocessing (no
  symbols, spaces, accents)

- `Palabra_stem`: Stemmed version of Palabra

- `values1`-`values3`: Words at different preprocessing stages

- Columns ending in `_dict`: Binary (0/1) dictionary membership. With
  `_lo`/`_hi` suffixes for low/high variants

- Columns ending in `_dir`: Direction coding (1=high, 0=neutral,
  -1=low). NA if word not in dictionary

- `fasttext.*`: Fasttext word embedding dimensions (Common Crawl)

- `Glove.*`: GloVe word embedding dimensions (Common Crawl 840B)

- `Word2vec.*`: Word2vec embedding dimensions (Google News)
