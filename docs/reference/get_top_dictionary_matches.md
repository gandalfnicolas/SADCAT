# Get Top SADCAT Dictionary Matches

Inspect the most frequent words in `text_col` that match each SADCAT
dictionary dimension. Useful for quick sanity checks of what the
dictionary coding is capturing in your dataset.

## Usage

``` r
get_top_dictionary_matches(
  data,
  text_col = "tv3",
  top_n = 20L,
  sadcat_dict = NULL,
  dictionary_names = NULL,
  min_count = 1L,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame containing text responses.

- text_col:

  Column containing text to inspect (default `"tv3"`).

- top_n:

  Number of top words to return per dimension (default `20`).

- sadcat_dict:

  Optional SADCAT dictionary object from
  [`prepare_sadcat_dictionaries`](https://gandalfnicolas.github.io/SADCAT/reference/prepare_sadcat_dictionaries.md).
  If `NULL`, it is prepared automatically.

- dictionary_names:

  Optional vector of dictionary names to include (e.g.,
  `c("Warmth_dic", "Competence_dic")`). If `NULL`, uses base SADCAT
  dimensions (`*_dic`) only.

- min_count:

  Minimum token frequency to keep (default `1`).

- verbose:

  Print progress? (default `TRUE`).

## Value

A data.frame with columns:

- `Dictionary`: SADCAT dimension label (without `_dic`)

- `word`: Matched token

- `n`: Token frequency in the dataset

- `prop`: Proportion within that dictionary's matched tokens

Returns an empty data frame with these columns if no matches are found.
