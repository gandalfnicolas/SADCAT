# Apply a single sentiment dictionary and compute valence scores

Apply a single sentiment dictionary and compute valence scores

## Usage

``` r
apply_single_valence_dict(toksval, dict_obj, name, is_lexicoder = FALSE)
```

## Arguments

- toksval:

  quanteda tokens object

- dict_obj:

  quanteda dictionary object

- name:

  Dictionary name (for column naming)

- is_lexicoder:

  If TRUE, handle neg_positive and neg_negative sub-categories

## Value

Data frame with Val and ValNA columns for this dictionary
