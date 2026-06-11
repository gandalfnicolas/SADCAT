# Mask selected columns to NA where response text is missing

Mask selected columns to NA where response text is missing

## Usage

``` r
.mask_missing_response_cols(data, response_col, cols_or_patterns)
```

## Arguments

- data:

  A data frame

- response_col:

  Column indicating whether the response is missing

- cols_or_patterns:

  Character vector of exact column names and/or regex patterns

## Value

Data frame with selected columns masked to NA for missing responses
