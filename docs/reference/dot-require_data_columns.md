# Validate required columns before running a pipeline stage

Validate required columns before running a pipeline stage

## Usage

``` r
.require_data_columns(data, cols, context)
```

## Arguments

- data:

  A data.frame

- cols:

  Required column names

- context:

  Short stage/function label for the error message

## Value

Invisibly returns TRUE
