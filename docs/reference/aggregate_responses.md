# Aggregate Responses

Aggregate response-level data to group-level. Performs three types of
aggregation (sum, mean, distinct) and joins them. Auto-detects standard
pipeline columns by pattern; user can add extra columns via parameters.

## Usage

``` r
aggregate_responses(
  data,
  group_cols = c("Synonym.GroupX", "Group", "Level"),
  sum_cols = NULL,
  mean_cols = NULL,
  distinct_cols = NULL,
  extra_sum_cols = NULL,
  extra_mean_cols = NULL,
  extra_distinct_cols = NULL,
  create_noNA = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  The full long-format data.frame from the pipeline

- group_cols:

  Character vector of grouping columns (default c("Synonym.GroupX",
  "Group", "Level"))

- sum_cols:

  Columns to sum-aggregate. If NULL, auto-detects prevalence and NoMatch
  columns (plus traditional)

- mean_cols:

  Columns to mean-aggregate. If NULL, auto-detects standard patterns

- distinct_cols:

  Columns to preserve by distinct. If NULL, auto-detects Warning columns

- extra_sum_cols:

  Additional columns to include in sum aggregation

- extra_mean_cols:

  Additional columns to include in mean aggregation

- extra_distinct_cols:

  Additional columns to include in distinct aggregation

- create_noNA:

  Deprecated. Ignored; no extra noNA columns are created.

- verbose:

  Print progress? (default TRUE)

## Value

A data.frame with one row per unique combination of group_cols
