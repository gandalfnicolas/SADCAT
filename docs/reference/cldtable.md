# Compact Letter Display Table

Create compact letter display tables from model and emmeans

## Usage

``` r
cldtable(modelx, emmeans, round_digits = 3, stat_adjust = "Tukey")
```

## Arguments

- modelx:

  A fitted model object

- emmeans:

  An emmeans object

- round_digits:

  Number of digits to round to (default 3)

- stat_adjust:

  P-value adjustment method (default "Tukey")

## Value

Data frame with CLD annotations
