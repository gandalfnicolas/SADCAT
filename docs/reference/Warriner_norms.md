# Warriner et al. (2013) affective valence norms

Mean valence ratings for 13,905 English lemmas from Warriner, Kuperman,
& Brysbaert (2013), rescaled to the \[-1, 1\] range. The original 1-9
Likert ratings (where 5 = neutral) are linearly transformed via
`(V.Mean.Sum - 5) / 4`, so 1 (very negative) maps to -1, 5 to 0, and 9
(very positive) to +1. Used by `score_valence(include_warriner = TRUE)`.

## Usage

``` r
Warriner_norms
```

## Format

A data frame with 13,905 rows and 2 columns:

- `word`: lowercased lemma (character)

- `valence`: rescaled mean valence in \[-1, 1\] (numeric)

## Source

Warriner, A. B., Kuperman, V., & Brysbaert, M. (2013). Norms of valence,
arousal, and dominance for 13,915 English lemmas. *Behavior Research
Methods, 45*, 1191-1207.
[doi:10.3758/s13428-012-0314-x](https://doi.org/10.3758/s13428-012-0314-x)
. Supplementary material redistributed under the journal's supplementary
terms; see `data-raw/build_warriner_jockers.R` for the build recipe.
