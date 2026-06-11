# Score Text Valence

Score text responses using up to 8 sentiment dictionaries. Always
applies Lexicoder (LSD2015), NRC, Bing, AFINN, and Loughran. By default
also applies SentiWordNet, Warriner et al. (2013) affective norms, and
the Jockers/Rinker lexicon. The Bing/NRC/AFINN/Loughran/Lexicoder/
SentiWordNet dictionaries score each text as -1, 0, or +1
(presence-based). Warriner and Jockers are continuous-valued in \[-1,
1\] (mean of matched-word valences; Warriner's 1-9 ratings are linearly
rescaled, Jockers values clipped to \[-1, 1\]). All per-dictionary
scores are then averaged into the combined negation-aware `ValenceYesNA`
/ `ValenceNoNA`.

## Usage

``` r
score_valence(
  data,
  text_col = "tv",
  response_col = NULL,
  include_sentiwn = TRUE,
  sentiwn_threshold = 0.1,
  include_warriner = TRUE,
  include_jockers = TRUE
)
```

## Arguments

- data:

  A data.frame with text to score

- text_col:

  Column containing cleaned text for valence scoring (default "tv").
  Uses tv (not tv2/tv3) to preserve original response for LLMs with low
  spelling errors. Change to "tv2" or "tv3" for human participants (a
  singularized text column matches more dictionary entries when
  responses contain plural/inflected forms).

- response_col:

  Column used for NA-gating (default: same as text_col). Only needed if
  your NA-indicator column differs from text_col.

- include_sentiwn:

  Logical (default `TRUE`). If `TRUE`, also apply SentiWordNet
  (presence-based, thresholded at `sentiwn_threshold`) and add
  `Val_sentiwn` / `Val_sentiwnNA`.

- sentiwn_threshold:

  Numeric in \[0, 1\] (default 0.10). Minimum \|Val_sentiwn\| magnitude
  for a word to count as positive or negative.

- include_warriner:

  Logical (default `TRUE`). If `TRUE`, also apply Warriner et al. (2013)
  affective norms (continuous valence in \[-1, 1\] from 13,915 lemmas,
  rescaled from the original 1-9 Likert) and add `Val_warriner` /
  `Val_warrinerNA`.

- include_jockers:

  Logical (default `TRUE`). If `TRUE`, also apply the Jockers/Rinker
  lexicon (continuous valence clipped to \[-1, 1\] from 11,710 entries)
  and add `Val_jockers` / `Val_jockersNA`.

## Value

The input data with new columns. Always: Val_lexicoder, Val_NRC,
Val_bing, Val_affin, Val_loughran, their NA variants, ValenceYesNA, and
ValenceNoNA. Optionally also Val_sentiwn, Val_warriner, Val_jockers (and
their NA variants) per the `include_*` flags.
