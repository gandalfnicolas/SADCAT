# Run the Full SADCAT Pipeline

Run the full SADCAT LLM response processing pipeline. Chains all
pipeline stages in sequence: text preprocessing, valence scoring,
dictionary matching, embedding computation, seed similarity computation,
and aggregation. Each stage can be skipped via the `stages` parameter.

## Usage

``` r
process_responses(
  data,
  text_col = "responsex",
  response_col = NULL,
  group_cols = c("Synonym.GroupX", "Group", "Level"),
  stages = c("preprocess", "valence", "dictionaries", "socats", "embeddings", "seeds",
    "aggregate"),
  spellcheck = TRUE,
  spellcheck_method = "hunspell",
  singularize_text = TRUE,
  java_home = NULL,
  wordnet_dict = NULL,
  gemini_spellcheck_key = NULL,
  gemini_spellcheck_model = "gemini-2.0-flash",
  gemini_spellcheck_context = NULL,
  valence_text_col = "tv",
  embedding_methods = c("sbert"),
  sbert_model = "paraphrase-mpnet-base-v2",
  gemini_api_key = NULL,
  gemini_dims = 2000L,
  gemini_batch_size = 10L,
  gemini_sleep = 63,
  seed_method = "correlation",
  extra_sum_cols = NULL,
  extra_mean_cols = NULL,
  extra_distinct_cols = NULL,
  save_intermediates = FALSE,
  save_prefix = "pipeline",
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame with raw LLM responses

- text_col:

  Column with raw text responses (default "responsex")

- response_col:

  Column with original response for NA-gating (default: same as
  text_col). Only needed if your NA-indicator column differs from
  text_col.

- group_cols:

  Grouping columns for aggregation (default c("Synonym.GroupX", "Group",
  "Level"))

- stages:

  Character vector of stages to run. Default: all stages. Options:
  "preprocess", "valence", "dictionaries", "socats", "embeddings",
  "seeds", "aggregate"

- spellcheck:

  Run spell-checking in preprocessing? (default TRUE)

- spellcheck_method:

  Method for spell-checking: "hunspell" (default) or "gemini"
  (LLM-based, requires API key). See
  [`preprocess_text`](https://gandalfnicolas.github.io/SADCAT/reference/preprocess_text.md).

- singularize_text:

  Run singularization in preprocessing? (default TRUE)

- java_home:

  Optional Java home path for spell-checking. If NULL or not found,
  auto-detects from environment, PATH, and common install locations.

- wordnet_dict:

  Optional WordNet dictionary path. If NULL or not found, auto-detects
  from environment and common install locations.

- gemini_spellcheck_key:

  Gemini API key for LLM spell-checking (default NULL).

- gemini_spellcheck_model:

  Gemini model for spell-checking (default "gemini-2.0-flash").

- gemini_spellcheck_context:

  Custom system prompt for Gemini spell-checking (default NULL).

- valence_text_col:

  Column to use for valence scoring (default "tv"). Use "tv" for LLMs
  (preserve original), "tv2" for human participants.

- embedding_methods:

  Embedding methods: "sbert", "gemini", or both (default "sbert")

- sbert_model:

  SBERT model name (default "paraphrase-mpnet-base-v2")

- gemini_api_key:

  Gemini API key. If NULL, reads from environment variable

- gemini_dims:

  Gemini embedding dimensionality (default 2000)

- gemini_batch_size:

  Gemini batch size (default 10)

- gemini_sleep:

  Seconds between Gemini batches (default 63)

- seed_method:

  Seed similarity method: "correlation" or "cosine" (default
  "correlation")

- extra_sum_cols:

  Extra columns for sum aggregation

- extra_mean_cols:

  Extra columns for mean aggregation

- extra_distinct_cols:

  Extra columns for distinct aggregation

- save_intermediates:

  Save CSVs after each stage? (default FALSE)

- save_prefix:

  Prefix for intermediate CSV filenames (default "pipeline")

- verbose:

  Print progress? (default TRUE)

## Value

A named list with `$long` (full long-format data) and `$agg` (aggregated
data, if "aggregate" stage was run)

## Details

**Stage-by-stage usage.** Each stage can be called as a standalone
exported function. This is useful for inspecting intermediate results,
customizing parameters per stage, or integrating with external
processing. The functions must be called in order because each stage
depends on columns produced by earlier stages:


    # Stage 1: Preprocess -> produces tv, tv2, tv3
    dat <- preprocess_text(dat, text_col = "responsex", spellcheck = FALSE)

    # Stage 2: Valence -> produces Val_*, ValenceYesNA, ValenceNoNA
    dat <- score_valence(dat, text_col = "tv")

    # Stage 3: SADCAT dictionaries -> produces compact outputs such as
    # {Dim}_prevalence, {Dim}_Valence, {Dim}_valenceStrictNA, {Dim}_valenceNoNA,
    # {Dim}_direction, and NoMatch
    dat <- match_dictionaries(dat, text_col = "tv3",
                              valence_col = "ValenceYesNA", valence_nona_col = "ValenceNoNA")

    # Stage 3b (optional): SOCATS social category dictionaries
    dat <- match_dictionaries(dat, text_col = "tv3",
                              valence_col = "ValenceYesNA", valence_nona_col = "ValenceNoNA",
                              sadcat_dict = FALSE, socats = TRUE)

    # Stage 4 (optional): Sentence embeddings (requires Python via reticulate)
    dat <- compute_embeddings(dat, text_col = "tv", methods = "sbert")

    # Stage 5 (optional): Seed similarities (requires Stage 4)
    dat <- compute_seed_similarities(dat, embedding_prefix = "SBERT")

    # Stage 6: Aggregate to group level
    agg <- aggregate_responses(dat,
              group_cols = c("Synonym.GroupX", "Group", "Level"))

**Valence columns.**

- `ValenceYesNA`: Mean of negation-aware scores across 5 sentiment
  dictionaries. NA when no dictionary matched any sentiment word.
  Negation is applied once to this combined score, not to individual
  dictionary scores.

- `ValenceNoNA`: Same as `ValenceYesNA`, but 0 instead of NA.

- `Val_lexicoder`, `Val_NRC`, etc.: Per-dictionary raw presence scores
  (-1, 0, or 1) without negation flipping.

- `{Dim}_prevalence`: 1/0 dimension prevalence, NA when response is
  missing.

- `{Dim}_Valence`: Default per-dimension valence. NA when the dimension
  is not tagged; otherwise `ValenceNoNA` (0 for sentiment-less tagged
  responses, signed value otherwise). Best for `mean(., na.rm=TRUE)`.

- `{Dim}_valenceStrictNA`: NA when the dimension is not tagged OR
  `ValenceYesNA` is NA. Strictly NA-gated on both axes.

- `{Dim}_valenceNoNA`: 0 when the dimension is not tagged or no
  sentiment matched; NA only when response is missing.

- `{Dim}_direction`: Direction score for directional dimensions (NA when
  not applicable).

- `{Dim}_directionNoNA`: Same as `{Dim}_direction`, but 0 when dimension
  is absent (NA when response missing).

- `NoMatch`: 1 when no SADCAT dimension matched, 0 otherwise (NA when
  response missing).

## See also

[`preprocess_text`](https://gandalfnicolas.github.io/SADCAT/reference/preprocess_text.md),
[`score_valence`](https://gandalfnicolas.github.io/SADCAT/reference/score_valence.md),
[`match_dictionaries`](https://gandalfnicolas.github.io/SADCAT/reference/match_dictionaries.md),
[`compute_embeddings`](https://gandalfnicolas.github.io/SADCAT/reference/compute_embeddings.md),
[`compute_seed_similarities`](https://gandalfnicolas.github.io/SADCAT/reference/compute_seed_similarities.md),
[`aggregate_responses`](https://gandalfnicolas.github.io/SADCAT/reference/aggregate_responses.md)
