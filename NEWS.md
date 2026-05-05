# SADCAT NEWS

## SADCAT 0.2.0 (development)

### New features

* **`score_valence()` adds three new dictionaries by default**: SentiWordNet,
  Warriner et al. (2013) affective norms, and the Jockers/Rinker lexicon are
  now applied alongside the existing five (Lexicoder, NRC, Bing, AFINN,
  Loughran), bringing the combined `ValenceYesNA` / `ValenceNoNA` averages
  to up to 8 dictionaries. New parameters:

  * `include_sentiwn` (default `TRUE`), `sentiwn_threshold` (default `0.10`)
    use the SentiWordNet column already bundled in `Sentiments$Val_sentiwn`,
    presence-based scoring at `-1 / 0 / +1`. The threshold filters
    marginal-magnitude entries (default 0.10 keeps about 32k of 38k).
  * `include_warriner` (default `TRUE`) uses Warriner et al. (2013) norms
    bundled in the new `Warriner_norms` dataset (13,905 lemmas; original 1-9
    Likert linearly rescaled to [-1, 1] via `(V.Mean.Sum - 5) / 4`).
    Continuous-valued: each text's `Val_warriner` is the mean of matched-word
    valences. Validated on common English vocabulary; widely used in
    person-perception, social cognition, and prejudice research.
  * `include_jockers` (default `TRUE`) uses the Jockers/Rinker lexicon
    bundled in the new `Jockers_norms` dataset (11,697 entries; continuous
    valence in [-1, 1]). 13 entries from the upstream `lexicon` source fell
    outside [-1, 1] (e.g., `overly` = -2, `could have` = -1.05, `too much` =
    -2) and were dropped because they are shifter / amplifier coefficients
    designed for sentimentr's sliding-window aggregator, not independently
    sentiment-bearing words. Validation context is narrative/literary text
    rather than person perception specifically; use with that caveat.

  Negation handling is preserved: a positive Warriner/Jockers word inside
  a negation scope contributes its negated value to the combined average.
  Set any `include_*` to `FALSE` to opt out of an individual dictionary;
  set all three to `FALSE` to recover the prior 5-dictionary output exactly.

  **Behavior change vs. 0.1.0**: existing callers of `score_valence()` will
  see `ValenceYesNA` / `ValenceNoNA` shift, additional `Val_*` columns,
  and increased coverage of previously-no-match responses (≈22% rescue at
  default thresholds on a 47.8k-row human-response corpus).

### Data

* New bundled datasets `Warriner_norms` and `Jockers_norms` (see
  `data-raw/build_warriner_jockers.R` for the build recipe; Warriner data
  is downloaded once from the journal's supplementary materials and
  redistributed under Springer's free supplementary terms; Jockers/Rinker
  is sourced from the MIT-licensed `lexicon` R package).

### Bug fixes

* **`singularize2()` now actually singularizes -es plurals**: a stray
  `if (stringr::str_detect(word, "es$")) return(word)` early-return made the
  irregular-plural switch (`valves` → `valve`, `ladies` → `lady`, `boxes` →
  `box`, etc.) and the `ves`/`ies`/`zes`/`ses`/`es` suffix block dead code.
  Removed the early-return so those branches run as intended; also reordered
  the opening guards so `is.na(word)` is checked before dictionary membership.
  This is a behavior fix (correctness): words ending in `-es` that were
  previously returned unchanged will now be singularized.

* **`singularize2()` `-zes` / `-ses` branch no longer over-strips**: the
  branch previously stripped three trailing characters unconditionally, so
  `freezes` → `free`, `noses` → `no`, `roses` → `ro`, etc. (results that
  happened to be valid dictionary entries with completely different meanings
  were silently accepted). It now tries candidate suffixes in order:
  strip `s` (`freezes` → `freeze`, `noses` → `nose`, `houses` → `house`),
  then strip `es` (`buses` → `bus`, `gases` → `gas`, `classes` → `class`),
  then strip `zes`/`ses` (`quizzes` → `quiz`), keeping the first that's
  in `SemNetDictionaries::general.dictionary`. Known remaining edge case:
  Latin `-is` plurals like `crises` still mis-singularize because
  `general.dictionary` contains spurious short fragments (e.g., `cris`)
  that beat `crisis`; fixing that requires dictionary cleanup, not logic
  changes.

* **`singularize2()` no longer mangles short inputs and singular `-ss`
  nouns**: three guards added.
  (1) Words shorter than 3 characters short-circuit and return unchanged
  (`"a"` no longer becomes `"on"`, `"i"` no longer becomes `"us"`, `"as"`
  no longer becomes `"a"`, `"is"` no longer becomes `"i"`).
  (2) The final `-s` stripping branch now skips words ending in `-ss`,
  preventing `mass` → `mas` and `pass` → `pas`.
  (3) After any stripping rule, a candidate shorter than 3 characters
  falls back to the original word, preventing `bus` → `bu` and
  `axes` → `ax`.

* **`Spellcheck2()` no longer mangles NA markers**: previously, hunspell
  would receive inputs like `"n/a"` and `"?"` and offer nonsense suggestions
  (`"n/a"` → `"naan"`, `"?"` → `"s"`), which then propagated through the
  pipeline and got aggregated as content responses instead of missing data.
  `Spellcheck2()` now short-circuits before hunspell when the input has no
  letters and no digits (pure punctuation: `"?"`, `"--"`, `"..."`) or matches
  an explicit n/a variant (`"n/a"`, `"N/A"`, `"n.a."`, `"#N/A"`, etc.) and
  returns the canonical `"na"` placeholder.

### Breaking changes

* **Global valence rename**: `score_valence()` now outputs `ValenceYesNA`
  instead of `Valence`. `ValenceNoNA` is unchanged. Downstream code that reads
  `data$Valence` must migrate to `data$ValenceYesNA`.

* **Per-dimension valence columns renamed and extended**: `match_dictionaries()`
  now produces three per-dimension valence columns instead of two. The column
  formerly named `{Dim}_valence` is now `{Dim}_valenceStrictNA`; the new
  default column is `{Dim}_Valence` (capital V):

  | Old name | New name | Semantics |
  |---|---|---|
  | `{Dim}_valence` | `{Dim}_valenceStrictNA` | NA if dim not tagged **or** `ValenceYesNA` is NA |
  | *(new)* | `{Dim}_Valence` | NA if dim not tagged; else `ValenceNoNA` (0 for sentiment-less tagged responses) |
  | `{Dim}_valenceNoNA` | `{Dim}_valenceNoNA` | Unchanged: 0 if dim not tagged; NA only for missing response |

  The most common substitution is `_valence` → `_Valence` (capital V), which
  silently **changes the semantics** of downstream means: tagged responses with
  no matched sentiment words now contribute **0** (neutral) rather than being
  dropped as NA. Verify that this is the desired behaviour before migrating.

* **`match_dictionaries()` default parameter**: `valence_col` default changed
  from `"Valence"` to `"ValenceYesNA"` to match the renamed output of
  `score_valence()`. Callers that pass the old default explicitly must update.

### Migration guide

```r
# Old
data$Valence
combined$Sociability_valence

# New
data$ValenceYesNA          # NA-gated global (was Valence)
data$ValenceNoNA           # zero-imputed global (unchanged)
combined$Sociability_Valence         # new default: NA when not tagged, else ValenceNoNA
combined$Sociability_valenceStrictNA # old "_valence" semantics
combined$Sociability_valenceNoNA     # unchanged
```
