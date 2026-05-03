# SADCAT NEWS

## SADCAT 0.2.0 (development)

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
