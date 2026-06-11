# Preprocess Text Responses

Preprocess text responses: lowercase, spell-check, singularize. Takes a
data frame with a raw text column and produces cleaned versions: `tv`
(lowercased), `tv2` (spell-checked), `tv3` (singularized).

## Usage

``` r
preprocess_text(
  data,
  text_col = "responsex",
  spellcheck = TRUE,
  spellcheck_method = "hunspell",
  singularize = TRUE,
  java_home = NULL,
  wordnet_dict = NULL,
  gemini_spellcheck_key = NULL,
  gemini_spellcheck_model = "gemini-2.0-flash",
  gemini_spellcheck_context = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame containing text responses

- text_col:

  Name of the column with raw text (default "responsex")

- spellcheck:

  Logical. Run spell-checking? (default TRUE)

- spellcheck_method:

  Method for spell-checking: "hunspell" (default, uses Java + WordNet +
  hunspell with edit-distance ranking and bigram context) or "gemini"
  (uses Gemini LLM for context-aware correction, requires API key).

- singularize:

  Logical. Run singularization? (default TRUE)

- java_home:

  Optional Java home path. If NULL or not found, auto-detects from
  environment, PATH, and common install locations. Only used when
  spellcheck_method = "hunspell".

- wordnet_dict:

  Optional WordNet dictionary path. If NULL or not found, auto-detects
  from environment and common install locations. Only used when
  spellcheck_method = "hunspell".

- gemini_spellcheck_key:

  Gemini API key for LLM spell-checking. If NULL, reads from
  Sys.getenv("GEMINI_API_KEY"). Only used when spellcheck_method =
  "gemini".

- gemini_spellcheck_model:

  Gemini model for spell-checking (default "gemini-2.0-flash").

- gemini_spellcheck_context:

  Custom system prompt providing context for Gemini spell-checking. If
  NULL, uses a default prompt describing the text as social target
  descriptions.

- verbose:

  Print progress? (default TRUE)

## Value

The input data with columns `tv`, `tv2`, `tv3` added
