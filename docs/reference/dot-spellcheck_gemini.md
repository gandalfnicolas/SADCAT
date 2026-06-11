# Spell-check texts using Gemini LLM

Spell-check texts using Gemini LLM

## Usage

``` r
.spellcheck_gemini(
  texts,
  api_key,
  model = "gemini-2.0-flash",
  batch_size = 50L,
  sleep = 5,
  context_prompt = NULL,
  verbose = TRUE
)
```

## Arguments

- texts:

  Character vector of text values to spell-check

- api_key:

  Gemini API key

- model:

  Gemini model name (default "gemini-2.0-flash")

- batch_size:

  Number of texts per API call (default 50)

- sleep:

  Seconds between batches (default 5)

- context_prompt:

  System prompt providing task context for Gemini

- verbose:

  Print progress? (default TRUE)

## Value

Character vector of corrected texts (same length as input)
