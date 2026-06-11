# Average Word embeddings for seed dictionaries

Average seed embeddings for SADCAT dictionaries rebuilt from curated
seed words.

## Usage

``` r
Seed_Vectors_Avg
```

## Format

A data frame with seed vectors. Key column groups:

- `Dictionary`: Dictionary row label (base, `_hi`, `_lo`, or `_dir`)

- `SBERT_1`...`SBERT_768`: SBERT embedding dimensions

- `Gemini_1`...`Gemini_768`: Gemini embedding dimensions
