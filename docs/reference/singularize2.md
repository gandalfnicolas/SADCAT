# Singularize a Word

Singularize a word, respecting SADCAT dictionary. Converts plural forms
to singular. Skips words already in the SADCAT dictionary. Handles
irregular plurals via switch cases and standard
"ves"/"ies"/"zes"/"ses"/"es" suffix rules.

## Usage

``` r
singularize2(word, dictionary = TRUE)
```

## Arguments

- word:

  A single character string to singularize

- dictionary:

  Logical. If TRUE, check result against SemNetDictionaries

## Value

Singularized version of the word
