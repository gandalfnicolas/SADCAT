# Suppress R CMD check NOTEs for non-standard evaluation variables
# and package data objects used as default arguments
utils::globalVariables(c(
  # Pipe operator
  "%>%",
  # Package data objects used as default function arguments
  "All.steps_Dictionaries",
  "Seed_Vectors_Avg",
  "Dictionaries",
  # NSE / tidyverse column names used in new pipeline files
  ".data", "sentiment", "Val_lexicoder", "Estimate", "p.value",
  # NSE column names from pre-existing files
  "Sentiments", "word4", "Val_bing", "Val_NRC", "Val_afinn",
  "Val_loughran", "Val_sentiwn", "word", ".", "Val", "contains",
  "Val_SWN", "Val_bing2", "Val_NRC2", "Val_afinn2", "Val_loughran2",
  "Val_sentiwn2", "funs", "getRelatedSynsets", "getWord",
  "str_replace", "str_replace_all", "str_trim"
))
