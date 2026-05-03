canonical_response_fixture <- function() {
  dat <- data.frame(
    row_id = 1:30,
    responsex = c(
      "warm",
      "not warm",
      "friendly",
      "non-friendly",
      "competent",
      "not competent",
      "immoral",
      "not immoral",
      "warm but not competent",
      "old but wise",
      "lazy and rude",
      "hard-working and honest",
      "kind, generous, and caring",
      "cold",
      "rich",
      "poor",
      "family oriented",
      "doctor",
      "engineer",
      "teenager",
      "woman",
      "black",
      "",
      NA_character_,
      "warm!",
      "Warm",
      "not warm and friendly",
      "limited competence but warm",
      "no one is friendly",
      "bad at math"
    ),
    stringsAsFactors = FALSE
  )
  dat$response <- dat$responsex
  dat
}

adversarial_response_fixture <- function(n = 64L, seed = 1L) {
  set.seed(seed)
  base <- c(
    "warm", "friendly", "competent", "immoral", "doctor", "teenager",
    "family oriented", "not warm", "warm but not competent", "non-friendly"
  )
  punct <- c("", "!", ".", "?", "...")
  make_case <- list(
    function(x) x,
    function(x) toupper(x),
    function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  )
  values <- character(n)
  for (i in seq_len(n)) {
    text <- sample(base, 1)
    text <- make_case[[sample.int(length(make_case), 1)]](text)
    text <- paste0(sample(c("", "  ", "\""), 1), text, sample(punct, 1), sample(c("", " ", "\""), 1))
    text <- gsub(" ", sample(c(" ", "  ", "-"), 1), text, fixed = TRUE)
    values[i] <- text
  }
  if (n >= 8) {
    values[c(2, 3)] <- values[1]
    values[4] <- "warm!"
    values[5] <- "Warm"
    values[6] <- ""
    values[7] <- NA_character_
    values[8] <- "not warm and friendly"
  }
  data.frame(
    row_id = seq_len(n),
    responsex = values,
    response = values,
    stringsAsFactors = FALSE
  )
}

simple_seed_vectors <- function() {
  data.frame(
    Dictionary = c("Warmth", "Competence"),
    SBERT_1 = c(1, 0),
    SBERT_2 = c(0, 1),
    stringsAsFactors = FALSE
  )
}

simple_embedding_fixture <- function() {
  data.frame(
    row_id = 1:4,
    response = c("warm", "competent", NA, "mixed"),
    SBERT_1 = c(1, 0, 1, 0.5),
    SBERT_2 = c(0, 1, 1, 0.5),
    stringsAsFactors = FALSE
  )
}

pipeline_aggregate_fixture <- function() {
  data.frame(
    Synonym.GroupX = c("A", "A", "B"),
    Group = c("G", "G", "G"),
    Level = c("L", "L", "L"),
    warmth_dic_binary2 = c(1, 0, 1),
    ValenceYesNA = c(1, -1, NA),
    ValenceNoNA = c(1, -1, 0),
    Warmth_Valence = c(1, NA, 0),
    Warmth_valenceStrictNA = c(1, NA, NA),
    Warmth_valenceNoNA = c(1, 0, 0),
    Warmth_dirx3 = c(1, NA, -1),
    Warning = c("note", "note", NA),
    stringsAsFactors = FALSE
  )
}

# Fixture for testing three-column valence semantics.
# "warm" is in the Warmth dictionary. Rows vary ValenceYesNA and binary state.
valence_semantics_fixture <- function() {
  data.frame(
    response    = c("warm",      "warm",      "neutral word", NA_character_),
    tv3         = c("warm",      "warm",      "neutral word", NA_character_),
    ValenceYesNA = c(0.5,        NA_real_,    NA_real_,       NA_real_),
    ValenceNoNA  = c(0.5,        0,           0,              0),
    stringsAsFactors = FALSE
  )
}

wordnet_test_path <- function() {
  Sys.getenv("SADCAT_WORDNET_DICT", "")
}

has_wordnet_test_env <- function() {
  nzchar(wordnet_test_path()) && dir.exists(wordnet_test_path())
}

has_treetagger_test_env <- function() {
  identical(tolower(Sys.getenv("SADCAT_RUN_TREETAGGER_TESTS", "false")), "true") &&
    dir.exists("C:\\treetagger") &&
    requireNamespace("koRpus.lang.en", quietly = TRUE)
}
