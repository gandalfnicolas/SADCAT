# =============================================================================
# SADCAT Pipeline Functions for LLM Response Processing
# =============================================================================
# This file consolidates the ~600-line text processing, dictionary coding,
# sentiment scoring, embedding retrieval, seed similarity computation, and
# aggregation pipeline into reusable functions.
#
# Intended for eventual inclusion in the SADCAT R package.
#
# Usage:
#   source("sadcat_pipeline_functions.R")
#   result <- process_responses(my_data, text_col = "responsex")
#
# Or stage-by-stage:
#   dat <- preprocess_text(dat, text_col = "responsex")
#   dat <- score_valence(dat)
#   dat <- match_dictionaries(dat)
#   dat <- compute_embeddings(dat, methods = c("sbert", "gemini"))
#   dat <- compute_seed_similarities(dat, embedding_prefix = "SBERT")
#   agg <- aggregate_responses(dat)
# =============================================================================

# ---- Package Dependencies ---------------------------------------------------
# These would go in DESCRIPTION Imports when added to SADCAT package
# Required: SADCAT, quanteda, tidytext, dplyr, tidyr, stringr, lsa, plyr
# Optional: reticulate (for embeddings), rJava, wordnet, hunspell,
#           SemNetDictionaries (for singularize2)

# ---- Constants --------------------------------------------------------------

# Primary SADCAT dimensions (those with hi/lo directional variants)
.SADCAT_DIR_DIMS <- c("Sociability", "Morality", "Ability", "Assertiveness",
                      "Status", "Warmth", "Competence", "Beliefs",
                      "Health", "Beauty", "Deviance")

# Non-directional dimensions (binary-only, no hi/lo variants)
.SADCAT_NDIR_DIMS <- c("Occupation", "Emotion", "Family", "Socialgroups",
                       "Geography", "Appearance", "Other", "OtherwFam")

# All dimensions
.SADCAT_ALL_DIMS <- c(.SADCAT_DIR_DIMS, .SADCAT_NDIR_DIMS)

# Negation patterns
.NEGATION_PATTERN <- "[Aa]nti-|[Nn]on-|[Bb]ad at |[Ll]imited |[Nn]ot |[Nn]o |[Nn]ever |[Nn]either |[Hh]ardly |[Ll]ess "
.NEGATION_PATTERN_VAL <- "[Aa]nti-|[Nn]on-"
.NEGATION_TOKEN_SEPARATOR <- "__"
.NEGATION_SCOPE_MAX_TOKENS <- 4L
.NEGATION_CLAUSE_BREAKERS <- c(".", ",", ";", ":", "!", "?", "but", "however", "though", "although", "yet")
.NEGATION_CUE_HEADS <- c("anti", "non", "not", "no", "never", "neither", "hardly", "less", "limited")

# Cache environment for memoized dictionary preparation
.sadcat_cache <- new.env(parent = emptyenv())
.negation_cache <- new.env(parent = emptyenv())


# ---- Utility Functions ------------------------------------------------------

#' Compute cosine similarity between a vector and columns of a matrix
#'
#' @param vector Numeric vector
#' @param dictionaries Matrix where each column is a reference vector
#' @return Named numeric vector of cosine similarities
#' @export
cosinize <- function(vector, dictionaries) {
  apply(dictionaries, 2, function(x)
    lsa::cosine(as.vector(unlist(x)), as.vector(unlist(vector))))
}

#' Compute Euclidean distance between a vector and columns of a matrix
#'
#' @param vector Numeric vector
#' @param dictionaries Matrix where each column is a reference vector
#' @return Named numeric vector of Euclidean distances
#' @export
euclideanize <- function(vector, dictionaries) {
  apply(dictionaries, 2, function(x)
    dist(rbind(as.vector(unlist(x)), as.vector(unlist(vector)))))
}

#' Create compact letter display tables from model and emmeans
#'
#' @param modelx A fitted model object
#' @param emmeans An emmeans object
#' @param round_digits Number of digits to round to (default 3)
#' @param stat_adjust P-value adjustment method (default "Tukey")
#' @return Data frame with CLD annotations
#' @export
cldtable <- function(modelx, emmeans, round_digits = 3, stat_adjust = "Tukey") {
  lmveremdat <- as.data.frame(emmeans)
  lmveremdat <- lmveremdat[order(-lmveremdat[[2]]), ]
  if ("null" %in% colnames(lmveremdat)) {
    lmveremdat <- lmveremdat[, !colnames(lmveremdat) %in% "null"]
  }
  lmveremdat <- lmveremdat %>% dplyr::mutate_if(is.numeric, round, digits = round_digits)
  lmveremdat$p.value <- ifelse(
    lmveremdat$p.value %in% c("0.000", "0.00", "0.0", "0"),
    "<.001", lmveremdat$p.value
  )
  lmveremdat <- as.data.frame(lmveremdat)
  concatenated_values <- paste0("[", paste(lmveremdat[, 5], lmveremdat[, 6], sep = ", "), "]")
  lmveremdat <- as.data.frame(lmveremdat)[, -c(5, 6)]
  lmveremdat <- data.frame(lmveremdat[, 1:2], concatenated_values, lmveremdat[, 3:(ncol(lmveremdat))])
  names(lmveremdat) <- c(names(lmveremdat)[1], sub("\\~.*", "", formula(modelx))[[2]],
                         "95% CI", "SE", "df", names(lmveremdat)[6], "p")
  model_means_cld <- multcomp::cld(object = emmeans, adjust = stat_adjust,
                                   Letters = letters, alpha = 0.05, sort = TRUE, reversed = TRUE)
  lmveremdat[, 2] <- paste(lmveremdat[, 2], trimws(model_means_cld$.group), sep = "")
  return(lmveremdat)
}

#' Compute Cohen's d from emmeans pairwise comparisons
#'
#' @param mod A fitted mixed model object
#' @param pairwises Pairwise comparisons from emmeans
#' @return Data frame with contrast info and Cohen's d values
#' @export
Cohens_d_EMmeans <- function(mod, pairwises) {
  cohens_d_sigma <- sqrt(sum(as.data.frame(lme4::VarCorr(mod))$vcov))
  Cohens <- abs(as.data.frame(pairwises)$Estimate / cohens_d_sigma)
  info <- dplyr::select(as.data.frame(pairwises), -c(Estimate:p.value))
  cbind(info, Cohens)
}

#' NaN-aware replacement for data frames
#' @keywords internal
replace_nan_with_na <- function(df) {
  is_nan_df <- do.call(cbind, lapply(df, is.nan))
  df[is_nan_df] <- NA
  df
}

.prepare_quanteda_text <- function(x, prefix = "doc") {
  x <- enc2utf8(as.character(x))
  x <- unname(x)
  names(x) <- paste0(prefix, "_", seq_along(x))
  x
}

.tokenize_quanteda_text <- function(x, prefix = "doc", ...) {
  quanteda::tokens(.prepare_quanteda_text(x, prefix = prefix), ...)
}

.normalize_valence_text <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  x <- stringr::str_replace_all(
    x,
    "(?i)\\banti\\s*(?:-|\\s)\\s*(?=[[:alpha:]])",
    " zzzneganti "
  )
  x <- SADCAT::clean_large_text(x)
  x <- stringr::str_replace_all(x, "\\((no|non|nor|neither|never|hardly|nobody|none|avoid|avoided|avoiding|avoids|nothing|less|minimize|reduce|withdraw|low|little|without|no_longer)\\)", " ")
  x <- stringr::str_replace_all(x, "zzzneganti", "anti")
  stringr::str_squish(x)
}

.normalize_valence_terms <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  x <- SADCAT::clean_large_text(x)
  x <- stringr::str_replace_all(x, "\\((no|non|nor|neither|never|hardly|nobody|none|avoid|avoided|avoiding|avoids|nothing|less|minimize|reduce|withdraw|low|little|without|no_longer)\\)", " ")
  stringr::str_squish(x)
}

.normalize_sadcat_text <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  x <- stringr::str_replace_all(x, "-", " ")
  x <- stringr::str_replace_all(x, "([\\.,;:!?])", " \\1 ")
  stringr::str_squish(x)
}

.normalize_sadcat_terms <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  x <- stringr::str_replace_all(x, "-", " ")
  stringr::str_squish(x)
}

.singularize_token_vector <- function(tokens) {
  if (!length(tokens)) {
    return(tokens)
  }
  word_idx <- grepl("[[:alpha:]]", tokens)
  if (any(word_idx)) {
    tokens[word_idx] <- vapply(tokens[word_idx], SADCAT::delete_ending_Ss, character(1))
  }
  tokens
}

.normalize_term_set <- function(terms,
                                normalizer,
                                singularize_terms = FALSE,
                                drop_cue_leading = FALSE) {
  if (is.null(terms) || length(terms) == 0) {
    return(character(0))
  }

  toks <- .tokenize_quanteda_text(
    normalizer(terms),
    prefix = "term",
    remove_numbers = FALSE,
    remove_punct = TRUE,
    remove_symbols = TRUE
  )
  tok_list <- quanteda::as.list(toks)
  if (singularize_terms) {
    tok_list <- lapply(tok_list, .singularize_token_vector)
  }

  out <- vapply(tok_list, function(tok) {
    tok <- tok[nzchar(tok)]
    if (!length(tok)) {
      return("")
    }
    paste(tok, collapse = " ")
  }, character(1))

  out <- unique(out[nzchar(out)])
  if (drop_cue_leading) {
    out <- out[!vapply(out, .starts_with_negation_cue, logical(1))]
  }
  out
}

.starts_with_negation_cue <- function(term) {
  if (is.na(term) || !nzchar(term)) {
    return(FALSE)
  }
  parts <- strsplit(term, " ", fixed = TRUE)[[1]]
  if (!length(parts)) {
    return(FALSE)
  }
  if (length(parts) >= 2 && identical(parts[1:2], c("bad", "at"))) {
    return(TRUE)
  }
  parts[1] %in% .NEGATION_CUE_HEADS
}

.join_term_tokens <- function(terms) {
  if (!length(terms)) {
    return(character(0))
  }
  stringr::str_replace_all(terms, " ", .NEGATION_TOKEN_SEPARATOR)
}

.filter_negated_phrase_terms <- function(terms) {
  if (!length(terms)) {
    return(terms)
  }
  keep <- grepl(" ", terms, fixed = TRUE) | !vapply(terms, .starts_with_negation_cue, logical(1))
  terms[keep]
}

.build_compound_patterns <- function(terms) {
  multi_terms <- unique(terms[grepl(" ", terms, fixed = TRUE)])
  if (!length(multi_terms)) {
    return(NULL)
  }
  quanteda::phrase(multi_terms)
}

.tokenize_for_scoped_negation <- function(text,
                                          normalizer,
                                          singularize_words = FALSE,
                                          compound_patterns = NULL) {
  toks <- .tokenize_quanteda_text(
    normalizer(text),
    prefix = "response",
    remove_numbers = FALSE,
    remove_punct = FALSE,
    remove_symbols = FALSE
  )

  if (singularize_words) {
    tok_list <- quanteda::as.list(toks)
    tok_list <- lapply(tok_list, .singularize_token_vector)
    toks <- quanteda::as.tokens(tok_list)
  }

  if (!is.null(compound_patterns) && length(compound_patterns) > 0) {
    toks <- quanteda::tokens_compound(
      toks,
      pattern = compound_patterns,
      concatenator = .NEGATION_TOKEN_SEPARATOR
    )
  }

  lapply(quanteda::as.list(toks), as.character)
}

.is_punctuation_token <- function(token) {
  grepl("^[[:punct:]]+$", token)
}

.mark_negated_tokens <- function(tokens, cue_tokens) {
  if (!length(tokens)) {
    return(logical(0))
  }

  flip_counts <- integer(length(tokens))
  cue_idx <- which(tokens %in% cue_tokens)
  if (!length(cue_idx)) {
    return(rep(FALSE, length(tokens)))
  }

  for (idx in cue_idx) {
    seen_words <- 0L
    j <- idx + 1L
    while (j <= length(tokens)) {
      token <- tokens[j]
      if (token %in% .NEGATION_CLAUSE_BREAKERS) {
        break
      }
      if (.is_punctuation_token(token)) {
        j <- j + 1L
        next
      }

      flip_counts[j] <- flip_counts[j] + 1L
      seen_words <- seen_words + 1L
      if (seen_words >= .NEGATION_SCOPE_MAX_TOKENS) {
        break
      }
      j <- j + 1L
    }
  }

  flip_counts %% 2L == 1L
}

.build_scoped_token_profile <- function(text,
                                        normalizer,
                                        cue_tokens,
                                        compound_patterns = NULL,
                                        singularize_words = FALSE) {
  tokens <- .tokenize_for_scoped_negation(
    text = text,
    normalizer = normalizer,
    singularize_words = singularize_words,
    compound_patterns = compound_patterns
  )
  negated <- lapply(tokens, .mark_negated_tokens, cue_tokens = cue_tokens)
  list(tokens = tokens, negated = negated)
}

.compute_group_presence <- function(profile, term_groups) {
  group_names <- names(term_groups)
  n_docs <- length(profile$tokens)

  nonneg_mat <- matrix(FALSE, nrow = n_docs, ncol = length(group_names),
                       dimnames = list(NULL, group_names))
  neg_mat <- matrix(FALSE, nrow = n_docs, ncol = length(group_names),
                    dimnames = list(NULL, group_names))

  for (i in seq_len(n_docs)) {
    doc_tokens <- profile$tokens[[i]]
    doc_negated <- profile$negated[[i]]
    if (!length(doc_tokens)) {
      next
    }

    keep_idx <- !vapply(doc_tokens, .is_punctuation_token, logical(1))
    doc_tokens <- doc_tokens[keep_idx]
    doc_negated <- doc_negated[keep_idx]
    if (!length(doc_tokens)) {
      next
    }

    nonneg_tokens <- unique(doc_tokens[!doc_negated])
    neg_tokens <- unique(doc_tokens[doc_negated])

    nonneg_mat[i, ] <- vapply(term_groups, function(terms) {
      length(terms) > 0 && any(nonneg_tokens %in% terms)
    }, logical(1))
    neg_mat[i, ] <- vapply(term_groups, function(terms) {
      length(terms) > 0 && any(neg_tokens %in% terms)
    }, logical(1))
  }

  list(nonneg = nonneg_mat, neg = neg_mat)
}

.get_valence_negation_spec <- function() {
  cache_key <- "valence_negation_spec"
  if (exists(cache_key, envir = .negation_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .negation_cache, inherits = FALSE))
  }

  nrc_dict <- build_sentiment_dictionary("nrc")
  bing_dict <- build_sentiment_dictionary("bing")
  afinn_dict <- build_sentiment_dictionary("afinn")
  loughran_dict <- build_sentiment_dictionary("loughran")
  lex_dict <- quanteda::data_dictionary_LSD2015

  term_groups <- list(
    lex_positive = .join_term_tokens(.normalize_term_set(as.list(lex_dict)$positive, .normalize_valence_terms, drop_cue_leading = TRUE)),
    lex_negative = .join_term_tokens(.normalize_term_set(as.list(lex_dict)$negative, .normalize_valence_terms, drop_cue_leading = TRUE)),
    nrc_positive = .join_term_tokens(.normalize_term_set(as.list(nrc_dict)[[1]], .normalize_valence_terms, drop_cue_leading = TRUE)),
    nrc_negative = .join_term_tokens(.normalize_term_set(as.list(nrc_dict)[[2]], .normalize_valence_terms, drop_cue_leading = TRUE)),
    bing_positive = .join_term_tokens(.normalize_term_set(as.list(bing_dict)[[1]], .normalize_valence_terms, drop_cue_leading = TRUE)),
    bing_negative = .join_term_tokens(.normalize_term_set(as.list(bing_dict)[[2]], .normalize_valence_terms, drop_cue_leading = TRUE)),
    affin_positive = .join_term_tokens(.normalize_term_set(as.list(afinn_dict)[[1]], .normalize_valence_terms, drop_cue_leading = TRUE)),
    affin_negative = .join_term_tokens(.normalize_term_set(as.list(afinn_dict)[[2]], .normalize_valence_terms, drop_cue_leading = TRUE)),
    loughran_positive = .join_term_tokens(.normalize_term_set(as.list(loughran_dict)[[1]], .normalize_valence_terms, drop_cue_leading = TRUE)),
    loughran_negative = .join_term_tokens(.normalize_term_set(as.list(loughran_dict)[[2]], .normalize_valence_terms, drop_cue_leading = TRUE))
  )

  compound_terms <- unique(c(
    unlist(lapply(term_groups, function(x) stringr::str_replace_all(x, .NEGATION_TOKEN_SEPARATOR, " ")), use.names = FALSE),
    "bad at"
  ))
  compound_terms <- compound_terms[grepl(" ", compound_terms, fixed = TRUE)]

  spec <- list(
    term_groups = term_groups,
    compound_patterns = .build_compound_patterns(compound_terms),
    cue_tokens = c("anti", "not", "limited", paste0("bad", .NEGATION_TOKEN_SEPARATOR, "at")),
    lex_neg_positive_terms = .filter_negated_phrase_terms(.normalize_term_set(as.list(lex_dict)$neg_positive, .normalize_valence_terms)),
    lex_neg_negative_terms = .filter_negated_phrase_terms(.normalize_term_set(as.list(lex_dict)$neg_negative, .normalize_valence_terms))
  )

  assign(cache_key, spec, envir = .negation_cache)
  spec
}

.get_sadcat_direction_negation_spec <- function(sadcat_dict) {
  dict_list <- as.list(sadcat_dict)
  cache_key <- paste0(
    "sadcat_direction_negation_spec__",
    paste(names(dict_list), lengths(dict_list), collapse = "|")
  )
  if (exists(cache_key, envir = .negation_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .negation_cache, inherits = FALSE))
  }

  term_groups <- list()
  dims <- list()

  for (dim in .SADCAT_DIR_DIMS) {
    hi_name <- paste0(dim, "_dic_hi")
    lo_name <- paste0(dim, "_dic_lo")
    hi_terms <- .join_term_tokens(.normalize_term_set(dict_list[[hi_name]], .normalize_sadcat_terms, singularize_terms = TRUE, drop_cue_leading = TRUE))
    lo_terms <- .join_term_tokens(.normalize_term_set(dict_list[[lo_name]], .normalize_sadcat_terms, singularize_terms = TRUE, drop_cue_leading = TRUE))

    hi_group <- paste0(dim, "_hi")
    lo_group <- paste0(dim, "_lo")
    term_groups[[hi_group]] <- hi_terms
    term_groups[[lo_group]] <- lo_terms
    dims[[dim]] <- c(hi_group = hi_group, lo_group = lo_group)
  }

  compound_terms <- unique(c(
    unlist(lapply(term_groups, function(x) stringr::str_replace_all(x, .NEGATION_TOKEN_SEPARATOR, " ")), use.names = FALSE),
    "bad at"
  ))
  compound_terms <- compound_terms[grepl(" ", compound_terms, fixed = TRUE)]

  spec <- list(
    term_groups = term_groups,
    dims = dims,
    compound_patterns = .build_compound_patterns(compound_terms),
    cue_tokens = c(
      "anti", "non", "not", "no", "never", "neither",
      "hardly", "less", "limited",
      paste0("bad", .NEGATION_TOKEN_SEPARATOR, "at")
    )
  )

  assign(cache_key, spec, envir = .negation_cache)
  spec
}

.lookup_phrase_presence <- function(text, terms) {
  if (!length(terms)) {
    return(rep(FALSE, length(text)))
  }
  toks <- .tokenize_quanteda_text(
    .normalize_valence_terms(text),
    prefix = "phrase",
    remove_numbers = FALSE,
    remove_punct = TRUE,
    remove_symbols = TRUE
  )
  toks <- quanteda::tokens_compound(
    toks,
    pattern = quanteda::phrase(terms),
    concatenator = .NEGATION_TOKEN_SEPARATOR
  )
  term_tokens <- .join_term_tokens(terms)
  vapply(quanteda::as.list(toks), function(tok) {
    any(tok %in% term_tokens)
  }, logical(1))
}

.score_valence_from_scoped_tokens <- function(text) {
  spec <- .get_valence_negation_spec()
  profile <- .build_scoped_token_profile(
    text = text,
    normalizer = .normalize_valence_text,
    cue_tokens = spec$cue_tokens,
    compound_patterns = spec$compound_patterns,
    singularize_words = FALSE
  )
  presence <- .compute_group_presence(profile, spec$term_groups)
  lex_neg_presence <- list(
    neg_positive = .lookup_phrase_presence(text, spec$lex_neg_positive_terms),
    neg_negative = .lookup_phrase_presence(text, spec$lex_neg_negative_terms)
  )

  build_output <- function(pos_key, neg_key, name, neg_pos_key = NULL, neg_neg_key = NULL) {
    # Raw presence (no negation flipping) for individual dictionary columns
    any_pos <- presence$nonneg[, pos_key] | presence$neg[, pos_key]
    any_neg <- presence$nonneg[, neg_key] | presence$neg[, neg_key]

    # Negation-aware (for computing combined average only)
    neg_pos_binary <- presence$nonneg[, pos_key] | presence$neg[, neg_key]
    neg_neg_binary <- presence$nonneg[, neg_key] | presence$neg[, pos_key]

    if (!is.null(neg_pos_key)) {
      # Lexicoder neg_positive = negated positive phrases -> negative sentiment
      any_neg <- any_neg | lex_neg_presence[[neg_pos_key]]
      neg_neg_binary <- neg_neg_binary | lex_neg_presence[[neg_pos_key]]
    }
    if (!is.null(neg_neg_key)) {
      # Lexicoder neg_negative = negated negative phrases -> positive sentiment
      any_pos <- any_pos | lex_neg_presence[[neg_neg_key]]
      neg_pos_binary <- neg_pos_binary | lex_neg_presence[[neg_neg_key]]
    }

    # Raw individual scores (no negation)
    raw_val <- any_pos - any_neg
    raw_valna <- ifelse(any_pos + any_neg == 0, NA, raw_val)

    # Negation-aware scores (internal, for combined average)
    neg_val <- neg_pos_binary - neg_neg_binary
    neg_valna <- ifelse(neg_pos_binary + neg_neg_binary == 0, NA, neg_val)

    out <- data.frame(raw_val = raw_val, raw_valna = raw_valna, neg_valna = neg_valna)
    colnames(out) <- c(paste0("Val_", name), paste0("Val_", name, "NA"),
                        paste0(".neg_valna_", name))
    out
  }

  out <- cbind(
    build_output("lex_positive", "lex_negative", "lexicoder", "neg_positive", "neg_negative"),
    build_output("nrc_positive", "nrc_negative", "NRC"),
    build_output("bing_positive", "bing_negative", "bing"),
    build_output("affin_positive", "affin_negative", "affin"),
    build_output("loughran_positive", "loughran_negative", "loughran")
  )

  replace_nan_with_na(out)
}

.compute_adjusted_direction_scores <- function(text, sadcat_dict) {
  spec <- .get_sadcat_direction_negation_spec(sadcat_dict)
  profile <- .build_scoped_token_profile(
    text = text,
    normalizer = .normalize_sadcat_text,
    cue_tokens = spec$cue_tokens,
    compound_patterns = spec$compound_patterns,
    singularize_words = TRUE
  )
  presence <- .compute_group_presence(profile, spec$term_groups)

  out <- data.frame(row.names = seq_along(text))
  for (dim in names(spec$dims)) {
    hi_group <- spec$dims[[dim]][["hi_group"]]
    lo_group <- spec$dims[[dim]][["lo_group"]]

    hi_binary <- presence$nonneg[, hi_group] | presence$neg[, lo_group]
    lo_binary <- presence$nonneg[, lo_group] | presence$neg[, hi_group]
    score <- hi_binary - lo_binary

    out[[paste0(dim, "_dirx")]] <- score
    out[[paste0(dim, "_dirx2")]] <- score
  }

  out
}


# =============================================================================
# STAGE 1: preprocess_text()
# =============================================================================

#' Spell-check text using SADCAT dictionary, WordNet, and hunspell
#'
#' Checks if word is in dictionary, correctly spelled, or in WordNet.
#' If not, uses hunspell suggestions. Requires rJava, wordnet, hunspell.
#'
#' @param raw A single character string to spell-check
#' @param dict_tv Character vector of known dictionary words
#' @param rawlist Character vector of all raw values (for matching suggestions)
#' @return Spell-checked version of the input
#' @export
Spellcheck2 <- function(raw, dict_tv = SADCAT::All.steps_Dictionaries$tv, rawlist,
                         bigram_freqs = NULL) {
  tryCatch({
    if (raw == "NA" | is.na(raw)) {
      return("na")
    }
    if (raw %in% dict_tv) {
      return(raw)
    }
    if (hunspell::hunspell_check(raw)) {
      return(raw)
    }
    # Multi-word: split, spell-check each word with left/right context, rejoin
    if (grepl(" ", raw) | grepl("-", raw)) {
      sep <- if (grepl(" ", raw)) " " else "-"
      words <- strsplit(raw, sep, fixed = TRUE)[[1]]
      n <- length(words)
      corrected <- character(n)
      for (i in seq_len(n)) {
        left <- if (i > 1) corrected[i - 1] else NULL
        right <- if (i < n) words[i + 1] else NULL
        corrected[i] <- .spellcheck_single_word(
          words[i], dict_tv = dict_tv, rawlist = rawlist,
          left_word = left, right_word = right,
          bigram_freqs = bigram_freqs
        )
      }
      result <- paste(corrected, collapse = sep)
      result <- SADCAT::clean_naresponses(result)
      return(tolower(result))
    }
    # Single word
    result <- .spellcheck_single_word(raw, dict_tv = dict_tv, rawlist = rawlist,
                                       bigram_freqs = bigram_freqs)
    result <- SADCAT::clean_naresponses(result)
    return(tolower(result))
  }, error = function(s) {
    return("*******ERROR")
  })
}

.spellcheck_single_word <- function(word, dict_tv, rawlist,
                                     left_word = NULL, right_word = NULL,
                                     bigram_freqs = NULL) {
  if (is.na(word) || word == "") return(word)
  if (word %in% dict_tv) return(word)
  if (hunspell::hunspell_check(word)) return(word)

  filter <- wordnet::getTermFilter("ExactMatchFilter", word, TRUE)
  if (!is.null(wordnet::getIndexTerms("NOUN", 5, filter)) ||
      !is.null(wordnet::getIndexTerms("ADJECTIVE", 5, filter)) ||
      !is.null(wordnet::getIndexTerms("VERB", 5, filter))) {
    return(word)
  }

  # Not in any dictionary, try hunspell suggestions ranked by edit distance
  suggestions <- hunspell::hunspell_suggest(word)[[1]]
  suggestions <- suggestions[!is.na(suggestions)]
  if (length(suggestions) == 0) return(word)

  suggestions <- utils::head(suggestions, 10)
  dists <- as.integer(utils::adist(word, suggestions))

  bg_scores <- rep(0L, length(suggestions))
  if (!is.null(bigram_freqs)) {
    bg_scores <- .score_bigram_context(suggestions, left_word, right_word,
                                        bigram_freqs)
  }

  in_rawlist <- suggestions %in% rawlist

  rank_df <- data.frame(
    idx = seq_along(suggestions),
    dist = dists,
    bg = bg_scores,
    inraw = in_rawlist,
    stringsAsFactors = FALSE
  )
  rank_df <- rank_df[order(rank_df$dist, -rank_df$bg, -rank_df$inraw), ]

  suggestions[rank_df$idx[1]]
}

.build_bigram_freqs <- function(corpus) {
  freqs <- new.env(hash = TRUE, parent = emptyenv())
  for (text in corpus) {
    if (is.na(text) || text == "") next
    words <- strsplit(text, "\\s+")[[1]]
    if (length(words) < 2) next
    for (i in seq_len(length(words) - 1)) {
      key <- paste0(words[i], "\t", words[i + 1])
      val <- get0(key, envir = freqs, ifnotfound = 0L)
      assign(key, val + 1L, envir = freqs)
    }
  }
  freqs
}

.score_bigram_context <- function(candidates, left_word, right_word,
                                   bigram_freqs) {
  scores <- integer(length(candidates))
  for (i in seq_along(candidates)) {
    s <- 0L
    if (!is.null(left_word)) {
      key <- paste0(left_word, "\t", candidates[i])
      s <- s + get0(key, envir = bigram_freqs, ifnotfound = 0L)
    }
    if (!is.null(right_word)) {
      key <- paste0(candidates[i], "\t", right_word)
      s <- s + get0(key, envir = bigram_freqs, ifnotfound = 0L)
    }
    scores[i] <- s
  }
  scores
}

.spellcheck_gemini <- function(texts,
                                api_key,
                                model = "gemini-2.0-flash",
                                batch_size = 50L,
                                sleep = 5,
                                context_prompt = NULL,
                                verbose = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for Gemini spellchecking. ",
         "Install with: install.packages('reticulate')")
  }

  if (is.null(context_prompt)) {
    context_prompt <- paste(
      "You are a spelling correction tool.",
      "The following texts are short responses describing a social target",
      "(e.g., a person, social group, or face).",
      "Correct any spelling errors while preserving the original meaning",
      "and wording. Do not add or remove words. Do not change capitalization",
      "beyond what is needed for correction.",
      "Return ONLY the corrected texts, one per line, in the same order.",
      "If a line has no errors, return it unchanged."
    )
  }

  genai <- reticulate::import("google.genai")
  types <- reticulate::import("google.genai.types")
  client <- genai$Client(api_key = api_key)

  corrected <- character(length(texts))
  n_batches <- ceiling(length(texts) / batch_size)

  for (b in seq_len(n_batches)) {
    start_idx <- (b - 1) * batch_size + 1
    end_idx <- min(b * batch_size, length(texts))
    batch <- texts[start_idx:end_idx]

    if (verbose) message("    Gemini spellcheck batch ", b, " / ", n_batches,
                         " (items ", start_idx, "-", end_idx, ")")

    numbered_input <- paste(seq_along(batch), batch, sep = ". ")
    prompt <- paste0(context_prompt, "\n\n",
                     paste(numbered_input, collapse = "\n"))

    result <- tryCatch({
      client$models$generate_content(
        model = model,
        contents = prompt
      )
    }, error = function(e) {
      warning("  Gemini batch ", b, " failed: ", conditionMessage(e))
      NULL
    })

    if (!is.null(result)) {
      response_text <- result$text
      lines <- strsplit(response_text, "\n")[[1]]
      lines <- trimws(lines)
      lines <- lines[nchar(lines) > 0]
      lines <- sub("^\\d+\\.\\s*", "", lines)

      if (length(lines) == length(batch)) {
        corrected[start_idx:end_idx] <- tolower(lines)
      } else {
        warning("  Gemini batch ", b, ": expected ", length(batch),
                " lines but got ", length(lines), ". Using originals.")
        corrected[start_idx:end_idx] <- batch
      }
    } else {
      corrected[start_idx:end_idx] <- batch
    }

    if (b < n_batches) {
      if (verbose) message("    Sleeping ", sleep, " seconds (rate limit)...")
      Sys.sleep(sleep)
    }
  }

  corrected
}

#' Singularize a word, respecting SADCAT dictionary
#'
#' Converts plural forms to singular. Skips words already in the SADCAT
#' dictionary and words ending in "es" (to avoid incorrect singularization).
#' Handles irregular plurals via switch cases.
#'
#' @param word A single character string to singularize
#' @param dictionary Logical. If TRUE, check result against SemNetDictionaries
#' @return Singularized version of the word
#' @export
singularize2 <- function(word, dictionary = TRUE) {
  if (word %in% SADCAT::All.steps_Dictionaries$tv) {
    return(word)
  }
  if (is.na(word)) {
    return(word)
  }
  if (stringr::str_detect(word, 'es$')) {
    return(word)
  }
  spl <- unlist(strsplit(word, " "))
  if (length(spl) > 1) {
    word <- spl[length(spl)]
    multiple <- TRUE
  } else {
    multiple <- FALSE
  }
  orig.word <- word
  checker <- SemNetDictionaries::general.dictionary
  chn <- FALSE
  word <- switch(word,
    children = "child", geese = "goose", men = "man", women = "woman",
    teeth = "tooth", feet = "foot", mice = "mouse", people = "person",
    lice = "louse", valves = "valve", trees = "tree", scribbles = "scribble",
    peduncles = "peduncle", mallees = "mallee", panicles = "panicle",
    ridges = "ridge", petioles = "petiole", angles = "angle",
    bristles = "bristle", edges = "edge", fissures = "fissure",
    sutures = "suture", occurrences = "occurrence", bees = "bee"
  )
  if (is.null(word)) {
    word <- orig.word
  } else {
    chn <- TRUE
  }
  last.lets <- substr(word, nchar(word) - 2, nchar(word))
  if (!chn) {
    if (any(last.lets == c("ves", "ies", "zes", "ses"))) {
      if (last.lets == "ves") {
        word <- substr(word, 1, nchar(word) - 3)
        word <- paste0(word, "f")
        if (!word %in% checker) word <- paste0(word, "e")
        chn <- TRUE
      } else if (last.lets == "ies") {
        word <- substr(word, 1, nchar(word) - 3)
        word <- paste0(word, "y")
        if (!word %in% checker) {
          word <- substr(word, 1, nchar(word) - 1)
          word <- paste0(word, "ie")
        }
        chn <- TRUE
      } else if (any(last.lets == c("zes", "ses"))) {
        word <- substr(word, 1, nchar(word) - 3)
        chn <- TRUE
        if (!word %in% checker) {
          word <- orig.word
          chn <- FALSE
        }
      }
    }
  }
  last.lets <- substr(word, nchar(word) - 1, nchar(word))
  if (!chn) {
    if (last.lets == "es") {
      word <- substr(word, 1, nchar(word) - 2)
      if (!word %in% checker) word <- paste0(word, "is")
      chn <- TRUE
    }
  }
  last.lets <- substr(word, nchar(word), nchar(word))
  if (!chn) {
    if (any(last.lets == c("s", "i", "a"))) {
      if (last.lets == "s") {
        word <- substr(word, 1, nchar(word) - 1)
      } else if (last.lets == "i") {
        word <- substr(word, 1, nchar(word) - 1)
        word <- paste0(word, "us")
      } else if (last.lets == "a") {
        word <- substr(word, 1, nchar(word) - 1)
        word <- paste0(word, "on")
      }
    }
  }
  if (isTRUE(dictionary)) {
    if (!word %in% checker) {
      if (isTRUE(multiple)) orig.word <- paste(spl, collapse = " ")
      return(orig.word)
    } else {
      if (multiple) {
        spl[length(spl)] <- word
        word <- paste(spl, collapse = " ")
      }
      return(word)
    }
  } else {
    if (multiple) {
      spl[length(spl)] <- word
      word <- paste(spl, collapse = " ")
    }
    return(word)
  }
}

#' Preprocess text responses: lowercase, spell-check, singularize
#'
#' Takes a data frame with a raw text column and produces cleaned versions:
#' \code{tv} (lowercased), \code{tv2} (spell-checked), \code{tv3} (singularized).
#'
#' @param data A data.frame containing text responses
#' @param text_col Name of the column with raw text (default "responsex")
#' @param spellcheck Logical. Run spell-checking? (default TRUE)
#' @param singularize Logical. Run singularization? (default TRUE)
#' @param java_home Path to Java JRE (needed for WordNet spell-checking)
#' @param wordnet_dict Path to WordNet dictionary files
#' @param verbose Print progress? (default TRUE)
#' @return The input data with columns \code{tv}, \code{tv2}, \code{tv3} added
#' @export
preprocess_text <- function(data,
                            text_col = "responsex",
                            spellcheck = TRUE,
                            spellcheck_method = "hunspell",
                            singularize = TRUE,
                            java_home = "C:\\Program Files\\Java\\jre-1.8",
                            wordnet_dict = "C:\\dict",
                            gemini_spellcheck_key = NULL,
                            gemini_spellcheck_model = "gemini-2.0-flash",
                            gemini_spellcheck_context = NULL,
                            verbose = TRUE) {
  if (verbose) message("--- Stage 1: Preprocessing text ---")

  # Step 1: Lowercase
  data$tv <- tolower(data[[text_col]])
  data$tv <- ifelse(data$tv == "", NA, data$tv)

  # Step 2: Spell-check on unique values
  if (spellcheck) {
    unique_vals <- unique(data$tv)

    if (spellcheck_method == "gemini") {
      # ---- Gemini LLM spell-checking ----
      if (verbose) message("  Spell-checking via Gemini LLM...")
      api_key <- gemini_spellcheck_key
      if (is.null(api_key) || api_key == "") {
        api_key <- Sys.getenv("GEMINI_API_KEY")
      }
      if (is.null(api_key) || api_key == "") {
        stop("Gemini API key not found. Provide via gemini_spellcheck_key ",
             "parameter or set GEMINI_API_KEY environment variable.")
      }
      non_na_idx <- !is.na(unique_vals)
      non_na_vals <- unique_vals[non_na_idx]

      if (length(non_na_vals) > 0) {
        corrected_vals <- .spellcheck_gemini(
          texts = non_na_vals,
          api_key = api_key,
          model = gemini_spellcheck_model,
          context_prompt = gemini_spellcheck_context,
          verbose = verbose
        )
        result_sc <- unique_vals
        result_sc[non_na_idx] <- corrected_vals
      } else {
        result_sc <- unique_vals
      }
    } else {
      # ---- Hunspell spell-checking with edit distance + bigram context ----
      if (verbose) message("  Spell-checking (requires Java + WordNet)...")
      Sys.setenv(JAVA_HOME = java_home)
      wordnet::setDict(wordnet_dict)

      bigram_freqs <- NULL
      non_na_vals <- unique_vals[!is.na(unique_vals)]
      if (length(non_na_vals) >= 50) {
        if (verbose) message("    Building bigram frequencies from ",
                             length(non_na_vals), " unique values...")
        bigram_freqs <- .build_bigram_freqs(non_na_vals)
      }

      result_sc <- vector("character", length(unique_vals))
      for (i in seq_along(unique_vals)) {
        if (verbose && i %% 500 == 0) message("    Spell-checked ", i, " / ", length(unique_vals))
        result_sc[i] <- Spellcheck2(raw = unique_vals[i], rawlist = unique_vals,
                                     bigram_freqs = bigram_freqs)
      }
    }

    lookup_sc <- data.frame(tv = unique_vals, tv2 = result_sc, stringsAsFactors = FALSE)
    data <- dplyr::left_join(data, lookup_sc, by = "tv")
  } else {
    data$tv2 <- data$tv
  }

  # Step 3: Singularize on unique values of tv2
  if (singularize) {
    if (verbose) message("  Singularizing...")
    unique_vals2 <- unique(data$tv2)
    result_sg <- vector("character", length(unique_vals2))
    for (i in seq_along(unique_vals2)) {
      if (verbose && i %% 500 == 0) message("    Singularized ", i, " / ", length(unique_vals2))
      result_sg[i] <- singularize2(unique_vals2[i])
    }
    lookup_sg <- data.frame(tv2 = unique_vals2, tv3 = result_sg, stringsAsFactors = FALSE)
    data <- dplyr::left_join(data, lookup_sg, by = "tv2")
  } else {
    data$tv3 <- data$tv2
  }

  # Step 4: Replace errors with NA
  data$tv2 <- ifelse(data$tv2 == "*******ERROR", NA, data$tv2)
  data$tv3 <- ifelse(data$tv3 == "*******ERROR", NA, data$tv3)

  if (verbose) message("  Preprocessing complete. Columns added: tv, tv2, tv3")
  return(data)
}


# =============================================================================
# STAGE 2: score_valence()
# =============================================================================

#' Build a quanteda sentiment dictionary from tidytext
#'
#' @param name One of "nrc", "bing", "afinn", "loughran"
#' @return A quanteda dictionary with positive and negative entries
#' @keywords internal
build_sentiment_dictionary <- function(name) {
  cache_key <- paste0("sentiment_dictionary_", name)
  if (exists(cache_key, envir = .negation_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .negation_cache, inherits = FALSE))
  }

  if (name == "nrc") {
    sent <- tidytext::get_sentiments("nrc") %>%
      dplyr::filter(sentiment == "positive" | sentiment == "negative")
    pos_col <- paste0("positive_", name)
    neg_col <- paste0("negative_", name)
    sent[[pos_col]] <- ifelse(sent$sentiment == "positive", tolower(sent$word), NA)
    sent[[neg_col]] <- ifelse(sent$sentiment == "negative", tolower(sent$word), NA)
    sent <- dplyr::select(sent, dplyr::all_of(c(pos_col, neg_col)))
  } else if (name == "bing") {
    sent <- tidytext::get_sentiments("bing")
    pos_col <- paste0("positive_", name)
    neg_col <- paste0("negative_", name)
    sent[[pos_col]] <- ifelse(sent$sentiment == "positive", tolower(sent$word), NA)
    sent[[neg_col]] <- ifelse(sent$sentiment == "negative", tolower(sent$word), NA)
    sent <- dplyr::select(sent, dplyr::all_of(c(pos_col, neg_col)))
  } else if (name == "afinn") {
    sent <- tidytext::get_sentiments("afinn")
    pos_col <- paste0("positive_", name)
    neg_col <- paste0("negative_", name)
    sent[[pos_col]] <- ifelse(sent$value > 0, tolower(sent$word), NA)
    sent[[neg_col]] <- ifelse(sent$value < 0, tolower(sent$word), NA)
    sent <- dplyr::select(sent, dplyr::all_of(c(pos_col, neg_col)))
  } else if (name == "loughran") {
    sent <- tidytext::get_sentiments("loughran") %>%
      dplyr::filter(sentiment == "positive" | sentiment == "negative")
    pos_col <- paste0("positive_", name)
    neg_col <- paste0("negative_", name)
    sent[[pos_col]] <- ifelse(sent$sentiment == "positive", tolower(sent$word), NA)
    sent[[neg_col]] <- ifelse(sent$sentiment == "negative", tolower(sent$word), NA)
    sent <- dplyr::select(sent, dplyr::all_of(c(pos_col, neg_col)))
  }
  sent_list <- lapply(sent, function(x) x[!is.na(x)])
  out <- quanteda::dictionary(sent_list)
  assign(cache_key, out, envir = .negation_cache)
  out
}

#' Apply a single sentiment dictionary and compute valence scores
#'
#' @param toksval quanteda tokens object
#' @param dict_obj quanteda dictionary object
#' @param name Dictionary name (for column naming)
#' @param is_lexicoder If TRUE, handle neg_positive and neg_negative sub-categories
#' @return Data frame with Val and ValNA columns for this dictionary
#' @keywords internal
apply_single_valence_dict <- function(toksval, dict_obj, name, is_lexicoder = FALSE) {
  toks_lookup <- quanteda::tokens_lookup(toksval, dict_obj,
                                         nested_scope = "dictionary", exclusive = TRUE)
  result <- quanteda::convert(quanteda::dfm(toks_lookup), to = "data.frame")

  if (is_lexicoder) {
    result$negative <- result$negative + result$neg_positive
    result$positive <- result$positive + result$neg_negative
  }

  # Binarize
  neg_col <- if (is_lexicoder) "negative" else paste0("negative_", name)
  pos_col <- if (is_lexicoder) "positive" else paste0("positive_", name)

  neg_binary <- ifelse(result[[neg_col]] > 0, 1, 0)
  pos_binary <- ifelse(result[[pos_col]] > 0, 1, 0)

  val_name <- paste0("Val_", name)
  valna_name <- paste0("Val_", name, "NA")

  out <- data.frame(
    val = pos_binary - neg_binary,
    valna = ifelse(pos_binary + neg_binary == 0, NA, pos_binary - neg_binary)
  )
  colnames(out) <- c(val_name, valna_name)
  out
}

#' Score text responses using 5 sentiment dictionaries
#'
#' Applies Lexicoder (LSD2015), NRC, Bing, AFINN, and Loughran sentiment
#' dictionaries. Produces per-dictionary valence scores and a combined score.
#'
#' @param data A data.frame with text to score
#' @param text_col Column containing cleaned text for valence scoring (default "tv").
#'   Uses tv (not tv2/tv3) to preserve original response for LLMs with low spelling errors.
#'   Change to "tv2" for human participants.
#' @param response_col Column with original response for negation detection (default "response")
#' @return The input data with 12 new columns: Val_lexicoder, Val_NRC, Val_bing,
#'   Val_affin, Val_loughran, their NA variants, Valence, and ValenceNoNA
#' @export
score_valence <- function(data,
                          text_col = "tv",
                          response_col = NULL) {
  if (is.null(response_col)) response_col <- text_col
  message("--- Stage 2: Scoring valence (5 dictionaries) ---")

  # Deduplicate: score unique texts only, then map back
  texts <- data[[text_col]]
  uniq_mask <- !duplicated(texts)
  uniq_texts <- texts[uniq_mask]
  n_uniq <- length(uniq_texts)
  n_total <- length(texts)

  if (n_uniq < n_total) {
    message("  Deduplicating: ", n_total, " rows -> ", n_uniq, " unique texts")
    uniq_scores <- .score_valence_from_scoped_tokens(uniq_texts)
    map_idx <- match(texts, uniq_texts)
    valence_scores <- uniq_scores[map_idx, , drop = FALSE]
    rownames(valence_scores) <- NULL
  } else {
    valence_scores <- .score_valence_from_scoped_tokens(texts)
  }

  # Separate internal negation-aware columns from output columns
  neg_valna_cols <- grep("^\\.neg_valna_", names(valence_scores), value = TRUE)
  neg_valna_avg <- rowMeans(valence_scores[, neg_valna_cols, drop = FALSE], na.rm = TRUE)

  # Add only the raw (non-negation) individual dictionary columns to output
  data <- cbind(data, valence_scores[, !names(valence_scores) %in% neg_valna_cols, drop = FALSE])

  # Combined valence (negation-aware, applied once to the average)
  # Valence: NA when no dictionary matched any sentiment words
  # ValenceNoNA: 0 instead of NA
  data$Valence <- neg_valna_avg
  data$ValenceNoNA <- ifelse(is.nan(neg_valna_avg), 0, neg_valna_avg)

  val_cols <- c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")
  valna_cols <- c("Val_lexicoderNA", "Val_NRCNA", "Val_bingNA", "Val_affinNA", "Val_loughranNA")

  # Ensure valence family columns are NA when the original response is missing
  data <- .mask_missing_response_cols(
    data,
    response_col,
    cols_or_patterns = c("^Val_", "^Valence$", "^ValenceNoNA$")
  )

  # Replace NaN with NA
  data <- replace_nan_with_na(data)

  message("  Valence scoring complete. Columns added: ",
          paste(c(val_cols, valna_cols, "Valence", "ValenceNoNA"), collapse = ", "))
  return(data)
}


# =============================================================================
# STAGE 3: prepare_sadcat_dictionaries()
# =============================================================================

#' Prepare SADCAT dictionaries for quanteda matching
#'
#' Transforms \code{SADCAT::All.steps_Dictionaries} into a quanteda dictionary
#' object. The result is cached in a package-level environment so it only needs
#' to be computed once per R session.
#'
#' @param pre_dictionaries The raw SADCAT dictionary data (default: SADCAT::All.steps_Dictionaries)
#' @param cache Logical. Cache result for session? (default TRUE)
#' @return A quanteda::dictionary object with ~33 categories
#' @export
prepare_sadcat_dictionaries <- function(pre_dictionaries = SADCAT::All.steps_Dictionaries,
                                        cache = TRUE) {
  # Return cached version if available
  if (cache && exists("prepared_dict", envir = .sadcat_cache)) {
    message("  Using cached SADCAT dictionaries.")
    return(get("prepared_dict", envir = .sadcat_cache))
  }

  message("--- Stage 3: Preparing SADCAT dictionaries ---")

  Pre_Dictionaries <- pre_dictionaries

  # Add valence-conditioned columns
  Pre_Dictionaries$relative_dict_val <- ifelse(Pre_Dictionaries$relative_dict == 0, NA, Pre_Dictionaries$Val)
  Pre_Dictionaries$beauty_dict_val <- ifelse(Pre_Dictionaries$beauty_dict == 0, NA, Pre_Dictionaries$Val)

  # Prepare values0: lowercase, UTF-8, dash-to-space, remove ending Ss, remove punctuation
  Pre_Dictionaries$values0 <- enc2utf8(as.character(Pre_Dictionaries$tv))
  Pre_Dictionaries$values0 <- tolower(Pre_Dictionaries$values0)
  Pre_Dictionaries$values0 <- gsub("-", " ", Pre_Dictionaries$values0)

  # Remove ending Ss (word by word)
  delete_ending_Ss2_internal <- function(x) {
    if (is.na(x)) return(x)
    unlist(lapply(x, function(y) {
      paste(sapply(strsplit(y, ' '), SADCAT::delete_ending_Ss), collapse = ' ')
    }))
  }
  Pre_Dictionaries$values0 <- vapply(Pre_Dictionaries$values0,
                                     delete_ending_Ss2_internal,
                                     character(1),
                                     USE.NAMES = FALSE)

  # Tokenize to remove punctuation and symbols, then reconstruct
  corpusx2 <- .tokenize_quanteda_text(Pre_Dictionaries$values0,
                                      prefix = "sadcat_term",
                                      remove_numbers = FALSE,
                                      remove_punct = TRUE,
                                      remove_symbols = TRUE)
  Pre_Dictionaries$values0 <- vapply(seq_along(corpusx2), function(i) {
    paste(corpusx2[[i]], collapse = ' ')
  }, character(1))

  # ---- Build dictionary mapping ----
  # Each category maps dict_column -> values0 where the column == 1

  # Helper: extract values0 where a column is 1
  extract_words <- function(col_name) {
    mask <- Pre_Dictionaries[[col_name]] == 1
    mask[is.na(mask)] <- FALSE
    vals <- Pre_Dictionaries$values0[mask]
    vals[!is.na(vals) & vals != ""]
  }

  dict_list <- list()

  # Base dimensions
  base_map <- c(
    Sociability_dic = "Sociability_dict",
    Morality_dic = "Morality_dict",
    Ability_dic = "Ability_dict",
    Assertiveness_dic = "Assertiveness_dict",
    Status_dic = "Status_dict",
    Warmth_dic = "Warmth_dict",
    Competence_dic = "Competence_dict",
    Beliefs_dic = "Beliefs_dict",
    Beauty_dic = "beauty_dict"
  )
  for (nm in names(base_map)) {
    dict_list[[nm]] <- extract_words(base_map[[nm]])
  }

  # High variants
  hi_map <- c(
    Sociability_dic_hi = "Sociability_dict_hi",
    Morality_dic_hi = "Morality_dict_hi",
    Ability_dic_hi = "Ability_dict_hi",
    Assertiveness_dic_hi = "Assertiveness_dict_hi",
    Status_dic_hi = "Status_dict_hi",
    Warmth_dic_hi = "Warmth_dict_hi",
    Competence_dic_hi = "Competence_dict_hi",
    Beliefs_dic_hi = "Beliefs_dict_hi",
    Health_dic_hi = "Health_dict_hi",
    Beauty_dic_hi = "Beauty_dict_hi",
    Deviance_dic_hi = "Deviance_dict_hi"
  )
  for (nm in names(hi_map)) {
    dict_list[[nm]] <- extract_words(hi_map[[nm]])
  }

  # Low variants
  lo_map <- c(
    Sociability_dic_lo = "Sociability_dict_lo",
    Morality_dic_lo = "Morality_dict_lo",
    Ability_dic_lo = "Ability_dict_lo",
    Assertiveness_dic_lo = "Assertiveness_dict_lo",
    Status_dic_lo = "Status_dict_lo",
    Warmth_dic_lo = "Warmth_dict_lo",
    Competence_dic_lo = "Competence_dict_lo",
    Beliefs_dic_lo = "Beliefs_dict_lo",
    Health_dic_lo = "Health_dict_lo",
    Beauty_dic_lo = "Beauty_dict_lo",
    Deviance_dic_lo = "Deviance_dict_lo"
  )
  for (nm in names(lo_map)) {
    dict_list[[nm]] <- extract_words(lo_map[[nm]])
  }

  # Non-directional dimensions
  ndir_map <- c(
    Health_dic = "health_dict",
    Family_dic = "relative_dict",
    Emotion_dic = "emotions_dict",
    Deviance_dic = "deviance_dict",
    Socialgroups_dic = "socialgroups_dict",
    Geography_dic = "Geography_dict",
    Occupation_dic = "occupation_dict",
    Other_dic = "Other_dict",
    Appearance_dic = "Appearance_dict"
  )
  for (nm in names(ndir_map)) {
    dict_list[[nm]] <- extract_words(ndir_map[[nm]])
  }

  # OtherwFam: Other OR relative
  mask_owf <- (Pre_Dictionaries$Other_dict == 1 | Pre_Dictionaries$relative_dict == 1)
  mask_owf[is.na(mask_owf)] <- FALSE
  dict_list[["OtherwFam_dic"]] <- Pre_Dictionaries$values0[mask_owf]
  dict_list[["OtherwFam_dic"]] <- dict_list[["OtherwFam_dic"]][
    !is.na(dict_list[["OtherwFam_dic"]]) & dict_list[["OtherwFam_dic"]] != ""]

  # Create quanteda dictionary
  result <- quanteda::dictionary(dict_list)

  # Cache
  if (cache) {
    assign("prepared_dict", result, envir = .sadcat_cache)
  }

  message("  Dictionary preparation complete. ", length(dict_list), " categories.")
  return(result)
}


# =============================================================================
# STAGE 4: match_dictionaries()
# =============================================================================

#' Match text responses against SADCAT dictionaries
#'
#' Tokenizes response text, matches against SADCAT stereotype dictionaries,
#' and computes binary indicators, percentages, direction scores, and
#' per-dimension valence/direction columns.
#'
#' @param data A data.frame with preprocessed text and valence scores
#' @param text_col Column with singularized text to match (default "tv3")
#' @param response_col Column with original lowercased text for NA checks (default "tv")
#' @param valence_col Name of combined valence column (default "Valence")
#' @param sadcat_dict Pre-computed quanteda dictionary. If NULL, calls prepare_sadcat_dictionaries()
#' @return The input data with many new columns: dictionary counts, percentages,
#'   binary indicators, direction scores, per-dimension valence, etc.
#' @export
match_dictionaries <- function(data,
                               text_col = "tv3",
                               response_col = NULL,
                               valence_col = "Valence",
                               valence_nona_col = "ValenceNoNA",
                               sadcat_dict = NULL) {
  if (is.null(response_col)) response_col <- text_col
  message("--- Stage 4: Matching dictionaries ---")

  # Get or prepare dictionaries
  if (is.null(sadcat_dict)) {
    sadcat_dict <- prepare_sadcat_dictionaries()
  }

  # ---- Prepare text for matching (tv3 -> tv4) ----
  data <- enc2utf8(as.character(data[[text_col]]))
  data <- tolower(data)
  data <- gsub("-", " ", data)

  # Remove ending Ss
  delete_ending_Ss2_internal <- function(x) {
    if (is.na(x)) return(x)
    unlist(lapply(x, function(y) {
      paste(sapply(strsplit(y, ' '), SADCAT::delete_ending_Ss), collapse = ' ')
    }))
  }
  data <- vapply(data, delete_ending_Ss2_internal, character(1), USE.NAMES = FALSE)

  # ---- Tokenize and match ----
  toks <- .tokenize_quanteda_text(data,
                                  prefix = "response",
                                  remove_numbers = FALSE,
                                  remove_punct = TRUE,
                                  remove_symbols = TRUE)

  toks_dict_pre <- quanteda::tokens_lookup(toks, dictionary = sadcat_dict,
                                           nested_scope = "dictionary",
                                           exclusive = TRUE, levels = 1)
  toks_dict_df <- quanteda::convert(quanteda::dfm(toks_dict_pre), to = "data.frame")

  # Combine with token count and original data
  toks_dict_df <- quanteda::ntoken(toks)
  toks_dict <- cbind(toks_dict_df, data)

  # ---- Identify dictionary column names (lowercase from quanteda output) ----
  dict_cols_all <- tolower(names(sadcat_dict))

  # ---- Percentages ----
  for (col in dict_cols_all) {
    toks_dict[[paste0(col, "_percent")]] <- toks_dict[[col]] / toks_dict
  }

  # ---- Binary indicators ----
  for (col in dict_cols_all) {
    toks_dict[[paste0(col, "_binary")]] <- ifelse(toks_dict[[col]] > 0, 1, 0)
  }

  # ---- Direction scores (hi - lo for directional dimensions) ----
  for (dim in .SADCAT_DIR_DIMS) {
    hi_col <- paste0(tolower(dim), "_dic_hi_binary")
    lo_col <- paste0(tolower(dim), "_dic_lo_binary")
    dirx_col <- paste0(dim, "_dirx")
    if (hi_col %in% names(toks_dict) && lo_col %in% names(toks_dict)) {
      toks_dict[[dirx_col]] <- toks_dict[[hi_col]] - toks_dict[[lo_col]]
    }
  }

  # ---- Binary2: NA if response is NA ----
  all_base_dims <- c(.SADCAT_DIR_DIMS, .SADCAT_NDIR_DIMS)
  for (dim in all_base_dims) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    binary2_col <- paste0(tolower(dim), "_dic_binary2")
    if (binary_col %in% names(toks_dict)) {
      toks_dict[[binary2_col]] <- ifelse(is.na(toks_dict[[response_col]]),
                                         NA, toks_dict[[binary_col]])
    }
  }

  # ---- None2y: no dictionary match ----
  binary2_cols <- grep("_dic_binary2$", names(toks_dict), value = TRUE)
  if (length(binary2_cols) > 0) {
    row_sums <- rowSums(toks_dict[, binary2_cols, drop = FALSE], na.rm = FALSE)
    toks_dict <- ifelse(is.na(row_sums), NA, ifelse(row_sums > 0, 0, 1))
  }

  # ---- Per-dimension Valy / ValyNoNA ----
  for (dim in all_base_dims) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    valy_dim <- paste0(dim, "_Valy")
    valyno_dim <- paste0(dim, "_ValyNoNA")

    if (binary_col %in% names(toks_dict) && valence_col %in% names(toks_dict)) {
      toks_dict[[valy_dim]] <- ifelse(toks_dict[[binary_col]] == 0,
                                      NA, toks_dict[[valence_col]])
    }
    if (binary_col %in% names(toks_dict) && valence_nona_col %in% names(toks_dict)) {
      toks_dict[[valyno_dim]] <- ifelse(toks_dict[[binary_col]] == 0,
                                        0, toks_dict[[valence_nona_col]])
    }
  }

  # NONE Valy
  if ("None2y" %in% names(toks_dict) && valence_col %in% names(toks_dict)) {
    toks_dict$NONE_Valy <- ifelse(toks_dict$None2y == 0, NA, toks_dict[[valence_col]])
  }

  adjusted_dir <- .compute_adjusted_direction_scores(data[[text_col]], sadcat_dict)
  for (col in names(adjusted_dir)) {
    toks_dict[[col]] <- adjusted_dir[[col]]
  }

  # ---- Fix valy3/valyNoNA3 and dirx3: NA if binary==0 OR binary2 is NA ----
  # Directional dimensions: valy3, valyNoNA3, and dirx3
  for (dim in .SADCAT_DIR_DIMS) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    binary2_col <- paste0(tolower(dim), "_dic_binary2")
    valy_dim <- paste0(dim, "_Valy")
    valyno_dim <- paste0(dim, "_ValyNoNA")
    valy3_col <- paste0(dim, "_valy3")
    valyno3_col <- paste0(dim, "_valyNoNA3")
    dirx_col <- paste0(dim, "_dirx")
    dirx2_col <- paste0(dim, "_dirx2")
    dirx3_col <- paste0(dim, "_dirx3")

    if (all(c(binary_col, binary2_col, valy_dim) %in% names(toks_dict))) {
      toks_dict[[valy3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valy_dim]])
    }
    if (all(c(binary_col, binary2_col, valyno_dim) %in% names(toks_dict))) {
      toks_dict[[valyno3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valyno_dim]])
    }
    if (all(c(binary_col, binary2_col, dirx_col) %in% names(toks_dict))) {
      toks_dict[[dirx3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[dirx_col]])
    }
  }

  # Non-directional dimensions: valy3 and valyNoNA3
  ndir_for_valy3 <- c("Occupation", "Emotion", "Deviance", "Socialgroups",
                       "Geography", "Appearance", "Other", "OtherwFam")
  for (dim in ndir_for_valy3) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    binary2_col <- paste0(tolower(dim), "_dic_binary2")
    valy_dim <- paste0(dim, "_Valy")
    valyno_dim <- paste0(dim, "_ValyNoNA")
    valy3_col <- paste0(dim, "_valy3")
    valyno3_col <- paste0(dim, "_valyNoNA3")

    if (all(c(binary_col, binary2_col, valy_dim) %in% names(toks_dict))) {
      toks_dict[[valy3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valy_dim]])
    }
    if (all(c(binary_col, binary2_col, valyno_dim) %in% names(toks_dict))) {
      toks_dict[[valyno3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valyno_dim]])
    }
  }

  # Ensure dictionary-derived valence/direction families are NA when response is missing
  toks_dict <- .mask_missing_response_cols(
    toks_dict,
    response_col,
    cols_or_patterns = c(
      "_Valy$", "_ValyNoNA$", "_valy3$", "_valyNoNA3$",
      "_dirx$", "_dirx2$", "_dirx3$"
    )
  )

  # Replace NaN with NA
  toks_dict <- replace_nan_with_na(toks_dict)

  message("  Dictionary matching complete.")
  return(toks_dict)
}


# =============================================================================
# STAGE 5: compute_embeddings()
# =============================================================================

#' Compute sentence embeddings using SBERT and/or Gemini via reticulate
#'
#' Generates embeddings for unique text values and joins them back to the data.
#' SBERT uses the sentence-transformers Python library; Gemini uses the Google
#' genai Python library with batched API calls and rate limiting.
#'
#' @param data A data.frame with a text column
#' @param text_col Column containing text to encode (default "tv")
#' @param methods Character vector: one or both of "sbert" and "gemini" (default "sbert")
#' @param sbert_model SBERT model name (default "paraphrase-mpnet-base-v2")
#' @param sbert_dims Number of dimensions in SBERT output (default 768)
#' @param gemini_api_key Gemini API key. If NULL, reads from Sys.getenv("GEMINI_API_KEY")
#' @param gemini_model Gemini model name (default "gemini-embedding-exp-03-07")
#' @param gemini_dims Embedding dimensionality for Gemini (default 2000)
#' @param gemini_batch_size Batch size for Gemini API calls (default 10)
#' @param gemini_sleep Seconds to sleep between Gemini batches (default 63)
#' @param gemini_task_type Gemini task type (default "SEMANTIC_SIMILARITY")
#' @param response_col Column used to set embeddings to NA where response is missing (default "response")
#' @param verbose Print progress? (default TRUE)
#' @return The input data with embedding columns appended (SBERT_1:SBERT_N and/or Gemini_1:Gemini_N)
#' @export
compute_embeddings <- function(data,
                               text_col = "tv",
                               methods = c("sbert"),
                               sbert_model = "paraphrase-mpnet-base-v2",
                               sbert_dims = 768L,
                               gemini_api_key = NULL,
                               gemini_model = "gemini-embedding-exp-03-07",
                               gemini_dims = 2000L,
                               gemini_batch_size = 10L,
                               gemini_sleep = 63,
                               gemini_task_type = "SEMANTIC_SIMILARITY",
                               response_col = NULL,
                               verbose = TRUE) {
  if (is.null(response_col)) response_col <- text_col

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for compute_embeddings(). Install with: install.packages('reticulate')")
  }

  message("--- Stage 5: Computing embeddings ---")

  # Get unique non-NA text values
  unique_text <- unique(data[[text_col]])
  unique_text <- unique_text[!is.na(unique_text) & unique_text != ""]
  if (verbose) message("  Unique text values to encode: ", length(unique_text))

  # ---- SBERT ----
  if ("sbert" %in% tolower(methods)) {
    if (verbose) message("  Computing SBERT embeddings (model: ", sbert_model, ")...")

    st <- reticulate::import("sentence_transformers")
    model_sbert <- st$SentenceTransformer(sbert_model)

    encoding_matrix <- model_sbert$encode(unique_text)
    encoding_df <- as.data.frame(reticulate::py_to_r(encoding_matrix))
    colnames(encoding_df) <- paste0("SBERT_", seq_len(ncol(encoding_df)))

    sbert_vecs <- data.frame(word = unique_text, encoding_df, stringsAsFactors = FALSE)

    # Merge with data
    join_by <- stats::setNames("word", text_col)
    data <- dplyr::left_join(data, sbert_vecs, by = join_by)

    # Set embeddings to NA where response is NA
    sbert_cols <- paste0("SBERT_", seq_len(ncol(encoding_df)))
    data <- .mask_missing_response_cols(
      data,
      response_col,
      cols_or_patterns = c("^SBERT_\\d+$")
    )

    if (verbose) message("  SBERT complete. Added ", length(sbert_cols), " columns.")
  }

  # ---- Gemini ----
  if ("gemini" %in% tolower(methods)) {
    # Resolve API key
    api_key <- gemini_api_key
    if (is.null(api_key) || api_key == "") {
      api_key <- Sys.getenv("GEMINI_API_KEY")
    }
    if (is.null(api_key) || api_key == "") {
      stop("Gemini API key not found. Provide via gemini_api_key parameter or set GEMINI_API_KEY environment variable.")
    }

    if (verbose) message("  Computing Gemini embeddings (model: ", gemini_model,
                         ", dims: ", gemini_dims, ")...")

    genai <- reticulate::import("google.genai")
    types <- reticulate::import("google.genai.types")

    client <- genai$Client(api_key = api_key)

    config <- types$EmbedContentConfig(
      output_dimensionality = as.integer(gemini_dims),
      task_type = gemini_task_type
    )

    text_list <- as.list(unique_text)
    all_embeddings <- list()
    n_batches <- ceiling(length(text_list) / gemini_batch_size)

    for (i in seq(1, length(text_list), by = gemini_batch_size)) {
      batch_num <- ceiling(i / gemini_batch_size)
      batch_end <- min(i + gemini_batch_size - 1, length(text_list))
      batch <- text_list[i:batch_end]

      if (verbose) message("    Batch ", batch_num, " / ", n_batches,
                           " (items ", i, "-", batch_end, ")")

      result <- tryCatch({
        client$models$embed_content(
          model = gemini_model,
          contents = batch,
          config = config
        )
      }, error = function(e) {
        warning("  Batch ", batch_num, " failed: ", conditionMessage(e))
        NULL
      })

      if (!is.null(result)) {
        all_embeddings <- c(all_embeddings, list(result))
      }

      # Sleep to avoid rate limit (skip after last batch)
      if (batch_end < length(text_list)) {
        if (verbose) message("    Sleeping ", gemini_sleep, " seconds (rate limit)...")
        Sys.sleep(gemini_sleep)
      }
    }

    # Flatten embeddings
    flat_embeddings <- list()
    for (batch_result in all_embeddings) {
      for (emb in batch_result$embeddings) {
        flat_embeddings <- c(flat_embeddings, list(emb$values))
      }
    }

    # Build data frame
    gemini_matrix <- do.call(rbind, flat_embeddings)
    gemini_df <- as.data.frame(gemini_matrix)
    colnames(gemini_df) <- paste0("Gemini_", seq_len(ncol(gemini_df)))

    gemini_vecs <- data.frame(word = unique_text[seq_len(nrow(gemini_df))],
                              gemini_df, stringsAsFactors = FALSE)

    # Merge with data
    join_by <- stats::setNames("word", text_col)
    data <- dplyr::left_join(data, gemini_vecs, by = join_by)

    # Set embeddings to NA where response is NA
    gemini_cols <- paste0("Gemini_", seq_len(ncol(gemini_df)))
    data <- .mask_missing_response_cols(
      data,
      response_col,
      cols_or_patterns = c("^Gemini_\\d+$")
    )

    if (verbose) message("  Gemini complete. Added ", length(gemini_cols), " columns.")
  }

  message("  Embedding computation complete.")
  return(data)
}


# =============================================================================
# STAGE 6: compute_seed_similarities()
# =============================================================================

#' Compute similarity between response embeddings and SADCAT seed vectors
#'
#' For each response, computes the correlation (or cosine similarity) between
#' its embedding vector and each SADCAT seed vector. Works for any embedding
#' prefix (SBERT, Gemini, etc.) as long as matching columns exist in both the
#' data and seed vectors.
#'
#' @param data A data.frame with embedding columns (e.g., SBERT_1:SBERT_768)
#' @param embedding_prefix Prefix identifying embedding columns (default "SBERT")
#' @param seed_vectors Seed vector data. Default: SADCAT::Seed_Vectors_Avg
#' @param method "correlation" (default, uses cor()) or "cosine" (uses lsa::cosine())
#' @param verbose Print progress? (default TRUE)
#' @return The input data with new columns: {prefix}_{SeedName}.seed
#' @export
compute_seed_similarities <- function(data,
                                      embedding_prefix = "SBERT",
                                      seed_vectors = SADCAT::Seed_Vectors_Avg,
                                      method = "correlation",
                                      response_col = NULL,
                                      verbose = TRUE) {
  message("--- Stage 6: Computing seed similarities (", embedding_prefix, ") ---")

  # Check if seed vectors have columns matching the prefix
  seed_emb_cols <- grep(paste0("^", embedding_prefix, "_"), names(seed_vectors), value = TRUE)
  if (length(seed_emb_cols) == 0) {
    message("  No columns matching prefix '", embedding_prefix,
            "' found in seed_vectors. Skipping seed similarity computation.")
    return(data)
  }

  # Check if data has columns matching the prefix
  data_emb_cols <- grep(paste0("^", embedding_prefix, "_\\d+$"), names(data), value = TRUE)
  if (length(data_emb_cols) == 0) {
    message("  No embedding columns matching prefix '", embedding_prefix,
            "' found in data. Skipping seed similarity computation.")
    return(data)
  }

  # Extract seed info
  Seed_Vectors_names <- paste0(as.character(seed_vectors), ".seed")

  # Extract seed embedding matrix (dims x seeds)
  Seed_Matrix <- seed_vectors %>%
    dplyr::select(dplyr::all_of(seed_emb_cols)) %>%
    t()

  colnames(Seed_Matrix) <- Seed_Vectors_names

  if (verbose) message("  Computing ", method, " between ",
                       length(data_emb_cols), " dims and ",
                       ncol(Seed_Matrix), " seed vectors...")

  # Extract response embedding matrix
  response_matrix <- data[, data_emb_cols, drop = FALSE]

  # Compute similarities row-by-row
  if (method == "correlation") {
    sim_matrix <- t(apply(response_matrix, 1, function(z) {
      cor(as.numeric(z), Seed_Matrix)
    }))
  } else if (method == "cosine") {
    sim_matrix <- t(apply(response_matrix, 1, function(z) {
      cosinize(z, Seed_Matrix)
    }))
  } else {
    stop("method must be 'correlation' or 'cosine'")
  }

  # Name columns
  colnames(sim_matrix) <- paste0(embedding_prefix, "_", Seed_Vectors_names)
  sim_df <- as.data.frame(sim_matrix)

  # Bind to data
  data <- cbind(data, sim_df)

  # Ensure seed columns are NA when the original response is missing
  data <- .mask_missing_response_cols(
    data,
    response_col,
    cols_or_patterns = names(sim_df)
  )

  if (verbose) message("  Seed similarities complete. Added ", ncol(sim_df), " columns.")
  return(data)
}


# =============================================================================
# STAGE 7: aggregate_responses()
# =============================================================================

#' Aggregate response-level data to group-level
#'
#' Performs three types of aggregation (sum, mean, distinct) and joins them.
#' Auto-detects standard pipeline columns by pattern; user can add extra
#' columns via parameters. Optionally creates noNA versions of direction
#' and valence columns (replacing NA with 0).
#'
#' @param data The full long-format data.frame from the pipeline
#' @param group_cols Character vector of grouping columns (default c("Synonym.GroupX", "Group", "Level"))
#' @param sum_cols Columns to sum-aggregate. If NULL, auto-detects _binary2, None2y, traditional
#' @param mean_cols Columns to mean-aggregate. If NULL, auto-detects standard patterns
#' @param distinct_cols Columns to preserve by distinct. If NULL, auto-detects Warning columns
#' @param extra_sum_cols Additional columns to include in sum aggregation
#' @param extra_mean_cols Additional columns to include in mean aggregation
#' @param extra_distinct_cols Additional columns to include in distinct aggregation
#' @param create_noNA Logical. Create *noNA versions of direction/valence? (default TRUE)
#' @param verbose Print progress? (default TRUE)
#' @return A data.frame with one row per unique combination of group_cols
#' @export
aggregate_responses <- function(data,
                                group_cols = c("Synonym.GroupX", "Group", "Level"),
                                sum_cols = NULL,
                                mean_cols = NULL,
                                distinct_cols = NULL,
                                extra_sum_cols = NULL,
                                extra_mean_cols = NULL,
                                extra_distinct_cols = NULL,
                                create_noNA = TRUE,
                                verbose = TRUE) {
  message("--- Stage 7: Aggregating responses ---")

  all_names <- names(data)

  # ---- Auto-detect columns ----
  if (is.null(sum_cols)) {
    sum_cols <- grep("_dic_binary2$|^None2y$|^traditional$", all_names, value = TRUE)
  }
  if (!is.null(extra_sum_cols)) {
    extra_sum_cols <- extra_sum_cols[extra_sum_cols %in% all_names]
    sum_cols <- unique(c(sum_cols, extra_sum_cols))
  }

  if (is.null(mean_cols)) {
    mean_cols <- grep(
      paste0("^Valence$|^ValenceNoNA$|",
             "_dic_binary2$|^None2y$|",
             "_ValyNoNA$|^NONE_Valy$|",
             "_valy3$|_valyNoNA3$|_dirx3$|",
             "^SBERT_|^Gemini_|",
             "\\.seed$|",
             "^traditional$"),
      all_names, value = TRUE
    )
  }
  if (!is.null(extra_mean_cols)) {
    extra_mean_cols <- extra_mean_cols[extra_mean_cols %in% all_names]
    mean_cols <- unique(c(mean_cols, extra_mean_cols))
  }

  if (is.null(distinct_cols)) {
    distinct_cols <- grep("Warning$|^Level$", all_names, value = TRUE)
    # Remove Level if it's already in group_cols
    distinct_cols <- setdiff(distinct_cols, group_cols)
  }
  if (!is.null(extra_distinct_cols)) {
    extra_distinct_cols <- extra_distinct_cols[extra_distinct_cols %in% all_names]
    distinct_cols <- unique(c(distinct_cols, extra_distinct_cols))
  }

  # Ensure group_cols exist
  missing_groups <- setdiff(group_cols, all_names)
  if (length(missing_groups) > 0) {
    stop("Group columns not found in data: ", paste(missing_groups, collapse = ", "))
  }

  if (verbose) {
    message("  Sum columns: ", length(sum_cols))
    message("  Mean columns: ", length(mean_cols))
    message("  Distinct columns: ", length(distinct_cols))
  }

  # ---- Sum aggregation ----
  if (length(sum_cols) > 0) {
    sum_data <- data %>%
      dplyr::select(dplyr::all_of(c(group_cols, sum_cols))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)),
                       .groups = "drop")
    # Rename sum columns with _Sum suffix
    sum_rename <- setdiff(names(sum_data), group_cols)
    names(sum_data)[names(sum_data) %in% sum_rename] <- paste0(sum_rename, "_Sum")
  } else {
    sum_data <- NULL
  }

  # ---- Mean aggregation ----
  if (length(mean_cols) > 0) {
    mean_data <- data %>%
      dplyr::select(dplyr::all_of(c(group_cols, mean_cols))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~mean(., na.rm = TRUE)),
                       .groups = "drop")
  } else {
    mean_data <- NULL
  }

  # ---- Distinct aggregation ----
  if (length(distinct_cols) > 0) {
    distinct_data <- data %>%
      dplyr::select(dplyr::all_of(c(group_cols, distinct_cols))) %>%
      dplyr::distinct()
  } else {
    distinct_data <- NULL
  }

  # ---- Join all ----
  parts <- Filter(Negate(is.null), list(distinct_data, mean_data, sum_data))
  if (length(parts) == 0) {
    stop("No columns selected for aggregation.")
  }
  result <- plyr::join_all(parts, by = group_cols, type = "left")

  # Replace NaN with NA
  result <- replace_nan_with_na(result)

  # ---- Create noNA versions ----
  if (create_noNA) {
    # Direction noNA: _dirx3 columns
    dirx3_cols <- grep("_dirx3$", names(result), value = TRUE)
    for (col in dirx3_cols) {
      result[[paste0(col, "noNA")]] <- ifelse(is.na(result[[col]]), 0, result[[col]])
    }

    # Valence noNA: _valy3 and _valyNoNA3 columns
    valy_cols <- grep("_valy3$|_valyNoNA3$", names(result), value = TRUE)
    for (col in valy_cols) {
      result[[paste0(col, "noNA")]] <- ifelse(is.na(result[[col]]), 0, result[[col]])
    }

    if (verbose) {
      message("  Created ", length(dirx3_cols) + length(valy_cols), " noNA columns.")
    }
  }

  if (verbose) message("  Aggregation complete. Result: ",
                       nrow(result), " rows x ", ncol(result), " columns.")
  return(result)
}


# =============================================================================
# MASTER FUNCTION: process_responses()
# =============================================================================

#' Run the full SADCAT LLM response processing pipeline
#'
#' Chains all pipeline stages in sequence: text preprocessing, valence scoring,
#' dictionary matching, embedding computation, seed similarity computation,
#' and aggregation. Each stage can be skipped via the \code{stages} parameter.
#'
#' @param data A data.frame with raw LLM responses
#' @param text_col Column with raw text responses (default "responsex")
#' @param response_col Column with original response for negation/NA checks (default "response")
#' @param group_cols Grouping columns for aggregation (default c("Synonym.GroupX", "Group", "Level"))
#' @param stages Character vector of stages to run. Default: all stages.
#'   Options: "preprocess", "valence", "dictionaries", "embeddings", "seeds", "aggregate"
#' @param spellcheck Run spell-checking in preprocessing? (default TRUE)
#' @param singularize_text Run singularization in preprocessing? (default TRUE)
#' @param java_home Path to Java JRE for spell-checking
#' @param wordnet_dict Path to WordNet dictionary files
#' @param valence_text_col Column to use for valence scoring (default "tv").
#'   Use "tv" for LLMs (preserve original), "tv2" for human participants.
#' @param embedding_methods Embedding methods: "sbert", "gemini", or both (default "sbert")
#' @param sbert_model SBERT model name (default "paraphrase-mpnet-base-v2")
#' @param gemini_api_key Gemini API key. If NULL, reads from environment variable
#' @param gemini_dims Gemini embedding dimensionality (default 2000)
#' @param gemini_batch_size Gemini batch size (default 10)
#' @param gemini_sleep Seconds between Gemini batches (default 63)
#' @param seed_method Seed similarity method: "correlation" or "cosine" (default "correlation")
#' @param extra_sum_cols Extra columns for sum aggregation
#' @param extra_mean_cols Extra columns for mean aggregation
#' @param extra_distinct_cols Extra columns for distinct aggregation
#' @param save_intermediates Save CSVs after each stage? (default FALSE)
#' @param save_prefix Prefix for intermediate CSV filenames (default "pipeline")
#' @param verbose Print progress? (default TRUE)
#' @return A named list with \code{$long} (full long-format data) and
#'   \code{$agg} (aggregated data, if "aggregate" stage was run)
#' @export
process_responses <- function(data,
                              text_col = "responsex",
                              response_col = NULL,
                              group_cols = c("Synonym.GroupX", "Group", "Level"),
                              stages = c("preprocess", "valence", "dictionaries",
                                         "embeddings", "seeds", "aggregate"),
                              # Preprocessing params
                              spellcheck = TRUE,
                              spellcheck_method = "hunspell",
                              singularize_text = TRUE,
                              java_home = "C:\\Program Files\\Java\\jre-1.8",
                              wordnet_dict = "C:\\dict",
                              gemini_spellcheck_key = NULL,
                              gemini_spellcheck_model = "gemini-2.0-flash",
                              gemini_spellcheck_context = NULL,
                              # Valence params
                              valence_text_col = "tv",
                              # Embedding params
                              embedding_methods = c("sbert"),
                              sbert_model = "paraphrase-mpnet-base-v2",
                              gemini_api_key = NULL,
                              gemini_dims = 2000L,
                              gemini_batch_size = 10L,
                              gemini_sleep = 63,
                              # Seed similarity params
                              seed_method = "correlation",
                              # Aggregation params
                              extra_sum_cols = NULL,
                              extra_mean_cols = NULL,
                              extra_distinct_cols = NULL,
                              # Output params
                              save_intermediates = FALSE,
                              save_prefix = "pipeline",
                              verbose = TRUE) {
  if (is.null(response_col)) response_col <- text_col

  message("========================================")
  message("SADCAT Pipeline: process_responses()")
  message("Stages: ", paste(stages, collapse = " -> "))
  message("========================================")

  result <- list(long = NULL, agg = NULL)

  # ---- Stage 1: Preprocess ----
  if ("preprocess" %in% stages) {
    data <- preprocess_text(data,
                            text_col = text_col,
                            spellcheck = spellcheck,
                            spellcheck_method = spellcheck_method,
                            singularize = singularize_text,
                            java_home = java_home,
                            wordnet_dict = wordnet_dict,
                            gemini_spellcheck_key = gemini_spellcheck_key,
                            gemini_spellcheck_model = gemini_spellcheck_model,
                            gemini_spellcheck_context = gemini_spellcheck_context,
                            verbose = verbose)
    if (save_intermediates) {
      write.csv(data, paste0(save_prefix, "_1_preprocessed.csv"), row.names = FALSE)
    }
  }

  # ---- Stage 2: Valence ----
  if ("valence" %in% stages) {
    data <- score_valence(data,
                          text_col = valence_text_col,
                          response_col = response_col)
    if (save_intermediates) {
      write.csv(data, paste0(save_prefix, "_2_valence.csv"), row.names = FALSE)
    }
  }

  # ---- Stage 3+4: Dictionaries ----
  if ("dictionaries" %in% stages) {
    sadcat_dict <- prepare_sadcat_dictionaries()
    data <- match_dictionaries(data,
                               text_col = "tv3",
                               response_col = response_col,
                               valence_col = "Valence",
                               valence_nona_col = "ValenceNoNA",
                               sadcat_dict = sadcat_dict)
    if (save_intermediates) {
      write.csv(data, paste0(save_prefix, "_3_dictionaries.csv"), row.names = FALSE)
    }
  }

  # ---- Stage 5: Embeddings ----
  if ("embeddings" %in% stages) {
    data <- compute_embeddings(data,
                               text_col = "tv",
                               methods = embedding_methods,
                               sbert_model = sbert_model,
                               gemini_api_key = gemini_api_key,
                               gemini_dims = gemini_dims,
                               gemini_batch_size = gemini_batch_size,
                               gemini_sleep = gemini_sleep,
                               response_col = response_col,
                               verbose = verbose)
    if (save_intermediates) {
      write.csv(data, paste0(save_prefix, "_4_embeddings.csv"), row.names = FALSE)
    }
  }

  # ---- Stage 6: Seed similarities ----
  if ("seeds" %in% stages) {
    for (prefix in embedding_methods) {
      prefix_upper <- toupper(substr(prefix, 1, 1))
      prefix_name <- paste0(prefix_upper, substr(prefix, 2, nchar(prefix)))
      # SBERT -> "SBERT", gemini -> "Gemini"
      if (tolower(prefix) == "sbert") prefix_name <- "SBERT"
      if (tolower(prefix) == "gemini") prefix_name <- "Gemini"

      data <- compute_seed_similarities(data,
                                        embedding_prefix = prefix_name,
                                        method = seed_method,
                                        response_col = response_col,
                                        verbose = verbose)
    }
    if (save_intermediates) {
      write.csv(data, paste0(save_prefix, "_5_seeds.csv"), row.names = FALSE)
    }
  }

  # Final targeted masking pass for missing responses
  data <- .mask_missing_response_cols(
    data,
    response_col,
    cols_or_patterns = c(
      "^Val_", "^Valence$", "^ValenceNoNA$",
      "_Valy$", "_ValyNoNA$", "_valy3$", "_valyNoNA3$",
      "_dirx$", "_dirx2$", "_dirx3$",
      "^SBERT_\\d+$", "^Gemini_\\d+$", "\\.seed$"
    )
  )

  result$long <- data

  # ---- Stage 7: Aggregate ----
  if ("aggregate" %in% stages) {
    result$agg <- aggregate_responses(data,
                                      group_cols = group_cols,
                                      extra_sum_cols = extra_sum_cols,
                                      extra_mean_cols = extra_mean_cols,
                                      extra_distinct_cols = extra_distinct_cols,
                                      verbose = verbose)
    if (save_intermediates) {
      write.csv(result$agg, paste0(save_prefix, "_6_aggregated.csv"), row.names = FALSE)
    }
  }

  message("========================================")
  message("Pipeline complete!")
  if (!is.null(result$long)) message("  Long data: ", nrow(result$long), " rows x ", ncol(result$long), " cols")
  if (!is.null(result$agg)) message("  Aggregated: ", nrow(result$agg), " rows x ", ncol(result$agg), " cols")
  message("========================================")

  return(result)
}







# =============================================================================
# PATCHED OVERRIDES (2026-02-23)
# =============================================================================

.mask_missing_response_cols <- function(data, response_col, cols_or_patterns) {
  if (is.null(response_col) || !(response_col %in% names(data))) {
    return(data)
  }
  if (is.null(cols_or_patterns) || length(cols_or_patterns) == 0) {
    return(data)
  }

  missing_idx <- is.na(data[[response_col]])
  if (!any(missing_idx)) {
    return(data)
  }

  target_cols <- character(0)
  for (item in cols_or_patterns) {
    if (is.na(item) || !nzchar(item)) {
      next
    }
    if (item %in% names(data)) {
      target_cols <- c(target_cols, item)
      next
    }
    matches <- grep(item, names(data), value = TRUE)
    if (length(matches) > 0) {
      target_cols <- c(target_cols, matches)
    }
  }

  target_cols <- unique(target_cols)
  if (length(target_cols) == 0) {
    return(data)
  }

  data[missing_idx, target_cols] <- NA
  data
}

score_valence <- function(data,
                          text_col = "tv",
                          response_col = NULL) {
  if (is.null(response_col)) response_col <- text_col
  message("--- Stage 2: Scoring valence (5 dictionaries) ---")

  # Deduplicate: score unique texts only, then map back
  texts <- data[[text_col]]
  uniq_mask <- !duplicated(texts)
  uniq_texts <- texts[uniq_mask]
  n_uniq <- length(uniq_texts)
  n_total <- length(texts)

  if (n_uniq < n_total) {
    message("  Deduplicating: ", n_total, " rows -> ", n_uniq, " unique texts")
    uniq_scores <- .score_valence_from_scoped_tokens(uniq_texts)
    map_idx <- match(texts, uniq_texts)
    valence_scores <- uniq_scores[map_idx, , drop = FALSE]
    rownames(valence_scores) <- NULL
  } else {
    valence_scores <- .score_valence_from_scoped_tokens(texts)
  }

  # Separate internal negation-aware columns from output columns
  neg_valna_cols <- grep("^\\.neg_valna_", names(valence_scores), value = TRUE)
  neg_valna_avg <- rowMeans(valence_scores[, neg_valna_cols, drop = FALSE], na.rm = TRUE)

  # Add only the raw (non-negation) individual dictionary columns to output
  data <- cbind(data, valence_scores[, !names(valence_scores) %in% neg_valna_cols, drop = FALSE])

  # Combined valence (negation-aware, applied once to the average)
  # Valence: NA when no dictionary matched any sentiment words
  # ValenceNoNA: 0 instead of NA
  data$Valence <- neg_valna_avg
  data$ValenceNoNA <- ifelse(is.nan(neg_valna_avg), 0, neg_valna_avg)

  val_cols <- c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")
  valna_cols <- c("Val_lexicoderNA", "Val_NRCNA", "Val_bingNA", "Val_affinNA", "Val_loughranNA")

  # Ensure valence family columns are NA when the original response is missing
  data <- .mask_missing_response_cols(
    data,
    response_col,
    cols_or_patterns = c("^Val_", "^Valence$", "^ValenceNoNA$")
  )

  # Replace NaN with NA
  data <- replace_nan_with_na(data)

  message("  Valence scoring complete. Columns added: ",
          paste(c(val_cols, valna_cols, "Valence", "ValenceNoNA"), collapse = ", "))
  return(data)
}

match_dictionaries <- function(data,
                               text_col = "tv3",
                               response_col = NULL,
                               valence_col = "Valence",
                               valence_nona_col = "ValenceNoNA",
                               sadcat_dict = NULL) {
  if (is.null(response_col)) response_col <- text_col
  message("--- Stage 4: Matching dictionaries ---")

  if (is.null(sadcat_dict)) {
    sadcat_dict <- prepare_sadcat_dictionaries()
  }

  # ---- Deduplicate for expensive operations ----
  texts <- data[[text_col]]
  uniq_mask <- !duplicated(texts)
  uniq_texts <- texts[uniq_mask]
  map_idx <- match(texts, uniq_texts)
  n_uniq <- length(uniq_texts)
  n_total <- nrow(data)

  if (n_uniq < n_total) {
    message("  Deduplicating: ", n_total, " rows -> ", n_uniq, " unique texts")
  }

  # ---- Prepare text for matching (tv3 -> tv4) on unique texts only ----
  delete_ending_Ss2_internal <- function(x) {
    if (is.na(x)) return(x)
    unlist(lapply(x, function(y) {
      paste(sapply(strsplit(y, ' '), SADCAT::delete_ending_Ss), collapse = ' ')
    }))
  }

  uniq_tv4 <- enc2utf8(as.character(uniq_texts))
  uniq_tv4 <- tolower(uniq_tv4)
  uniq_tv4 <- gsub("-", " ", uniq_tv4)
  uniq_tv4 <- vapply(uniq_tv4, delete_ending_Ss2_internal, character(1), USE.NAMES = FALSE)

  # Map tv4 back to full data (needed for drop step)
  data$tv4 <- uniq_tv4[map_idx]

  # ---- Tokenize unique texts ----
  toks <- .tokenize_quanteda_text(uniq_tv4,
                                  prefix = "response",
                                  remove_numbers = FALSE,
                                  remove_punct = TRUE,
                                  remove_symbols = TRUE)

  toks_dict_pre <- quanteda::tokens_lookup(toks, dictionary = sadcat_dict,
                                           nested_scope = "dictionary",
                                           exclusive = TRUE, levels = 1)
  uniq_dict_df <- quanteda::convert(quanteda::dfm(toks_dict_pre), to = "data.frame")
  uniq_dict_df$doc_id <- NULL
  uniq_dict_df$ntoken <- quanteda::ntoken(toks)

  # Map unique results back to full data
  toks_dict_df <- uniq_dict_df[map_idx, , drop = FALSE]
  rownames(toks_dict_df) <- NULL
  toks_dict <- cbind(toks_dict_df, data)

  dict_cols_all <- tolower(names(sadcat_dict))

  for (col in dict_cols_all) {
    toks_dict[[paste0(col, "_percent")]] <- toks_dict[[col]] / toks_dict$ntoken
  }

  for (col in dict_cols_all) {
    toks_dict[[paste0(col, "_binary")]] <- ifelse(toks_dict[[col]] > 0, 1, 0)
  }

  for (dim in .SADCAT_DIR_DIMS) {
    hi_col <- paste0(tolower(dim), "_dic_hi_binary")
    lo_col <- paste0(tolower(dim), "_dic_lo_binary")
    dirx_col <- paste0(dim, "_dirx")
    if (hi_col %in% names(toks_dict) && lo_col %in% names(toks_dict)) {
      toks_dict[[dirx_col]] <- toks_dict[[hi_col]] - toks_dict[[lo_col]]
    }
  }

  all_base_dims <- c(.SADCAT_DIR_DIMS, .SADCAT_NDIR_DIMS)
  for (dim in all_base_dims) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    binary2_col <- paste0(tolower(dim), "_dic_binary2")
    if (binary_col %in% names(toks_dict)) {
      toks_dict[[binary2_col]] <- ifelse(is.na(toks_dict[[response_col]]),
                                         NA, toks_dict[[binary_col]])
    }
  }

  binary2_cols <- grep("_dic_binary2$", names(toks_dict), value = TRUE)
  if (length(binary2_cols) > 0) {
    row_sums <- rowSums(toks_dict[, binary2_cols, drop = FALSE], na.rm = FALSE)
    toks_dict$None2y <- ifelse(is.na(row_sums), NA, ifelse(row_sums > 0, 0, 1))
  }

  for (dim in all_base_dims) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    valy_dim <- paste0(dim, "_Valy")
    valyno_dim <- paste0(dim, "_ValyNoNA")

    if (binary_col %in% names(toks_dict) && valence_col %in% names(toks_dict)) {
      toks_dict[[valy_dim]] <- ifelse(toks_dict[[binary_col]] == 0,
                                      NA, toks_dict[[valence_col]])
    }
    if (binary_col %in% names(toks_dict) && valence_nona_col %in% names(toks_dict)) {
      toks_dict[[valyno_dim]] <- ifelse(toks_dict[[binary_col]] == 0,
                                        0, toks_dict[[valence_nona_col]])
    }
  }

  if ("None2y" %in% names(toks_dict) && valence_col %in% names(toks_dict)) {
    toks_dict$NONE_Valy <- ifelse(toks_dict$None2y == 0, NA, toks_dict[[valence_col]])
  }

  uniq_adjusted_dir <- .compute_adjusted_direction_scores(uniq_texts, sadcat_dict)
  for (col in names(uniq_adjusted_dir)) {
    toks_dict[[col]] <- uniq_adjusted_dir[map_idx, col]
  }

  for (dim in .SADCAT_DIR_DIMS) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    binary2_col <- paste0(tolower(dim), "_dic_binary2")
    valy_dim <- paste0(dim, "_Valy")
    valyno_dim <- paste0(dim, "_ValyNoNA")
    valy3_col <- paste0(dim, "_valy3")
    valyno3_col <- paste0(dim, "_valyNoNA3")
    dirx_col <- paste0(dim, "_dirx")
    dirx2_col <- paste0(dim, "_dirx2")
    dirx3_col <- paste0(dim, "_dirx3")

    if (all(c(binary_col, binary2_col, valy_dim) %in% names(toks_dict))) {
      toks_dict[[valy3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valy_dim]])
    }
    if (all(c(binary_col, binary2_col, valyno_dim) %in% names(toks_dict))) {
      toks_dict[[valyno3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valyno_dim]])
    }
    if (all(c(binary_col, binary2_col, dirx_col) %in% names(toks_dict))) {
      toks_dict[[dirx3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[dirx_col]])
    }
  }

  ndir_for_valy3 <- c("Occupation", "Emotion", "Deviance", "Socialgroups",
                       "Geography", "Appearance", "Other", "OtherwFam")
  for (dim in ndir_for_valy3) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    binary2_col <- paste0(tolower(dim), "_dic_binary2")
    valy_dim <- paste0(dim, "_Valy")
    valyno_dim <- paste0(dim, "_ValyNoNA")
    valy3_col <- paste0(dim, "_valy3")
    valyno3_col <- paste0(dim, "_valyNoNA3")

    if (all(c(binary_col, binary2_col, valy_dim) %in% names(toks_dict))) {
      toks_dict[[valy3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valy_dim]])
    }
    if (all(c(binary_col, binary2_col, valyno_dim) %in% names(toks_dict))) {
      toks_dict[[valyno3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valyno_dim]])
    }
  }

  toks_dict <- .mask_missing_response_cols(
    toks_dict,
    response_col,
    cols_or_patterns = c(
      "_Valy$", "_ValyNoNA$", "_valy3$", "_valyNoNA3$",
      "_dirx$", "_dirx2$", "_dirx3$"
    )
  )

  toks_dict <- replace_nan_with_na(toks_dict)

  message("  Dictionary matching complete.")
  return(toks_dict)
}

compute_embeddings <- function(data,
                               text_col = "tv",
                               methods = c("sbert"),
                               sbert_model = "paraphrase-mpnet-base-v2",
                               sbert_dims = 768L,
                               gemini_api_key = NULL,
                               gemini_model = "gemini-embedding-exp-03-07",
                               gemini_dims = 2000L,
                               gemini_batch_size = 10L,
                               gemini_sleep = 63,
                               gemini_task_type = "SEMANTIC_SIMILARITY",
                               response_col = NULL,
                               verbose = TRUE) {
  if (is.null(response_col)) response_col <- text_col

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for compute_embeddings(). Install with: install.packages('reticulate')")
  }

  message("--- Stage 5: Computing embeddings ---")

  unique_text <- unique(data[[text_col]])
  unique_text <- unique_text[!is.na(unique_text) & unique_text != ""]
  if (verbose) message("  Unique text values to encode: ", length(unique_text))

  if ("sbert" %in% tolower(methods)) {
    if (verbose) message("  Computing SBERT embeddings (model: ", sbert_model, ")...")

    st <- reticulate::import("sentence_transformers")
    model_sbert <- st$SentenceTransformer(sbert_model)

    encoding_matrix <- model_sbert$encode(unique_text)
    encoding_df <- as.data.frame(reticulate::py_to_r(encoding_matrix))
    colnames(encoding_df) <- paste0("SBERT_", seq_len(ncol(encoding_df)))

    sbert_vecs <- data.frame(word = unique_text, encoding_df, stringsAsFactors = FALSE)

    join_by <- stats::setNames("word", text_col)
    data <- dplyr::left_join(data, sbert_vecs, by = join_by)

    sbert_cols <- paste0("SBERT_", seq_len(ncol(encoding_df)))
    data <- .mask_missing_response_cols(
      data,
      response_col,
      cols_or_patterns = c("^SBERT_\\d+$")
    )

    if (verbose) message("  SBERT complete. Added ", length(sbert_cols), " columns.")
  }

  if ("gemini" %in% tolower(methods)) {
    api_key <- gemini_api_key
    if (is.null(api_key) || api_key == "") {
      api_key <- Sys.getenv("GEMINI_API_KEY")
    }
    if (is.null(api_key) || api_key == "") {
      stop("Gemini API key not found. Provide via gemini_api_key parameter or set GEMINI_API_KEY environment variable.")
    }

    if (verbose) message("  Computing Gemini embeddings (model: ", gemini_model,
                         ", dims: ", gemini_dims, ")...")

    genai <- reticulate::import("google.genai")
    types <- reticulate::import("google.genai.types")

    client <- genai$Client(api_key = api_key)

    config <- types$EmbedContentConfig(
      output_dimensionality = as.integer(gemini_dims),
      task_type = gemini_task_type
    )

    text_list <- as.list(unique_text)
    all_embeddings <- list()
    n_batches <- ceiling(length(text_list) / gemini_batch_size)

    for (i in seq(1, length(text_list), by = gemini_batch_size)) {
      batch_num <- ceiling(i / gemini_batch_size)
      batch_end <- min(i + gemini_batch_size - 1, length(text_list))
      batch <- text_list[i:batch_end]

      if (verbose) message("    Batch ", batch_num, " / ", n_batches,
                           " (items ", i, "-", batch_end, ")")

      result <- tryCatch({
        client$models$embed_content(
          model = gemini_model,
          contents = batch,
          config = config
        )
      }, error = function(e) {
        warning("  Batch ", batch_num, " failed: ", conditionMessage(e))
        NULL
      })

      if (!is.null(result)) {
        all_embeddings <- c(all_embeddings, list(result))
      }

      if (batch_end < length(text_list)) {
        if (verbose) message("    Sleeping ", gemini_sleep, " seconds (rate limit)...")
        Sys.sleep(gemini_sleep)
      }
    }

    flat_embeddings <- list()
    for (batch_result in all_embeddings) {
      for (emb in batch_result$embeddings) {
        flat_embeddings <- c(flat_embeddings, list(emb$values))
      }
    }

    gemini_matrix <- do.call(rbind, flat_embeddings)
    gemini_df <- as.data.frame(gemini_matrix)
    colnames(gemini_df) <- paste0("Gemini_", seq_len(ncol(gemini_df)))

    gemini_vecs <- data.frame(word = unique_text[seq_len(nrow(gemini_df))],
                              gemini_df, stringsAsFactors = FALSE)

    join_by <- stats::setNames("word", text_col)
    data <- dplyr::left_join(data, gemini_vecs, by = join_by)

    gemini_cols <- paste0("Gemini_", seq_len(ncol(gemini_df)))
    data <- .mask_missing_response_cols(
      data,
      response_col,
      cols_or_patterns = c("^Gemini_\\d+$")
    )

    if (verbose) message("  Gemini complete. Added ", length(gemini_cols), " columns.")
  }

  message("  Embedding computation complete.")
  return(data)
}

compute_seed_similarities <- function(data,
                                      embedding_prefix = "SBERT",
                                      seed_vectors = SADCAT::Seed_Vectors_Avg,
                                      method = "correlation",
                                      response_col = NULL,
                                      verbose = TRUE) {
  message("--- Stage 6: Computing seed similarities (", embedding_prefix, ") ---")

  seed_emb_cols <- grep(paste0("^", embedding_prefix, "_"), names(seed_vectors), value = TRUE)
  if (length(seed_emb_cols) == 0) {
    message("  No columns matching prefix '", embedding_prefix,
            "' found in seed_vectors. Skipping seed similarity computation.")
    return(data)
  }

  data_emb_cols <- grep(paste0("^", embedding_prefix, "_\\d+$"), names(data), value = TRUE)
  if (length(data_emb_cols) == 0) {
    message("  No embedding columns matching prefix '", embedding_prefix,
            "' found in data. Skipping seed similarity computation.")
    return(data)
  }

  Seed_Vectors_names <- paste0(as.character(seed_vectors$Dictionary), ".seed")

  Seed_Matrix <- seed_vectors %>%
    dplyr::select(dplyr::all_of(seed_emb_cols)) %>%
    t()

  colnames(Seed_Matrix) <- Seed_Vectors_names

  if (verbose) message("  Computing ", method, " between ",
                       length(data_emb_cols), " dims and ",
                       ncol(Seed_Matrix), " seed vectors...")

  response_matrix <- data[, data_emb_cols, drop = FALSE]

  if (method == "correlation") {
    sim_matrix <- t(apply(response_matrix, 1, function(z) {
      cor(as.numeric(z), Seed_Matrix)
    }))
  } else if (method == "cosine") {
    sim_matrix <- t(apply(response_matrix, 1, function(z) {
      cosinize(z, Seed_Matrix)
    }))
  } else {
    stop("method must be 'correlation' or 'cosine'")
  }

  colnames(sim_matrix) <- paste0(embedding_prefix, "_", Seed_Vectors_names)
  sim_df <- as.data.frame(sim_matrix)

  data <- cbind(data, sim_df)

  data <- .mask_missing_response_cols(
    data,
    response_col,
    cols_or_patterns = names(sim_df)
  )

  if (verbose) message("  Seed similarities complete. Added ", ncol(sim_df), " columns.")
  return(data)
}

aggregate_responses <- function(data,
                                group_cols = c("Synonym.GroupX", "Group", "Level"),
                                sum_cols = NULL,
                                mean_cols = NULL,
                                distinct_cols = NULL,
                                extra_sum_cols = NULL,
                                extra_mean_cols = NULL,
                                extra_distinct_cols = NULL,
                                create_noNA = TRUE,
                                verbose = TRUE) {
  message("--- Stage 7: Aggregating responses ---")

  all_names <- names(data)

  if (is.null(sum_cols)) {
    sum_cols <- grep("_dic_binary2$|^None2y$|^traditional$", all_names, value = TRUE)
  }
  if (!is.null(extra_sum_cols)) {
    extra_sum_cols <- extra_sum_cols[extra_sum_cols %in% all_names]
    sum_cols <- unique(c(sum_cols, extra_sum_cols))
  }

  if (is.null(mean_cols)) {
    mean_cols <- grep(
      paste0("^Valence$|^ValenceNoNA$|",
             "_dic_binary2$|^None2y$|",
             "_ValyNoNA$|^NONE_Valy$|",
             "_valy3$|_valyNoNA3$|_dirx3$|",
             "^SBERT_|^Gemini_|",
             "\\.seed$|",
             "^traditional$"),
      all_names, value = TRUE
    )
  }
  if (!is.null(extra_mean_cols)) {
    extra_mean_cols <- extra_mean_cols[extra_mean_cols %in% all_names]
    mean_cols <- unique(c(mean_cols, extra_mean_cols))
  }

  if (is.null(distinct_cols)) {
    distinct_cols <- grep("Warning$|^Level$", all_names, value = TRUE)
    distinct_cols <- setdiff(distinct_cols, group_cols)
  }
  if (!is.null(extra_distinct_cols)) {
    extra_distinct_cols <- extra_distinct_cols[extra_distinct_cols %in% all_names]
    distinct_cols <- unique(c(distinct_cols, extra_distinct_cols))
  }

  missing_groups <- setdiff(group_cols, all_names)
  if (length(missing_groups) > 0) {
    stop("Group columns not found in data: ", paste(missing_groups, collapse = ", "))
  }

  if (verbose) {
    message("  Sum columns: ", length(sum_cols))
    message("  Mean columns: ", length(mean_cols))
    message("  Distinct columns: ", length(distinct_cols))
  }

  if (length(sum_cols) > 0) {
    sum_data <- data %>%
      dplyr::select(dplyr::all_of(c(group_cols, sum_cols))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)),
                       .groups = "drop")
    sum_rename <- setdiff(names(sum_data), group_cols)
    names(sum_data)[names(sum_data) %in% sum_rename] <- paste0(sum_rename, "_Sum")
  } else {
    sum_data <- NULL
  }

  if (length(mean_cols) > 0) {
    mean_data <- data %>%
      dplyr::select(dplyr::all_of(c(group_cols, mean_cols))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~mean(., na.rm = TRUE)),
                       .groups = "drop")
  } else {
    mean_data <- NULL
  }

  if (length(distinct_cols) > 0) {
    distinct_data <- data %>%
      dplyr::select(dplyr::all_of(c(group_cols, distinct_cols))) %>%
      dplyr::distinct()
  } else {
    distinct_data <- NULL
  }

  parts <- Filter(Negate(is.null), list(distinct_data, mean_data, sum_data))
  if (length(parts) == 0) {
    stop("No columns selected for aggregation.")
  }
  result <- plyr::join_all(parts, by = group_cols, type = "left")

  result <- replace_nan_with_na(result)

  if (create_noNA) {
    dirx3_cols <- grep("_dirx3$", names(result), value = TRUE)
    for (col in dirx3_cols) {
      result[[paste0(col, "noNA")]] <- ifelse(is.na(result[[col]]), 0, result[[col]])
    }

    valy_cols <- grep("_valy3$|_valyNoNA3$", names(result), value = TRUE)
    for (col in valy_cols) {
      result[[paste0(col, "noNA")]] <- ifelse(is.na(result[[col]]), 0, result[[col]])
    }

    if (verbose) {
      message("  Created ", length(dirx3_cols) + length(valy_cols), " noNA columns.")
    }
  }

  if (verbose) message("  Aggregation complete. Result: ",
                       nrow(result), " rows x ", ncol(result), " columns.")
  return(result)
}

process_responses <- function(data,
                              text_col = "responsex",
                              response_col = NULL,
                              group_cols = c("Synonym.GroupX", "Group", "Level"),
                              stages = c("preprocess", "valence", "dictionaries",
                                         "embeddings", "seeds", "aggregate"),
                              spellcheck = TRUE,
                              spellcheck_method = "hunspell",
                              singularize_text = TRUE,
                              java_home = "C:\\Program Files\\Java\\jre-1.8",
                              wordnet_dict = "C:\\dict",
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
                              verbose = TRUE) {
  if (is.null(response_col)) response_col <- text_col

  message("========================================")
  message("SADCAT Pipeline: process_responses()")
  message("Stages: ", paste(stages, collapse = " -> "))
  message("========================================")

  result <- list(long = NULL, agg = NULL)

  if ("preprocess" %in% stages) {
    data <- preprocess_text(data,
                            text_col = text_col,
                            spellcheck = spellcheck,
                            spellcheck_method = spellcheck_method,
                            singularize = singularize_text,
                            java_home = java_home,
                            wordnet_dict = wordnet_dict,
                            gemini_spellcheck_key = gemini_spellcheck_key,
                            gemini_spellcheck_model = gemini_spellcheck_model,
                            gemini_spellcheck_context = gemini_spellcheck_context,
                            verbose = verbose)
    if (save_intermediates) write.csv(data, paste0(save_prefix, "_1_preprocessed.csv"), row.names = FALSE)
  }

  if ("valence" %in% stages) {
    data <- score_valence(data, text_col = valence_text_col, response_col = response_col)
    if (save_intermediates) write.csv(data, paste0(save_prefix, "_2_valence.csv"), row.names = FALSE)
  }

  if ("dictionaries" %in% stages) {
    sadcat_dict <- prepare_sadcat_dictionaries()
    data <- match_dictionaries(data,
                               text_col = "tv3",
                               response_col = response_col,
                               valence_col = "Valence",
                               valence_nona_col = "ValenceNoNA",
                               sadcat_dict = sadcat_dict)
    if (save_intermediates) write.csv(data, paste0(save_prefix, "_3_dictionaries.csv"), row.names = FALSE)
  }

  if ("embeddings" %in% stages) {
    data <- compute_embeddings(data,
                               text_col = "tv",
                               methods = embedding_methods,
                               sbert_model = sbert_model,
                               gemini_api_key = gemini_api_key,
                               gemini_dims = gemini_dims,
                               gemini_batch_size = gemini_batch_size,
                               gemini_sleep = gemini_sleep,
                               response_col = response_col,
                               verbose = verbose)
    if (save_intermediates) write.csv(data, paste0(save_prefix, "_4_embeddings.csv"), row.names = FALSE)
  }

  if ("seeds" %in% stages) {
    for (prefix in embedding_methods) {
      prefix_upper <- toupper(substr(prefix, 1, 1))
      prefix_name <- paste0(prefix_upper, substr(prefix, 2, nchar(prefix)))
      if (tolower(prefix) == "sbert") prefix_name <- "SBERT"
      if (tolower(prefix) == "gemini") prefix_name <- "Gemini"

      data <- compute_seed_similarities(data,
                                        embedding_prefix = prefix_name,
                                        method = seed_method,
                                        response_col = response_col,
                                        verbose = verbose)
    }
    if (save_intermediates) write.csv(data, paste0(save_prefix, "_5_seeds.csv"), row.names = FALSE)
  }

  data <- .mask_missing_response_cols(
    data,
    response_col,
    cols_or_patterns = c(
      "^Val_", "^Valence$", "^ValenceNoNA$",
      "_Valy$", "_ValyNoNA$", "_valy3$", "_valyNoNA3$",
      "_dirx$", "_dirx2$", "_dirx3$",
      "^SBERT_\\d+$", "^Gemini_\\d+$", "\\.seed$"
    )
  )

  result$long <- data

  if ("aggregate" %in% stages) {
    result$agg <- aggregate_responses(data,
                                      group_cols = group_cols,
                                      extra_sum_cols = extra_sum_cols,
                                      extra_mean_cols = extra_mean_cols,
                                      extra_distinct_cols = extra_distinct_cols,
                                      verbose = verbose)
    if (save_intermediates) write.csv(result$agg, paste0(save_prefix, "_6_aggregated.csv"), row.names = FALSE)
  }

  message("========================================")
  message("Pipeline complete!")
  if (!is.null(result$long)) message("  Long data: ", nrow(result$long), " rows x ", ncol(result$long), " cols")
  if (!is.null(result$agg)) message("  Aggregated: ", nrow(result$agg), " rows x ", ncol(result$agg), " cols")
  message("========================================")

  return(result)
}


