# Internal helpers for scoped negation-aware scoring

.normalize_valence_text <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  # Preserve anti- / anti <word> as a cue token before clean_large_text()
  x <- stringr::str_replace_all(
    x,
    "(?i)\\banti\\s*(?:-|\\s)\\s*(?=[[:alpha:]])",
    " zzzneganti "
  )
  x <- clean_large_text(x)
  x <- stringr::str_replace_all(x, "\\((no|non|nor|neither|never|hardly|nobody|none|avoid|avoided|avoiding|avoids|nothing|less|minimize|reduce|withdraw|low|little|without|no_longer)\\)", " ")
  x <- stringr::str_replace_all(x, "zzzneganti", "anti")
  stringr::str_squish(x)
}

.normalize_valence_terms <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  x <- clean_large_text(x)
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
    tokens[word_idx] <- vapply(tokens[word_idx], delete_ending_Ss, character(1))
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

.build_dict_output <- function(presence, pos_key, neg_key, name,
                                lex_neg_presence = NULL,
                                neg_pos_key = NULL,
                                neg_neg_key = NULL) {
  any_pos <- presence$nonneg[, pos_key] | presence$neg[, pos_key]
  any_neg <- presence$nonneg[, neg_key] | presence$neg[, neg_key]

  neg_pos_binary <- presence$nonneg[, pos_key] | presence$neg[, neg_key]
  neg_neg_binary <- presence$nonneg[, neg_key] | presence$neg[, pos_key]

  if (!is.null(neg_pos_key) && !is.null(lex_neg_presence)) {
    any_neg <- any_neg | lex_neg_presence[[neg_pos_key]]
    neg_neg_binary <- neg_neg_binary | lex_neg_presence[[neg_pos_key]]
  }
  if (!is.null(neg_neg_key) && !is.null(lex_neg_presence)) {
    any_pos <- any_pos | lex_neg_presence[[neg_neg_key]]
    neg_pos_binary <- neg_pos_binary | lex_neg_presence[[neg_neg_key]]
  }

  raw_val <- any_pos - any_neg
  raw_valna <- ifelse(any_pos + any_neg == 0, NA, raw_val)

  neg_val <- neg_pos_binary - neg_neg_binary
  neg_valna <- ifelse(neg_pos_binary + neg_neg_binary == 0, NA, neg_val)

  out <- data.frame(raw_val = raw_val, raw_valna = raw_valna, neg_valna = neg_valna)
  colnames(out) <- c(paste0("Val_", name), paste0("Val_", name, "NA"),
                      paste0(".neg_valna_", name))
  out
}

.score_valence_from_scoped_tokens <- function(text,
                                               include_sentiwn = TRUE,
                                               sentiwn_threshold = 0.10,
                                               include_warriner = TRUE,
                                               include_jockers = TRUE) {
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

  out <- cbind(
    .build_dict_output(presence, "lex_positive", "lex_negative", "lexicoder",
                        lex_neg_presence = lex_neg_presence,
                        neg_pos_key = "neg_positive", neg_neg_key = "neg_negative"),
    .build_dict_output(presence, "nrc_positive",      "nrc_negative",      "NRC"),
    .build_dict_output(presence, "bing_positive",     "bing_negative",     "bing"),
    .build_dict_output(presence, "affin_positive",    "affin_negative",    "affin"),
    .build_dict_output(presence, "loughran_positive", "loughran_negative", "loughran")
  )

  if (isTRUE(include_sentiwn)) {
    swn_spec <- .get_sentiwn_negation_spec(threshold = sentiwn_threshold)
    swn_profile <- .build_scoped_token_profile(
      text = text,
      normalizer = .normalize_valence_text,
      cue_tokens = swn_spec$cue_tokens,
      compound_patterns = swn_spec$compound_patterns,
      singularize_words = FALSE
    )
    swn_presence <- .compute_group_presence(swn_profile, swn_spec$term_groups)
    out <- cbind(out, .build_dict_output(swn_presence,
                                          "sentiwn_positive",
                                          "sentiwn_negative",
                                          "sentiwn"))
  }

  if (isTRUE(include_warriner)) {
    warr <- .get_continuous_lexicon_lookup("warriner")
    warr_scores <- .score_continuous_dict(profile, warr$env)
    out <- cbind(out, .build_continuous_dict_output(warr_scores, "warriner"))
  }

  if (isTRUE(include_jockers)) {
    jock <- .get_continuous_lexicon_lookup("jockers")
    jock_scores <- .score_continuous_dict(profile, jock$env)
    out <- cbind(out, .build_continuous_dict_output(jock_scores, "jockers"))
  }

  replace_nan_with_na(out)
}

.get_continuous_lexicon_lookup <- function(name) {
  cache_key <- paste0("continuous_lex_lookup_", name)
  if (exists(cache_key, envir = .negation_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .negation_cache, inherits = FALSE))
  }

  data_name <- switch(name,
    warriner = "Warriner_norms",
    jockers  = "Jockers_norms",
    stop("Unknown continuous lexicon: ", name)
  )
  e <- new.env(parent = emptyenv())
  utils::data(list = data_name, package = "SADCAT", envir = e)
  df <- e[[data_name]]

  raw_words <- as.character(df$word)
  vals <- as.numeric(df$valence)

  # Vectorized normalization: apply the same normalizer + tokenizer that
  # response text goes through, in a single batch over all lexicon words.
  toks <- .tokenize_quanteda_text(
    .normalize_valence_terms(raw_words),
    prefix = "term",
    remove_numbers = FALSE,
    remove_punct = TRUE,
    remove_symbols = TRUE
  )
  tok_list <- quanteda::as.list(toks)
  # Each lexicon entry produces a list of tokens. Keep entries that produce
  # exactly one non-empty token (single-word lexicon entries).
  norm_each <- vapply(tok_list, function(tok) {
    tok <- tok[nzchar(tok)]
    if (length(tok) == 1L) tok else NA_character_
  }, character(1), USE.NAMES = FALSE)

  keep <- !is.na(norm_each) &
    !grepl(.NEGATION_TOKEN_SEPARATOR, norm_each, fixed = TRUE) &
    !is.na(vals)
  norm_each <- norm_each[keep]
  vals <- vals[keep]

  ord <- !duplicated(norm_each)
  norm_each <- norm_each[ord]
  vals <- vals[ord]

  lookup <- new.env(hash = TRUE, parent = emptyenv(),
                    size = max(8L, length(norm_each)))
  for (i in seq_along(norm_each)) {
    assign(norm_each[i], vals[i], envir = lookup)
  }

  result <- list(env = lookup, n = length(norm_each))
  assign(cache_key, result, envir = .negation_cache)
  result
}

.score_continuous_dict <- function(profile, lookup_env) {
  n_docs <- length(profile$tokens)
  raw_score <- numeric(n_docs)
  raw_n <- integer(n_docs)
  neg_score <- numeric(n_docs)
  neg_n <- integer(n_docs)

  for (i in seq_len(n_docs)) {
    tokens <- profile$tokens[[i]]
    negated <- profile$negated[[i]]
    if (!length(tokens)) next

    keep <- !vapply(tokens, .is_punctuation_token, logical(1))
    tokens <- tokens[keep]
    negated <- negated[keep]
    if (!length(tokens)) next

    nonneg_unique <- unique(tokens[!negated])
    neg_unique <- unique(tokens[negated])

    lookup_one <- function(tok) {
      v <- get0(tok, envir = lookup_env, inherits = FALSE,
                ifnotfound = NA_real_)
      as.numeric(v)
    }
    nonneg_vals <- if (length(nonneg_unique))
      vapply(nonneg_unique, lookup_one, numeric(1)) else numeric(0)
    neg_vals <- if (length(neg_unique))
      vapply(neg_unique, lookup_one, numeric(1)) else numeric(0)

    matched_nonneg <- !is.na(nonneg_vals)
    matched_neg <- !is.na(neg_vals)

    raw_matched <- c(nonneg_vals[matched_nonneg], neg_vals[matched_neg])
    raw_score[i] <- if (length(raw_matched)) mean(raw_matched) else 0
    raw_n[i] <- length(raw_matched)

    signed_matched <- c(nonneg_vals[matched_nonneg],
                         -neg_vals[matched_neg])
    neg_score[i] <- if (length(signed_matched)) mean(signed_matched) else 0
    neg_n[i] <- length(signed_matched)
  }

  list(raw_score = raw_score, raw_n = raw_n,
       neg_score = neg_score, neg_n = neg_n)
}

.build_continuous_dict_output <- function(scores, name) {
  raw_val <- scores$raw_score
  raw_valna <- ifelse(scores$raw_n == 0L, NA_real_, scores$raw_score)
  neg_valna <- ifelse(scores$neg_n == 0L, NA_real_, scores$neg_score)

  out <- data.frame(raw_val = raw_val,
                    raw_valna = raw_valna,
                    neg_valna = neg_valna)
  colnames(out) <- c(paste0("Val_", name),
                      paste0("Val_", name, "NA"),
                      paste0(".neg_valna_", name))
  out
}

.get_sentiwn_negation_spec <- function(threshold = 0.10) {
  cache_key <- paste0("sentiwn_negation_spec_", format(threshold, nsmall = 4))
  if (exists(cache_key, envir = .negation_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .negation_cache, inherits = FALSE))
  }

  swn_env <- new.env(parent = emptyenv())
  utils::data("Sentiments", package = "SADCAT", envir = swn_env)
  swn <- swn_env$Sentiments
  swn <- swn[!is.na(swn$Val_sentiwn) & swn$Val_sentiwn != 0, c("word", "Val_sentiwn")]
  swn$word <- tolower(as.character(swn$word))
  swn <- swn[!is.na(swn$word) & nchar(swn$word) > 0, ]
  swn <- swn[!duplicated(swn$word), ]
  swn <- swn[abs(swn$Val_sentiwn) >= threshold, ]

  pos_terms <- swn$word[swn$Val_sentiwn > 0]
  neg_terms <- swn$word[swn$Val_sentiwn < 0]

  term_groups <- list(
    sentiwn_positive = .join_term_tokens(.normalize_term_set(
      pos_terms, .normalize_valence_terms, drop_cue_leading = TRUE)),
    sentiwn_negative = .join_term_tokens(.normalize_term_set(
      neg_terms, .normalize_valence_terms, drop_cue_leading = TRUE))
  )

  compound_terms <- unique(unlist(lapply(term_groups, function(x)
    stringr::str_replace_all(x, .NEGATION_TOKEN_SEPARATOR, " ")), use.names = FALSE))
  compound_terms <- compound_terms[grepl(" ", compound_terms, fixed = TRUE)]

  spec <- list(
    term_groups = term_groups,
    compound_patterns = .build_compound_patterns(compound_terms),
    cue_tokens = c("anti", "not", "limited",
                   paste0("bad", .NEGATION_TOKEN_SEPARATOR, "at"))
  )

  assign(cache_key, spec, envir = .negation_cache)
  spec
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
