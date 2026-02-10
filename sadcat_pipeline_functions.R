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

# Cache environment for memoized dictionary preparation
.sadcat_cache <- new.env(parent = emptyenv())


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
Spellcheck2 <- function(raw, dict_tv = SADCAT::All.steps_Dictionaries$tv, rawlist) {
  toreturn <- raw
  changed <- 0
  tryCatch({
    if (raw %in% dict_tv) {
      return(raw)
    }
    if (hunspell::hunspell_check(raw)) {
      return(raw)
    }
    if (grepl(" ", raw) | grepl("-", raw)) {
      return(raw)
    }
    if (raw == "NA" | is.na(raw)) {
      return("na")
    }
    filter <- wordnet::getTermFilter("ExactMatchFilter", raw, TRUE)
    if (is.null(wordnet::getIndexTerms("NOUN", 5, filter)) &
        is.null(wordnet::getIndexTerms("ADJECTIVE", 5, filter)) &
        is.null(wordnet::getIndexTerms("VERB", 5, filter))) {
      suggestions <- hunspell::hunspell_suggest(raw)
      for (s in suggestions[[1]][1:5]) {
        if (is.na(s)) {
          break
        } else if (s == raw) {
          toreturn <- s
          changed <- 1
          break
        } else if (s %in% rawlist) {
          toreturn <- s
          changed <- 1
          break
        }
      }
      if (changed == 0) {
        toreturn <- suggestions[[1]][1]
      }
    } else {
      return(raw)
    }
    toreturn <- SADCAT::clean_naresponses(toreturn)
    toreturn <- tolower(toreturn)
    return(toreturn)
  }, error = function(s) {
    return("*******ERROR")
  })
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
                            singularize = TRUE,
                            java_home = "C:\\Program Files\\Java\\jre-1.8",
                            wordnet_dict = "C:\\dict",
                            verbose = TRUE) {
  if (verbose) message("--- Stage 1: Preprocessing text ---")

  # Step 1: Lowercase
  data$tv <- tolower(data[[text_col]])
  data$tv <- ifelse(data$tv == "", NA, data$tv)

  # Step 2: Spell-check on unique values
  if (spellcheck) {
    if (verbose) message("  Spell-checking (requires Java + WordNet)...")
    Sys.setenv(JAVA_HOME = java_home)
    wordnet::setDict(wordnet_dict)

    unique_vals <- unique(data$tv)
    result_sc <- vector("character", length(unique_vals))
    for (i in seq_along(unique_vals)) {
      if (verbose && i %% 500 == 0) message("    Spell-checked ", i, " / ", length(unique_vals))
      result_sc[i] <- Spellcheck2(raw = unique_vals[i], rawlist = unique_vals)
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
  quanteda::dictionary(sent_list)
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
#'   Val_affin, Val_loughran, their NA variants, Valy, and ValyNA
#' @export
score_valence <- function(data,
                          text_col = "tv",
                          response_col = "response") {
  message("--- Stage 2: Scoring valence (5 dictionaries) ---")

  # Clean and tokenize
  valuex <- SADCAT::clean_large_text(data[[text_col]])
  toksval <- quanteda::tokens(valuex)

  # 1. Lexicoder (LSD2015) — built-in quanteda dictionary
  lex_result <- apply_single_valence_dict(toksval, quanteda::data_dictionary_LSD2015,
                                          "lexicoder", is_lexicoder = TRUE)

  # 2-5. NRC, Bing, AFINN, Loughran
  nrc_dict <- build_sentiment_dictionary("nrc")
  nrc_result <- apply_single_valence_dict(toksval, nrc_dict, "NRC")

  bing_dict <- build_sentiment_dictionary("bing")
  bing_result <- apply_single_valence_dict(toksval, bing_dict, "bing")

  afinn_dict <- build_sentiment_dictionary("afinn")
  afinn_result <- apply_single_valence_dict(toksval, afinn_dict, "affin")  # "affin" not "afinn" for backward compat

  loughran_dict <- build_sentiment_dictionary("loughran")
  loughran_result <- apply_single_valence_dict(toksval, loughran_dict, "loughran")

  # Bind all valence columns
  data <- cbind(data,
                lex_result,
                nrc_result,
                bing_result,
                afinn_result,
                loughran_result)

  # Apply negation reversal for lexicoder (anti-, non- patterns)
  data <- dplyr::mutate(data,
    Val_lexicoder = ifelse(grepl(.NEGATION_PATTERN_VAL, .data[[response_col]]),
                           Val_lexicoder * -1, Val_lexicoder))

  # Combined valence
  val_cols <- c("Val_lexicoder", "Val_NRC", "Val_bing", "Val_affin", "Val_loughran")
  valna_cols <- c("Val_lexicoderNA", "Val_NRCNA", "Val_bingNA", "Val_affinNA", "Val_loughranNA")

  data$Valy <- rowMeans(data[, val_cols, drop = FALSE], na.rm = TRUE)
  data$ValyNA <- rowMeans(data[, valna_cols, drop = FALSE], na.rm = TRUE)

  # Replace NaN with NA
  data <- replace_nan_with_na(data)

  message("  Valence scoring complete. Columns added: ",
          paste(c(val_cols, valna_cols, "Valy", "ValyNA"), collapse = ", "))
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
                                     character(1))

  # Tokenize to remove punctuation and symbols, then reconstruct
  corpusx2 <- quanteda::tokens(Pre_Dictionaries$values0,
                               remove_numbers = FALSE, remove_punct = TRUE, remove_symbols = TRUE)
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
#' @param valence_col Name of combined valence-NA column (default "ValyNA")
#' @param sadcat_dict Pre-computed quanteda dictionary. If NULL, calls prepare_sadcat_dictionaries()
#' @return The input data with many new columns: dictionary counts, percentages,
#'   binary indicators, direction scores, per-dimension valence, etc.
#' @export
match_dictionaries <- function(data,
                               text_col = "tv3",
                               response_col = "tv",
                               valence_col = "ValyNA",
                               sadcat_dict = NULL) {
  message("--- Stage 4: Matching dictionaries ---")

  # Get or prepare dictionaries
  if (is.null(sadcat_dict)) {
    sadcat_dict <- prepare_sadcat_dictionaries()
  }

  # ---- Prepare text for matching (tv3 -> tv4) ----
  data$tv4 <- enc2utf8(as.character(data[[text_col]]))
  data$tv4 <- tolower(data$tv4)
  data$tv4 <- gsub("-", " ", data$tv4)

  # Remove ending Ss
  delete_ending_Ss2_internal <- function(x) {
    if (is.na(x)) return(x)
    unlist(lapply(x, function(y) {
      paste(sapply(strsplit(y, ' '), SADCAT::delete_ending_Ss), collapse = ' ')
    }))
  }
  data$tv4 <- vapply(data$tv4, delete_ending_Ss2_internal, character(1))

  # ---- Tokenize and match ----
  toks <- quanteda::tokens(data$tv4, remove_numbers = FALSE,
                           remove_punct = TRUE, remove_symbols = TRUE)

  toks_dict_pre <- quanteda::tokens_lookup(toks, dictionary = sadcat_dict,
                                           nested_scope = "dictionary",
                                           exclusive = TRUE, levels = 1)
  toks_dict_df <- quanteda::convert(quanteda::dfm(toks_dict_pre), to = "data.frame")

  # Combine with token count and original data
  toks_dict_df$ntoken <- quanteda::ntoken(toks)
  toks_dict <- cbind(toks_dict_df, data)

  # ---- Identify dictionary column names (lowercase from quanteda output) ----
  # quanteda lowercases all dictionary names
  dict_cols_all <- tolower(names(sadcat_dict))

  # First and last dictionary columns for range-based operations
  first_dic <- dict_cols_all[1]
  last_dic <- dict_cols_all[length(dict_cols_all)]

  # ---- Percentages ----
  for (col in dict_cols_all) {
    toks_dict[[paste0(col, "_percent")]] <- toks_dict[[col]] / toks_dict$ntoken
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
  # For all base and non-directional dimensions
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
    toks_dict$None2y <- ifelse(is.na(row_sums), NA, ifelse(row_sums > 0, 0, 1))
  }

  # ---- Per-dimension ValyNA (NA if not in that dimension) ----
  for (dim in all_base_dims) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    valyna_dim <- paste0(dim, "_ValyNA")
    if (binary_col %in% names(toks_dict) && valence_col %in% names(toks_dict)) {
      toks_dict[[valyna_dim]] <- ifelse(toks_dict[[binary_col]] == 0,
                                        NA, toks_dict[[valence_col]])
    }
  }
  # NONE ValyNA
  if ("None2y" %in% names(toks_dict) && valence_col %in% names(toks_dict)) {
    toks_dict$NONE_ValyNA <- ifelse(toks_dict$None2y == 0, NA, toks_dict[[valence_col]])
  }

  # ---- Negation reversal for direction scores ----
  for (dim in .SADCAT_DIR_DIMS) {
    dirx_col <- paste0(dim, "_dirx")
    dirx2_col <- paste0(dim, "_dirx2")
    if (dirx_col %in% names(toks_dict)) {
      toks_dict[[dirx2_col]] <- ifelse(
        grepl(.NEGATION_PATTERN, toks_dict[[response_col]]),
        toks_dict[[dirx_col]] * -1,
        toks_dict[[dirx_col]]
      )
    }
  }

  # ---- Fix valy3 and dirx3: NA if binary==0 OR binary2 is NA ----
  # Directional dimensions: valy3 and dirx3
  for (dim in .SADCAT_DIR_DIMS) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    binary2_col <- paste0(tolower(dim), "_dic_binary2")
    valyna_dim <- paste0(dim, "_ValyNA")
    valy3_col <- paste0(dim, "_valy3")
    dirx_col <- paste0(dim, "_dirx")
    dirx3_col <- paste0(dim, "_dirx3")

    if (all(c(binary_col, binary2_col, valyna_dim) %in% names(toks_dict))) {
      toks_dict[[valy3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valyna_dim]])
    }
    if (all(c(binary_col, binary2_col, dirx_col) %in% names(toks_dict))) {
      toks_dict[[dirx3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[dirx_col]])
    }
  }

  # Non-directional dimensions: valy3 only (no dirx)
  ndir_for_valy3 <- c("Occupation", "Emotion", "Deviance", "Socialgroups",
                       "Geography", "Appearance", "Other", "OtherwFam")
  for (dim in ndir_for_valy3) {
    binary_col <- paste0(tolower(dim), "_dic_binary")
    binary2_col <- paste0(tolower(dim), "_dic_binary2")
    valyna_dim <- paste0(dim, "_ValyNA")
    valy3_col <- paste0(dim, "_valy3")

    if (all(c(binary_col, binary2_col, valyna_dim) %in% names(toks_dict))) {
      toks_dict[[valy3_col]] <- ifelse(
        toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
        NA, toks_dict[[valyna_dim]])
    }
  }

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
                               response_col = "response",
                               verbose = TRUE) {

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
    if (response_col %in% names(data)) {
      for (col in sbert_cols) {
        data[[col]] <- ifelse(is.na(data[[response_col]]), NA, data[[col]])
      }
    }

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
    if (response_col %in% names(data)) {
      for (col in gemini_cols) {
        data[[col]] <- ifelse(is.na(data[[response_col]]), NA, data[[col]])
      }
    }

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
  Seed_Vectors_names <- paste0(as.character(seed_vectors$Dictionary), ".seed")

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
      paste0("^Valy$|^ValyNA$|",
             "_dic_binary2$|^None2y$|",
             "_ValyNA$|^NONE_ValyNA$|",
             "_valy3$|_dirx3$|",
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
      dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(., na.rm = TRUE)),
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

    # Valence noNA: _valy3 columns
    valy3_cols <- grep("_valy3$", names(result), value = TRUE)
    for (col in valy3_cols) {
      result[[paste0(col, "noNA")]] <- ifelse(is.na(result[[col]]), 0, result[[col]])
    }

    if (verbose) {
      message("  Created ", length(dirx3_cols) + length(valy3_cols), " noNA columns.")
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
                              response_col = "response",
                              group_cols = c("Synonym.GroupX", "Group", "Level"),
                              stages = c("preprocess", "valence", "dictionaries",
                                         "embeddings", "seeds", "aggregate"),
                              # Preprocessing params
                              spellcheck = TRUE,
                              singularize_text = TRUE,
                              java_home = "C:\\Program Files\\Java\\jre-1.8",
                              wordnet_dict = "C:\\dict",
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
                            singularize = singularize_text,
                            java_home = java_home,
                            wordnet_dict = wordnet_dict,
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
                               response_col = "tv",
                               valence_col = "ValyNA",
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
                                        verbose = verbose)
    }
    if (save_intermediates) {
      write.csv(data, paste0(save_prefix, "_5_seeds.csv"), row.names = FALSE)
    }
  }

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
