#' @title Spell-check Text with SADCAT Dictionary
#'
#' @description Spell-check text using SADCAT dictionary, WordNet, and hunspell.
#' Checks if word is in dictionary, correctly spelled, or in WordNet.
#' If not, uses hunspell suggestions ranked by edit distance and bigram context.
#' Handles multi-word inputs by spell-checking each word individually.
#' Requires rJava, wordnet, hunspell.
#' @param raw A single character string to spell-check
#' @param dict_tv Character vector of known dictionary words
#' @param rawlist Character vector of all raw values (for matching suggestions)
#' @param bigram_freqs Bigram frequency environment from .build_bigram_freqs() (optional)
#' @return Spell-checked version of the input
#' @export Spellcheck2

Spellcheck2 <- function(raw, dict_tv = All.steps_Dictionaries$tv, rawlist,
                         bigram_freqs = NULL) {
  tryCatch({
    if (is.na(raw) || raw == "NA") {
      return("na")
    }
    # NA markers must short-circuit before hunspell, which otherwise turns
    # "n/a" -> "naan" and "?" -> "s". Catch:
    #   (1) inputs with no letters and no digits (e.g., "?", "--", "...")
    #   (2) explicit n/a variants
    raw_trim_lower <- tolower(trimws(raw))
    if (nchar(gsub("[^[:alnum:]]", "", raw_trim_lower)) == 0L ||
        raw_trim_lower %in% c("n/a", "n.a", "n.a.", "n\\a", "n_a",
                               "#n/a", "n / a", "n. a.")) {
      return("na")
    }
    # If the whole string is already known or correctly spelled, return as-is
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
      result <- clean_naresponses(result)
      return(tolower(result))
    }
    # Single word
    result <- .spellcheck_single_word(raw, dict_tv = dict_tv, rawlist = rawlist,
                                       bigram_freqs = bigram_freqs)
    result <- clean_naresponses(result)
    return(tolower(result))
  }, error = function(s) {
    return("*******ERROR")
  })
}


#' Spell-check a single word using edit distance and bigram context
#' @param word A single word
#' @param dict_tv Character vector of known dictionary words
#' @param rawlist Character vector of all raw values
#' @param left_word Word to the left (for bigram context), or NULL
#' @param right_word Word to the right (for bigram context), or NULL
#' @param bigram_freqs Bigram frequency environment, or NULL
#' @return Corrected word
#' @keywords internal
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

  # Take up to 10 suggestions
  suggestions <- utils::head(suggestions, 10)

  # Compute edit distances
  dists <- as.integer(utils::adist(word, suggestions))

  # Compute bigram context scores (0 if no bigram data)
  bg_scores <- rep(0L, length(suggestions))
  if (!is.null(bigram_freqs)) {
    bg_scores <- .score_bigram_context(suggestions, left_word, right_word,
                                        bigram_freqs)
  }

  # Rawlist membership bonus
  in_rawlist <- suggestions %in% rawlist

  ranked_idx <- .rank_spellcheck_candidates(
    word = word,
    suggestions = suggestions,
    dists = dists,
    bg_scores = bg_scores,
    in_rawlist = in_rawlist
  )

  suggestions[ranked_idx[1]]
}


#' Rank spell-check suggestions using SADCAT heuristics
#' @param word Original misspelled word
#' @param suggestions Candidate corrections
#' @param dists Edit distances from `word` to each candidate
#' @param bg_scores Bigram context scores
#' @param in_rawlist Logical vector indicating candidate presence in rawlist
#' @return Integer indices of suggestions in best-to-worst order
#' @keywords internal
.rank_spellcheck_candidates <- function(word, suggestions, dists, bg_scores,
                                         in_rawlist) {
  word_len <- nchar(word, type = "chars")
  sug_len <- nchar(suggestions, type = "chars")
  len_delta <- abs(sug_len - word_len)

  # Avoid over-truncation: when there is a non-short tie at min edit distance,
  # demote 1-2 character candidates (e.g., "rde" -> "rd" instead of "rude").
  short_penalty <- integer(length(suggestions))
  if (!is.na(word_len) && word_len >= 3L) {
    min_dist <- min(dists)
    min_dist_mask <- dists == min_dist
    short_at_min <- min_dist_mask & (sug_len <= 2L)
    if (any(min_dist_mask & !short_at_min)) {
      short_penalty[short_at_min] <- 1L
    }
  }

  rank_df <- data.frame(
    idx = seq_along(suggestions),
    dist = dists,
    short_penalty = short_penalty,
    bg = bg_scores,
    inraw = in_rawlist,
    len_delta = len_delta,
    stringsAsFactors = FALSE
  )

  rank_df <- rank_df[order(
    rank_df$dist,
    rank_df$short_penalty,
    -rank_df$bg,
    -rank_df$inraw,
    rank_df$len_delta,
    rank_df$idx
  ), ]

  rank_df$idx
}


#' Build bigram frequency table from a corpus of text values
#' @param corpus Character vector of text values (e.g., all unique responses)
#' @return An environment mapping "word1 word2" (tab-separated) keys to integer counts
#' @keywords internal
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


#' Score suggestion candidates by bigram co-occurrence with neighbors
#' @param candidates Character vector of candidate words
#' @param left_word Word to the left, or NULL
#' @param right_word Word to the right, or NULL
#' @param bigram_freqs Environment from .build_bigram_freqs()
#' @return Integer vector of scores (higher = better context fit)
#' @keywords internal
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


#' Spell-check texts using Gemini LLM
#' @param texts Character vector of text values to spell-check
#' @param api_key Gemini API key
#' @param model Gemini model name (default "gemini-2.0-flash")
#' @param batch_size Number of texts per API call (default 50)
#' @param sleep Seconds between batches (default 5)
#' @param context_prompt System prompt providing task context for Gemini
#' @param verbose Print progress? (default TRUE)
#' @return Character vector of corrected texts (same length as input)
#' @keywords internal
.spellcheck_gemini <- function(texts,
                                api_key,
                                model = "gemini-2.0-flash",
                                batch_size = 50L,
                                sleep = 5,
                                context_prompt = NULL,
                                verbose = TRUE) {
  if (!.has_namespace("reticulate")) {
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

  genai <- .reticulate_import("google.genai")
  types <- .reticulate_import("google.genai.types")
  client <- genai$Client(api_key = api_key)

  corrected <- character(length(texts))
  n_batches <- ceiling(length(texts) / batch_size)

  for (b in seq_len(n_batches)) {
    start_idx <- (b - 1) * batch_size + 1
    end_idx <- min(b * batch_size, length(texts))
    batch <- texts[start_idx:end_idx]

    if (verbose) message("    Gemini spellcheck batch ", b, " / ", n_batches,
                         " (items ", start_idx, "-", end_idx, ")")

    # Number each line so we can parse the response reliably
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
      # Parse numbered lines from response
      lines <- strsplit(response_text, "\n")[[1]]
      lines <- trimws(lines)
      lines <- lines[nchar(lines) > 0]
      # Strip leading "N. " numbering if present
      lines <- sub("^\\d+\\.\\s*", "", lines)

      if (length(lines) == length(batch)) {
        corrected[start_idx:end_idx] <- tolower(lines)
      } else {
        # Length mismatch, fall back to originals for this batch
        warning("  Gemini batch ", b, ": expected ", length(batch),
                " lines but got ", length(lines), ". Using originals.")
        corrected[start_idx:end_idx] <- batch
      }
    } else {
      corrected[start_idx:end_idx] <- batch
    }

    # Rate limit (skip after last batch)
    if (b < n_batches) {
      if (verbose) message("    Sleeping ", sleep, " seconds (rate limit)...")
      .sleep_seconds(sleep)
    }
  }

  corrected
}
