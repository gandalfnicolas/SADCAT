#' @title Preprocess Text Responses
#'
#' @description Preprocess text responses: lowercase, spell-check, singularize.
#' Takes a data frame with a raw text column and produces cleaned versions:
#' \code{tv} (lowercased), \code{tv2} (spell-checked), \code{tv3} (singularized).
#' @param data A data.frame containing text responses
#' @param text_col Name of the column with raw text (default "responsex")
#' @param spellcheck Logical. Run spell-checking? (default TRUE)
#' @param spellcheck_method Method for spell-checking: "hunspell" (default, uses
#'   Java + WordNet + hunspell with edit-distance ranking and bigram context) or
#'   "gemini" (uses Gemini LLM for context-aware correction, requires API key).
#' @param singularize Logical. Run singularization? (default TRUE)
#' @param java_home Optional Java home path. If NULL or not found, auto-detects from
#' environment, PATH, and common install locations. Only used when spellcheck_method = "hunspell".
#' @param wordnet_dict Optional WordNet dictionary path. If NULL or not found,
#' auto-detects from environment and common install locations. Only used when
#' spellcheck_method = "hunspell".
#' @param gemini_spellcheck_key Gemini API key for LLM spell-checking. If NULL,
#'   reads from Sys.getenv("GEMINI_API_KEY"). Only used when spellcheck_method = "gemini".
#' @param gemini_spellcheck_model Gemini model for spell-checking (default "gemini-2.0-flash").
#' @param gemini_spellcheck_context Custom system prompt providing context for Gemini
#'   spell-checking. If NULL, uses a default prompt describing the text as social
#'   target descriptions.
#' @param verbose Print progress? (default TRUE)
#' @return The input data with columns \code{tv}, \code{tv2}, \code{tv3} added
#' @export preprocess_text

preprocess_text <- function(data,
                            text_col = "responsex",
                            spellcheck = TRUE,
                            spellcheck_method = "hunspell",
                            singularize = TRUE,
                            java_home = NULL,
                            wordnet_dict = NULL,
                            gemini_spellcheck_key = NULL,
                            gemini_spellcheck_model = "gemini-2.0-flash",
                            gemini_spellcheck_context = NULL,
                            verbose = TRUE) {
  if (verbose) message("--- Stage 1: Preprocessing text ---")
  .require_data_columns(data, text_col, "preprocess_text()")

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
      # Filter out NAs for the API call
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
      resolved_java_home <- .resolve_java_home(java_home)
      resolved_wordnet_dict <- .resolve_wordnet_dict(wordnet_dict)
      if (verbose) {
        message("    Using JAVA_HOME: ", resolved_java_home)
        message("    Using WordNet dictionary: ", resolved_wordnet_dict)
      }

      Sys.setenv(JAVA_HOME = resolved_java_home)
      wordnet::setDict(resolved_wordnet_dict)

      # Build bigram frequencies for context-aware correction (50+ entries)
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
