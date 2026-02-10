#' @title Prepare SADCAT Dictionaries for Quanteda Matching
#'
#' @description Transforms \code{All.steps_Dictionaries} into a quanteda dictionary
#' object. The result is cached in a package-level environment so it only needs
#' to be computed once per R session.
#' @param pre_dictionaries The raw SADCAT dictionary data (default: All.steps_Dictionaries)
#' @param cache Logical. Cache result for session? (default TRUE)
#' @return A quanteda::dictionary object with ~33 categories
#' @export prepare_sadcat_dictionaries

prepare_sadcat_dictionaries <- function(pre_dictionaries = All.steps_Dictionaries,
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
      paste(sapply(strsplit(y, ' '), delete_ending_Ss), collapse = ' ')
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
