#' @title Match Text Against SADCAT and/or SOCATS Dictionaries
#'
#' @description Match text responses against SADCAT stereotype dictionaries
#' and/or SOCATS social category dictionaries. Tokenizes response text,
#' matches against dictionaries, and computes binary indicators, percentages,
#' direction scores, and per-dimension valence/direction columns.
#' @param data A data.frame with preprocessed text and valence scores
#' @param text_col Column with singularized text to match (default "tv3")
#' @param response_col Column with original lowercased text for NA checks (default "tv")
#' @param valence_col Name of combined valence-NA column (default "ValyNA")
#' @param valence_raw_col Name of combined valence column without NA-gating (default "Valy")
#' @param sadcat_dict Pre-computed SADCAT quanteda dictionary. If NULL and
#'   \code{sadcat = TRUE} (default), calls \code{prepare_sadcat_dictionaries()}.
#'   Set to FALSE to skip SADCAT matching entirely.
#' @param socats_dict Pre-computed SOCATS quanteda dictionary. If NULL and
#'   \code{socats = TRUE}, calls \code{prepare_socats_dictionaries()}.
#' @param socats Logical. Match against SOCATS dictionaries? (default FALSE)
#' @return The input data with many new columns: dictionary counts, percentages,
#'   binary indicators, direction scores, per-dimension valence, etc.
#' @export match_dictionaries

match_dictionaries <- function(data,
                               text_col = "tv3",
                               response_col = "tv",
                               valence_col = "ValyNA",
                               valence_raw_col = "Valy",
                               sadcat_dict = NULL,
                               socats_dict = NULL,
                               socats = FALSE) {
  message("--- Stage 4: Matching dictionaries ---")

  # Determine which dictionaries to match
  do_sadcat <- !identical(sadcat_dict, FALSE)
  do_socats <- socats || !is.null(socats_dict)

  # Get or prepare SADCAT dictionaries
  if (do_sadcat && is.null(sadcat_dict)) {
    sadcat_dict <- prepare_sadcat_dictionaries()
  }

  # Get or prepare SOCATS dictionaries
  if (do_socats && is.null(socats_dict)) {
    socats_dict <- prepare_socats_dictionaries()
  }

  # ---- Prepare text for matching (tv3 -> tv4) ----
  data$tv4 <- enc2utf8(as.character(data[[text_col]]))
  data$tv4 <- tolower(data$tv4)
  data$tv4 <- gsub("-", " ", data$tv4)

  # Remove ending Ss
  delete_ending_Ss2_internal <- function(x) {
    if (is.na(x)) return(x)
    unlist(lapply(x, function(y) {
      paste(sapply(strsplit(y, ' '), delete_ending_Ss), collapse = ' ')
    }))
  }
  data$tv4 <- vapply(data$tv4, delete_ending_Ss2_internal, character(1))

  # ---- Tokenize (shared for both dictionaries) ----
  toks <- quanteda::tokens(data$tv4, remove_numbers = FALSE,
                           remove_punct = TRUE, remove_symbols = TRUE)

  toks_dict <- data

  # ============================================================
  # SADCAT MATCHING
  # ============================================================
  if (do_sadcat) {
    message("  Matching SADCAT dictionaries...")

    toks_dict_pre <- quanteda::tokens_lookup(toks, dictionary = sadcat_dict,
                                             nested_scope = "dictionary",
                                             exclusive = TRUE, levels = 1)
    toks_dict_df <- quanteda::convert(quanteda::dfm(toks_dict_pre), to = "data.frame")

    # Add token count and combine
    toks_dict_df$ntoken <- quanteda::ntoken(toks)
    toks_dict <- cbind(toks_dict_df, toks_dict)

    # Identify SADCAT dictionary column names (quanteda lowercases them)
    dict_cols_all <- tolower(names(sadcat_dict))

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

    # ---- Per-dimension Valy / ValyNA (NA if not in that dimension) ----
    for (dim in all_base_dims) {
      binary_col <- paste0(tolower(dim), "_dic_binary")
      valy_dim <- paste0(dim, "_Valy")
      valyna_dim <- paste0(dim, "_ValyNA")

      if (binary_col %in% names(toks_dict) && valence_raw_col %in% names(toks_dict)) {
        toks_dict[[valy_dim]] <- ifelse(toks_dict[[binary_col]] == 0,
                                        NA, toks_dict[[valence_raw_col]])
      }
      if (binary_col %in% names(toks_dict) && valence_col %in% names(toks_dict)) {
        toks_dict[[valyna_dim]] <- ifelse(toks_dict[[binary_col]] == 0,
                                          NA, toks_dict[[valence_col]])
      }
    }

    # NONE ValyNA
    if ("None2y" %in% names(toks_dict) && valence_col %in% names(toks_dict)) {
      toks_dict$NONE_ValyNA <- ifelse(toks_dict$None2y == 0, NA, toks_dict[[valence_col]])
    }

    adjusted_dir <- .compute_adjusted_direction_scores(data[[text_col]], sadcat_dict)
    for (col in names(adjusted_dir)) {
      toks_dict[[col]] <- adjusted_dir[[col]]
    }

    # ---- Fix valy3/valyNA3 and dirx3: NA if binary==0 OR binary2 is NA ----
    # Directional dimensions: valy3, valyNA3 and dirx3
    for (dim in .SADCAT_DIR_DIMS) {
      binary_col <- paste0(tolower(dim), "_dic_binary")
      binary2_col <- paste0(tolower(dim), "_dic_binary2")
      valy_dim <- paste0(dim, "_Valy")
      valyna_dim <- paste0(dim, "_ValyNA")
      valy3_col <- paste0(dim, "_valy3")
      valyna3_col <- paste0(dim, "_valyNA3")
      dirx_col <- paste0(dim, "_dirx")
      dirx2_col <- paste0(dim, "_dirx2")
      dirx3_col <- paste0(dim, "_dirx3")

      if (all(c(binary_col, binary2_col, valy_dim) %in% names(toks_dict))) {
        toks_dict[[valy3_col]] <- ifelse(
          toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
          NA, toks_dict[[valy_dim]])
      }
      if (all(c(binary_col, binary2_col, valyna_dim) %in% names(toks_dict))) {
        toks_dict[[valyna3_col]] <- ifelse(
          toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
          NA, toks_dict[[valyna_dim]])
      }
      if (all(c(binary_col, binary2_col, dirx_col) %in% names(toks_dict))) {
        toks_dict[[dirx3_col]] <- ifelse(
          toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
          NA, toks_dict[[dirx_col]])
      }
    }

    # Non-directional dimensions: valy3 and valyNA3
    ndir_for_valy3 <- c("Occupation", "Emotion", "Deviance", "Socialgroups",
                         "Geography", "Appearance", "Other", "OtherwFam")
    for (dim in ndir_for_valy3) {
      binary_col <- paste0(tolower(dim), "_dic_binary")
      binary2_col <- paste0(tolower(dim), "_dic_binary2")
      valy_dim <- paste0(dim, "_Valy")
      valyna_dim <- paste0(dim, "_ValyNA")
      valy3_col <- paste0(dim, "_valy3")
      valyna3_col <- paste0(dim, "_valyNA3")

      if (all(c(binary_col, binary2_col, valy_dim) %in% names(toks_dict))) {
        toks_dict[[valy3_col]] <- ifelse(
          toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
          NA, toks_dict[[valy_dim]])
      }
      if (all(c(binary_col, binary2_col, valyna_dim) %in% names(toks_dict))) {
        toks_dict[[valyna3_col]] <- ifelse(
          toks_dict[[binary_col]] == 0 | is.na(toks_dict[[binary2_col]]),
          NA, toks_dict[[valyna_dim]])
      }
    }

    message("  SADCAT matching complete.")
  }

  # ============================================================
  # SOCATS MATCHING
  # ============================================================
  if (do_socats) {
    message("  Matching SOCATS dictionaries...")

    toks_socats_pre <- quanteda::tokens_lookup(toks, dictionary = socats_dict,
                                               nested_scope = "dictionary",
                                               exclusive = TRUE, levels = 1)
    toks_socats_df <- quanteda::convert(quanteda::dfm(toks_socats_pre), to = "data.frame")

    # Add token counts if not already present from SADCAT matching
    if (!"ntoken" %in% names(toks_dict)) {
      toks_socats_df$ntoken_socats <- quanteda::ntoken(toks)
    }

    # Remove doc_id to avoid duplication when binding
    toks_socats_df$doc_id <- NULL
    toks_dict <- cbind(toks_dict, toks_socats_df)

    # Identify SOCATS dictionary column names (quanteda lowercases them)
    socats_cols <- tolower(names(socats_dict))

    # ---- Assume numbers are ages ----
    if ("age_dic" %in% names(toks_dict)) {
      toks_dict$age_dic <- ifelse(grepl("\\d", toks_dict[[response_col]]),
                                   1, toks_dict$age_dic)
    }

    # ---- Binary indicators ----
    for (col in socats_cols) {
      toks_dict[[paste0(col, "_binary")]] <- ifelse(toks_dict[[col]] > 0, 1, 0)
    }

    # ---- SOCATS direction indicators (custom logic) ----
    toks_dict <- compute_socats_directions(toks_dict)

    message("  SOCATS matching complete.")
  }

  # Ensure dictionary-derived valence/direction families are NA when response is missing
  toks_dict <- .mask_missing_response_cols(
    toks_dict,
    response_col,
    cols_or_patterns = c(
      "_Valy$", "_ValyNA$", "_valy3$", "_valyNA3$",
      "_dirx$", "_dirx2$", "_dirx3$"
    )
  )

  # Replace NaN with NA
  toks_dict <- replace_nan_with_na(toks_dict)

  message("  Dictionary matching complete.")
  return(toks_dict)
}


#' Compute SOCATS-specific direction indicators
#' @param data Data frame with SOCATS binary columns
#' @return Data frame with direction columns added
#' @keywords internal
compute_socats_directions <- function(data) {
  # Age direction: ordinal (1=children, 2=teenager, 3=adult, 4=elderly)
  if (all(c("children_dic", "teenager_dic", "adult_dic", "elderly_dic") %in% names(data))) {
    data$Age_dir <- ifelse(data$children_dic > 0, 1,
                    ifelse(data$teenager_dic > 0, 2,
                    ifelse(data$adult_dic > 0, 3,
                    ifelse(data$elderly_dic > 0, 4, NA))))
  }

  # Race/Geography status direction: high - low
  hi_col <- "race.geo.us.hi.status_dic_binary"
  lo_col <- "race.geo.us.lo.status_dic_binary"
  if (all(c(hi_col, lo_col) %in% names(data))) {
    data$RaceGeo_dir <- data[[hi_col]] - data[[lo_col]]
  }

  # Sexual orientation direction: -1=majority, +1=minority/other
  maj_col <- "sexualor.majority_dic"
  min_col <- "sexualor.minority_dic"
  oth_col <- "other_sexorgenderid_dic"
  if (all(c(maj_col, min_col, oth_col) %in% names(data))) {
    data$SexualOr_dir <- ifelse(data[[maj_col]] > 0, -1,
                         ifelse(data[[min_col]] > 0 | data[[oth_col]] > 0, 1, NA))
  }

  # Gender direction: Women vs Men (-1=men, +1=women)
  if (all(c("men_dic", "women_dic") %in% names(data))) {
    data$Gender_dir.WvM <- ifelse(data$men_dic > 0, -1,
                           ifelse(data$women_dic > 0, 1, NA))
  }

  # Gender direction: Cis vs not (+1=cis, -1=trans/nonbinary/other)
  cis_col <- "cis_dic"
  trans_col <- "trans_dic"
  other_g_col <- "other.gender.id.express_dic"
  nb_col <- "nonbinary.mix.ambiguous.gender_dic"
  if (all(c(cis_col, trans_col, other_g_col, nb_col) %in% names(data))) {
    data$Gender_dir.Cisvnot <- ifelse(data[[cis_col]] > 0, 1,
                               ifelse(data[[trans_col]] > 0 |
                                      data[[other_g_col]] > 0 |
                                      data[[nb_col]] > 0, -1, NA))
  }

  return(data)
}
