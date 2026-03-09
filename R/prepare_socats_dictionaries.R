#' @title Prepare SOCATS Dictionaries for Quanteda Matching
#'
#' @description Transforms \code{SOCATS_Dictionaries} into a quanteda dictionary
#' object for social category matching. The result is cached in a package-level
#' environment so it only needs to be computed once per R session.
#' Preprocessing includes UTF-8 encoding, lowercasing, dash-to-space conversion,
#' lemmatization, removal of ending Ss, and punctuation/symbol removal.
#' @param socats_data The raw SOCATS dictionary data frame (default: SOCATS_Dictionaries)
#' @param cache Logical. Cache result for session? (default TRUE)
#' @return A quanteda::dictionary object with 37 social category entries
#' @export prepare_socats_dictionaries

prepare_socats_dictionaries <- function(socats_data = SOCATS_Dictionaries,
                                         cache = TRUE) {
  # Return cached version if available
  if (cache && exists("prepared_socats_dict", envir = .sadcat_cache)) {
    message("  Using cached SOCATS dictionaries.")
    return(get("prepared_socats_dict", envir = .sadcat_cache))
  }

  message("--- Preparing SOCATS dictionaries ---")

  soccats <- socats_data

  # ---- Preprocess dictionary terms ----
  soccats$values0 <- enc2utf8(as.character(soccats$word))
  soccats$values0 <- tolower(soccats$values0)

  # Lemmatize (SOCATS terms include inflected forms unlike SADCAT)
  soccats$values0 <- textstem::lemmatize_strings(soccats$values0)

  # Remove ending Ss
  delete_ending_Ss2_internal <- function(x) {
    if (is.na(x)) return(x)
    unlist(lapply(x, function(y) {
      paste(sapply(strsplit(y, ' '), delete_ending_Ss), collapse = ' ')
    }))
  }
  soccats$values0 <- vapply(soccats$values0,
                            delete_ending_Ss2_internal,
                            character(1),
                            USE.NAMES = FALSE)

  # Tokenize to remove punctuation and symbols, then reconstruct
  corpusx <- .tokenize_quanteda_text(soccats$values0,
                                     prefix = "socats_term",
                                     remove_numbers = TRUE,
                                     remove_punct = TRUE,
                                     remove_symbols = TRUE)
  soccats$values0 <- vapply(seq_along(corpusx), function(i) {
    paste(corpusx[[i]], collapse = ' ')
  }, character(1))

  # ---- Build dictionary mapping ----
  # Helper: extract values0 where a column is 1
  extract_words <- function(col_name) {
    mask <- soccats[[col_name]] == 1
    mask[is.na(mask)] <- FALSE
    vals <- soccats$values0[mask]
    vals[!is.na(vals) & vals != ""]
  }

  # Map CSV columns to dictionary entry names
  socats_map <- c(
    SOCAT_dic = "All",
    OtherID_dic = "Other.salient.ID",
    Jobs_dic = "Jobs",
    Geographies_dic = "Geography",
    Race.ethn_dic = "Race.ethn",
    Race.Geo.US.lo.Status_dic = "Racial.Geo.USlowerStatus",
    Race.Geo.US.hi.Status_dic = "Racial.Geo.UShigherStatus",
    Multiracial_dic = "Multiracial",
    White_dic = "White",
    Black_dic = "Black",
    Euro.Aus.Can.Russ_dic = "European.Australian.Canadian.Russian",
    Native.American_dic = "Native.American",
    Middle.Eastern.muslim_dic = "Middle.Eastern.muslim",
    Jewish.Israeli_dic = "Jewish.Israeli",
    East.Asian_dic = "East.Asian",
    South.Asian_dic = "South.Asian",
    SE.Asian_dic = "SE.Asian",
    Latin.American_dic = "Latin.American",
    African_dic = "African",
    US.American_dic = "US.American",
    Immigrant.foreigner_dic = "Immigrant.foreigner",
    Age_dic = "Age",
    Children_dic = "Children",
    Teenager_dic = "Teenager",
    Adult_dic = "Adult",
    Elderly_dic = "Elderly",
    Sexual.orientation_dic = "Sexual.orientation",
    SexualOR.minority_dic = "Sexual.minority",
    SexualOR.majority_dic = "Sexual.majority",
    Gender.id.and.expression_dic = "Gender.identity.and.expression",
    Men_dic = "Men",
    Women_dic = "Women",
    Nonbinary.mix.ambiguous.gender_dic = "Nonbinary.mix.ambiguous.gender",
    Cis_dic = "Cis",
    Trans_dic = "Trans",
    Other.Gender.id.express_dic = "Other.Gender.identities.or.expressions",
    Other_sexOrGenderId_dic = "Other_sexOrGenderId"
  )

  dict_list <- list()
  for (nm in names(socats_map)) {
    dict_list[[nm]] <- extract_words(socats_map[[nm]])
  }

  # Create quanteda dictionary
  result <- quanteda::dictionary(dict_list)

  # Cache
  if (cache) {
    assign("prepared_socats_dict", result, envir = .sadcat_cache)
  }

  message("  SOCATS dictionary preparation complete. ", length(dict_list), " categories.")
  return(result)
}
