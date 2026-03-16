#' NaN-aware replacement for data frames
#' @param df A data frame
#' @return Data frame with NaN values replaced by NA
#' @keywords internal
replace_nan_with_na <- function(df) {
  is_nan_df <- do.call(cbind, lapply(df, is.nan))
  df[is_nan_df] <- NA
  df
}

#' Validate required columns before running a pipeline stage
#' @param data A data.frame
#' @param cols Required column names
#' @param context Short stage/function label for the error message
#' @return Invisibly returns TRUE
#' @keywords internal
.require_data_columns <- function(data, cols, context) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      context,
      " requires missing column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Prepare character input for quanteda tokenization with stable docnames
#' @param x A character vector
#' @param prefix Prefix for generated docnames
#' @return Character vector with explicit unique names for quanteda
#' @keywords internal
.prepare_quanteda_text <- function(x, prefix = "doc") {
  x <- enc2utf8(as.character(x))
  x <- unname(x)
  names(x) <- paste0(prefix, "_", seq_along(x))
  x
}

#' Tokenize character input with stable row-based docnames
#' @param x A character vector
#' @param prefix Prefix for generated docnames
#' @param ... Passed to \code{quanteda::tokens()}
#' @return A quanteda tokens object
#' @keywords internal
.tokenize_quanteda_text <- function(x, prefix = "doc", ...) {
  quanteda::tokens(.prepare_quanteda_text(x, prefix = prefix), ...)
}

#' Check namespace availability
#' @param pkg Package name
#' @return Logical scalar
#' @keywords internal
.has_namespace <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

#' Wrapper around reticulate::import for testability
#' @param module Python module name
#' @return Imported Python module
#' @keywords internal
.reticulate_import <- function(module) {
  reticulate::import(module)
}

#' Wrapper around reticulate::py_to_r for testability
#' @param x Python object
#' @return Converted R object
#' @keywords internal
.reticulate_py_to_r <- function(x) {
  reticulate::py_to_r(x)
}

#' Wrapper around Sys.sleep for testability
#' @param seconds Numeric sleep duration
#' @return NULL invisibly
#' @keywords internal
.sleep_seconds <- function(seconds) {
  Sys.sleep(seconds)
}

#' Normalize and validate a directory-like path candidate
#' @param path Candidate path
#' @return Normalized path string or NULL
#' @keywords internal
.normalize_existing_dir <- function(path) {
  if (is.null(path) || length(path) == 0) {
    return(NULL)
  }
  path <- as.character(path)[1]
  if (is.na(path) || !nzchar(path)) {
    return(NULL)
  }
  path <- path.expand(path)
  path <- gsub("\\\\", "/", path)
  path <- sub("/+$", "", path)
  if (!dir.exists(path)) {
    return(NULL)
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Check whether a directory looks like a Java home
#' @param path Candidate Java home directory
#' @return Logical scalar
#' @keywords internal
.has_java_home <- function(path) {
  if (is.null(path) || !nzchar(path) || !dir.exists(path)) {
    return(FALSE)
  }
  file.exists(file.path(path, "bin", "java")) || file.exists(file.path(path, "bin", "java.exe"))
}

#' Normalize candidate Java home path
#' @param path Candidate Java path/home/bin/java executable
#' @return Normalized Java home directory or NULL
#' @keywords internal
.normalize_java_candidate <- function(path) {
  if (is.null(path) || length(path) == 0) {
    return(NULL)
  }
  path <- as.character(path)[1]
  if (is.na(path) || !nzchar(path)) {
    return(NULL)
  }

  path <- path.expand(path)
  path <- gsub("\\\\", "/", path)
  path <- sub("/+$", "", path)

  if (file.exists(path) && !dir.exists(path)) {
    b <- tolower(basename(path))
    if (b %in% c("java", "java.exe")) {
      path <- dirname(dirname(path))
    } else {
      return(NULL)
    }
  } else if (dir.exists(path)) {
    b <- tolower(basename(path))
    if (b == "bin") {
      path <- dirname(path)
    } else if (b == "contents" && dir.exists(file.path(path, "Home"))) {
      path <- file.path(path, "Home")
    } else if (dir.exists(file.path(path, "Contents", "Home"))) {
      path <- file.path(path, "Contents", "Home")
    }
  } else {
    return(NULL)
  }

  .normalize_existing_dir(path)
}

#' Resolve JAVA_HOME from explicit path, env vars, PATH, or common install paths
#' @param java_home Optional explicit JAVA_HOME-like directory
#' @return Resolved JAVA_HOME directory
#' @keywords internal
.resolve_java_home <- function(java_home = NULL) {
  path_hits <- Sys.which(c("java", "java.exe"))
  path_hits <- unname(path_hits[nzchar(path_hits)])

  path_candidates <- character(0)
  if (length(path_hits) > 0) {
    path_candidates <- c(path_hits[1], dirname(path_hits[1]), dirname(dirname(path_hits[1])))
  }

  mac_vm_homes <- character(0)
  mac_vm_root <- "/Library/Java/JavaVirtualMachines"
  if (dir.exists(mac_vm_root)) {
    mac_vm_dirs <- list.dirs(mac_vm_root, recursive = FALSE, full.names = TRUE)
    if (length(mac_vm_dirs) > 0) {
      mac_vm_homes <- file.path(mac_vm_dirs, "Contents", "Home")
    }
  }

  linux_jvm_dirs <- character(0)
  linux_jvm_root <- "/usr/lib/jvm"
  if (dir.exists(linux_jvm_root)) {
    linux_jvm_dirs <- list.dirs(linux_jvm_root, recursive = FALSE, full.names = TRUE)
  }

  candidates <- c(
    java_home,
    Sys.getenv("JAVA_HOME", ""),
    path_candidates,
    "C:/Program Files/Java/jre-1.8",
    "C:/Program Files/Java/jre",
    "C:/Program Files/Java/jdk",
    "C:/Program Files/Java",
    "C:/Program Files (x86)/Java/jre-1.8",
    "C:/Program Files (x86)/Java/jre",
    "C:/Program Files (x86)/Java",
    "/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home",
    "/usr/local/opt/openjdk/libexec/openjdk.jdk/Contents/Home",
    "/Library/Java/Home",
    "/usr/lib/jvm/default-java",
    "/usr/lib/jvm/java-17-openjdk-amd64",
    "/usr/lib/jvm/java-11-openjdk-amd64",
    mac_vm_homes,
    linux_jvm_dirs
  )

  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  candidates <- unique(candidates)
  for (candidate in candidates) {
    normalized <- .normalize_java_candidate(candidate)
    if (!is.null(normalized) && .has_java_home(normalized)) {
      return(normalized)
    }
    if (!is.null(normalized) && dir.exists(normalized)) {
      subdirs <- list.dirs(normalized, recursive = FALSE, full.names = TRUE)
      if (length(subdirs) > 0) {
        for (subdir in subdirs) {
          sub_norm <- .normalize_java_candidate(subdir)
          if (!is.null(sub_norm) && .has_java_home(sub_norm)) {
            return(sub_norm)
          }
        }
      }
    }
  }

  stop(
    "Java runtime not found for spell-checking. ",
    "Set `java_home` or `JAVA_HOME` to a Java home directory that contains `bin/java`."
  )
}

#' Check whether a directory looks like a WordNet dict directory
#' @param path Candidate dict directory
#' @return Logical scalar
#' @keywords internal
.has_wordnet_dict <- function(path) {
  if (is.null(path) || !nzchar(path) || !dir.exists(path)) {
    return(FALSE)
  }
  file.exists(file.path(path, "index.noun"))
}

#' Normalize candidate WordNet directory
#' @param path Candidate dict path or parent folder
#' @return Normalized dict directory or NULL
#' @keywords internal
.normalize_wordnet_candidate <- function(path) {
  base <- .normalize_existing_dir(path)
  if (is.null(base)) {
    return(NULL)
  }
  if (tolower(basename(base)) == "dict") {
    return(base)
  }
  dict_child <- .normalize_existing_dir(file.path(base, "dict"))
  if (!is.null(dict_child)) {
    return(dict_child)
  }
  base
}

#' Resolve WordNet dict directory from explicit path, env vars, or common locations
#' @param wordnet_dict Optional explicit WordNet dict path
#' @return Resolved WordNet dict directory
#' @keywords internal
.resolve_wordnet_dict <- function(wordnet_dict = NULL) {
  wnsearchdir <- Sys.getenv("WNSEARCHDIR", "")
  wn_home <- Sys.getenv("WNHOME", "")

  candidates <- c(
    wordnet_dict,
    Sys.getenv("SADCAT_WORDNET_DICT", ""),
    Sys.getenv("WORDNET_DICT", ""),
    wnsearchdir,
    wn_home,
    file.path(wn_home, "dict"),
    "C:/dict",
    "C:/WordNet/2.1/dict",
    "C:/WordNet/3.0/dict",
    "C:/Program Files/WordNet/2.1/dict",
    "C:/Program Files/WordNet/3.0/dict",
    "C:/Program Files (x86)/WordNet/2.1/dict",
    "C:/Program Files (x86)/WordNet/3.0/dict",
    "/usr/share/wordnet",
    "/usr/local/share/wordnet",
    "/opt/local/share/wordnet",
    "/opt/homebrew/share/wordnet",
    "/usr/local/WordNet-3.0/dict"
  )

  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  candidates <- unique(candidates)
  for (candidate in candidates) {
    normalized <- .normalize_wordnet_candidate(candidate)
    if (!is.null(normalized) && .has_wordnet_dict(normalized)) {
      return(normalized)
    }
  }

  stop(
    "WordNet dictionary not found for spell-checking. ",
    "Set `wordnet_dict`, `SADCAT_WORDNET_DICT`, `WNSEARCHDIR`, or `WNHOME` ",
    "to the WordNet `dict` directory."
  )
}

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
    sent <- dplyr::filter(
      tidytext::get_sentiments("nrc"),
      sentiment == "positive" | sentiment == "negative"
    )
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
    sent <- dplyr::filter(
      tidytext::get_sentiments("loughran"),
      sentiment == "positive" | sentiment == "negative"
    )
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

  # Binarize — find pos/neg columns from actual dfm result (case-insensitive)
  result_cols <- setdiff(colnames(result), "doc_id")
  if (is_lexicoder) {
    neg_col <- "negative"
    pos_col <- "positive"
  } else {
    neg_col <- grep("^negative", result_cols, value = TRUE, ignore.case = TRUE)[1]
    pos_col <- grep("^positive", result_cols, value = TRUE, ignore.case = TRUE)[1]
  }

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
#' Mask selected columns to NA where response text is missing
#' @param data A data frame
#' @param response_col Column indicating whether the response is missing
#' @param cols_or_patterns Character vector of exact column names and/or regex patterns
#' @return Data frame with selected columns masked to NA for missing responses
#' @keywords internal
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

