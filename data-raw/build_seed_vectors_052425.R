#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(pkgload)
  library(usethis)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  }
  normalizePath("data-raw/build_seed_vectors_052425.R", winslash = "/", mustWork = TRUE)
}

script_path <- get_script_path()
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

message("Loading SADCAT package code from repo...")
pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)

seed_words_path <- file.path("data-raw", "seed_words_052425.csv")
if (!file.exists(seed_words_path)) {
  stop("Missing seed words file: ", seed_words_path)
}

seed_raw <- utils::read.csv(
  seed_words_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

drop_cols <- c("Sum", grep("^Sentence", names(seed_raw), value = TRUE))
drop_cols <- unique(drop_cols[drop_cols %in% names(seed_raw)])
if (length(drop_cols) > 0) {
  seed_raw <- seed_raw[, setdiff(names(seed_raw), drop_cols), drop = FALSE]
}

required_cols <- c("term", "term_context")
missing_required <- setdiff(required_cols, names(seed_raw))
if (length(missing_required) > 0) {
  stop("Missing required columns in seed CSV: ", paste(missing_required, collapse = ", "))
}

seed_raw <- seed_raw[!is.na(seed_raw$term) & nzchar(seed_raw$term), , drop = FALSE]
if (anyDuplicated(seed_raw$term)) {
  dup_terms <- unique(seed_raw$term[duplicated(seed_raw$term)])
  stop(
    "Duplicate `term` entries found in seed CSV. ",
    "Each term must be unique. Duplicates: ",
    paste(utils::head(dup_terms, 20), collapse = ", ")
  )
}

seed_raw$term_context <- ifelse(
  is.na(seed_raw$term_context) | !nzchar(seed_raw$term_context),
  seed_raw$term,
  seed_raw$term_context
)

metadata_cols <- c("term", "term_adapted", "term_context")
dir_cols_all <- grep("_dir$", names(seed_raw), value = TRUE)

is_binary_indicator <- function(x) {
  vals <- unique(x[!is.na(x)])
  length(vals) > 0 && all(vals %in% c(0, 1))
}

is_direction_indicator <- function(x) {
  vals <- unique(x[!is.na(x)])
  length(vals) > 0 && all(vals %in% c(-1, 0, 1)) && any(vals %in% c(-1, 1))
}

candidate_base <- setdiff(names(seed_raw), c(metadata_cols, dir_cols_all))
base_cols <- candidate_base[vapply(seed_raw[candidate_base], is_binary_indicator, logical(1))]
base_cols <- base_cols[vapply(seed_raw[base_cols], function(x) any(x == 1, na.rm = TRUE), logical(1))]

dir_cols <- dir_cols_all[vapply(seed_raw[dir_cols_all], is_direction_indicator, logical(1))]

if (length(base_cols) == 0) stop("No base dictionary columns detected in seed CSV.")
if (length(dir_cols) == 0) stop("No valid *_dir columns detected in seed CSV.")

message("Base dictionary columns: ", length(base_cols))
message("Directional columns: ", length(dir_cols))

build_groups <- function(seed_df, base_columns, direction_columns) {
  base_groups <- setNames(
    lapply(base_columns, function(col) {
      unique(seed_df$term[seed_df[[col]] == 1])
    }),
    base_columns
  )

  dir_groups <- list()
  for (col in direction_columns) {
    stem <- sub("_dir$", "", col)
    hi_terms <- unique(seed_df$term[seed_df[[col]] == 1])
    lo_terms <- unique(seed_df$term[seed_df[[col]] == -1])
    dir_groups[[stem]] <- list(
      hi_terms = hi_terms,
      lo_terms = lo_terms
    )
  }

  list(base_groups = base_groups, dir_groups = dir_groups)
}

embed_terms <- function(seed_df,
                        text_source = c("term", "term_context"),
                        sbert_model = "paraphrase-mpnet-base-v2",
                        gemini_model = "gemini-embedding-001",
                        gemini_task_type = "SEMANTIC_SIMILARITY",
                        gemini_batch_size = 100L,
                        gemini_sleep = 1,
                        verbose = TRUE) {
  text_source <- match.arg(text_source)

  embed_input <- seed_df[, c("term", text_source), drop = FALSE]
  names(embed_input)[2] <- "seed_text"
  embed_input$response_flag <- embed_input$seed_text

  embedded <- compute_embeddings(
    embed_input,
    text_col = "seed_text",
    methods = c("sbert", "gemini"),
    sbert_model = sbert_model,
    sbert_dims = 768L,
    gemini_model = gemini_model,
    gemini_dims = 768L,
    gemini_batch_size = as.integer(gemini_batch_size),
    gemini_sleep = gemini_sleep,
    gemini_task_type = gemini_task_type,
    response_col = "response_flag",
    verbose = verbose
  )

  sbert_cols <- grep("^SBERT_\\d+$", names(embedded), value = TRUE)
  gemini_cols <- grep("^Gemini_\\d+$", names(embedded), value = TRUE)
  emb_cols <- c(sbert_cols, gemini_cols)

  if (length(sbert_cols) != 768L || length(gemini_cols) != 768L) {
    stop(
      "Expected 768 SBERT and 768 Gemini columns, got ",
      length(sbert_cols), " SBERT and ", length(gemini_cols), " Gemini."
    )
  }

  out <- embedded[, c("term", emb_cols), drop = FALSE]
  out <- out[!duplicated(out$term), , drop = FALSE]
  rownames(out) <- out$term
  out
}

mean_embedding <- function(term_embeddings, terms, emb_cols, row_name) {
  terms <- unique(terms[!is.na(terms) & nzchar(terms)])
  terms <- intersect(terms, rownames(term_embeddings))
  if (length(terms) == 0) {
    warning("Skipping row '", row_name, "' (no usable seed terms).")
    return(NULL)
  }
  colMeans(term_embeddings[terms, emb_cols, drop = FALSE], na.rm = TRUE)
}

build_seed_vectors <- function(term_embeddings, groups) {
  sbert_cols <- grep("^SBERT_\\d+$", names(term_embeddings), value = TRUE)
  gemini_cols <- grep("^Gemini_\\d+$", names(term_embeddings), value = TRUE)
  emb_cols <- c(sbert_cols, gemini_cols)

  rows <- list()

  # Base dictionary rows
  for (nm in names(groups$base_groups)) {
    vec <- mean_embedding(term_embeddings, groups$base_groups[[nm]], emb_cols, nm)
    if (!is.null(vec)) rows[[nm]] <- vec
  }

  # Directional rows: hi, lo, and hi-lo direction
  for (nm in names(groups$dir_groups)) {
    hi_name <- paste0(nm, "_hi")
    lo_name <- paste0(nm, "_lo")
    dir_name <- paste0(nm, "_dir")

    hi_vec <- mean_embedding(term_embeddings, groups$dir_groups[[nm]]$hi_terms, emb_cols, hi_name)
    lo_vec <- mean_embedding(term_embeddings, groups$dir_groups[[nm]]$lo_terms, emb_cols, lo_name)

    if (!is.null(hi_vec)) rows[[hi_name]] <- hi_vec
    if (!is.null(lo_vec)) rows[[lo_name]] <- lo_vec

    if (!is.null(hi_vec) && !is.null(lo_vec)) {
      rows[[dir_name]] <- hi_vec - lo_vec
    } else {
      warning("Skipping direction row '", dir_name, "' (missing hi/lo vector).")
    }
  }

  if (length(rows) == 0) {
    stop("No seed-vector rows were generated.")
  }

  mat <- do.call(rbind, rows)
  out <- data.frame(
    Dictionary = names(rows),
    as.data.frame(mat, stringsAsFactors = FALSE),
    row.names = NULL,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  sbert_cols <- grep("^SBERT_\\d+$", names(out), value = TRUE)
  gemini_cols <- grep("^Gemini_\\d+$", names(out), value = TRUE)
  out <- out[, c("Dictionary", sbert_cols, gemini_cols), drop = FALSE]
  out
}

validate_schema <- function(df, object_name) {
  if (!"Dictionary" %in% names(df)) {
    stop(object_name, " is missing `Dictionary` column.")
  }
  if (any(grepl("^Sentence", names(df))) || "Sum" %in% names(df)) {
    stop(object_name, " contains forbidden columns (`Sentence*` or `Sum`).")
  }
  sbert_cols <- grep("^SBERT_\\d+$", names(df), value = TRUE)
  gemini_cols <- grep("^Gemini_\\d+$", names(df), value = TRUE)
  if (length(sbert_cols) != 768L || length(gemini_cols) != 768L) {
    stop(
      object_name, " must contain 768 SBERT and 768 Gemini columns. Found ",
      length(sbert_cols), " SBERT and ", length(gemini_cols), " Gemini."
    )
  }
}

validate_direction_rows <- function(df, tol = 1e-8) {
  emb_cols <- setdiff(names(df), "Dictionary")
  dict_names <- df$Dictionary
  dir_names <- dict_names[grepl("_dir$", dict_names)]

  for (dir_name in dir_names) {
    stem <- sub("_dir$", "", dir_name)
    hi_name <- paste0(stem, "_hi")
    lo_name <- paste0(stem, "_lo")
    if (!(hi_name %in% dict_names && lo_name %in% dict_names)) next

    hi_vec <- as.numeric(df[df$Dictionary == hi_name, emb_cols, drop = FALSE])
    lo_vec <- as.numeric(df[df$Dictionary == lo_name, emb_cols, drop = FALSE])
    dir_vec <- as.numeric(df[df$Dictionary == dir_name, emb_cols, drop = FALSE])
    max_diff <- max(abs((hi_vec - lo_vec) - dir_vec), na.rm = TRUE)
    if (!is.finite(max_diff) || max_diff > tol) {
      stop("Direction row validation failed for ", dir_name, " (max diff=", max_diff, ").")
    }
  }
}

gemini_batch_size <- suppressWarnings(as.integer(Sys.getenv("SADCAT_SEED_GEMINI_BATCH_SIZE", "100")))
if (is.na(gemini_batch_size) || gemini_batch_size <= 0L) {
  gemini_batch_size <- 100L
}
gemini_sleep <- suppressWarnings(as.numeric(Sys.getenv("SADCAT_SEED_GEMINI_SLEEP", "1")))
if (is.na(gemini_sleep) || gemini_sleep < 0) {
  gemini_sleep <- 1
}

groups <- build_groups(seed_raw, base_cols, dir_cols)

message("Embedding terms using `term` (for Seed_Vectors_Avg and Seed_Vectors_allwords_Avg)...")
term_embeddings <- embed_terms(
  seed_raw,
  text_source = "term",
  gemini_task_type = "SEMANTIC_SIMILARITY",
  gemini_batch_size = gemini_batch_size,
  gemini_sleep = gemini_sleep,
  verbose = TRUE
)

message("Embedding terms using `term_context` (for Seed_Vectors_personcontext_Avg)...")
context_embeddings <- embed_terms(
  seed_raw,
  text_source = "term_context",
  gemini_task_type = "SEMANTIC_SIMILARITY",
  gemini_batch_size = gemini_batch_size,
  gemini_sleep = gemini_sleep,
  verbose = TRUE
)

Seed_Vectors_Avg <- build_seed_vectors(term_embeddings, groups)
Seed_Vectors_allwords_Avg <- build_seed_vectors(term_embeddings, groups)
Seed_Vectors_personcontext_Avg <- build_seed_vectors(context_embeddings, groups)

validate_schema(Seed_Vectors_Avg, "Seed_Vectors_Avg")
validate_schema(Seed_Vectors_allwords_Avg, "Seed_Vectors_allwords_Avg")
validate_schema(Seed_Vectors_personcontext_Avg, "Seed_Vectors_personcontext_Avg")

validate_direction_rows(Seed_Vectors_Avg)
validate_direction_rows(Seed_Vectors_allwords_Avg)
validate_direction_rows(Seed_Vectors_personcontext_Avg)

message("Saving regenerated seed vector datasets to `data/`...")
usethis::use_data(
  Seed_Vectors_Avg,
  Seed_Vectors_allwords_Avg,
  Seed_Vectors_personcontext_Avg,
  overwrite = TRUE,
  compress = "xz"
)

message("Seed vector rebuild complete.")
