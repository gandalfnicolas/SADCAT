# Build Warriner_norms.rda and Jockers_norms.rda for SADCAT.
#
# Sources
# -------
# Warriner et al. (2013). Norms of valence, arousal, and dominance for 13,915
#   English lemmas. Behavior Research Methods, 45, 1191-1207.
#   doi:10.3758/s13428-012-0314-x
#   Supplementary CSV: BRM-emot-submit.csv (free, redistributable per Springer
#   supplementary terms). The CSV is committed to data-raw/ for reproducibility.
#
# Jockers/Rinker. lexicon::hash_sentiment_jockers_rinker (MIT-licensed via the
#   lexicon R package).
#
# Output schema for both
# ----------------------
#   word    : character (lowercased, deduplicated)
#   valence : numeric in [-1, 1]
#
# Rescaling
# ---------
# Warriner: V.Mean.Sum is on a 1-9 Likert (5 = neutral). Rescale via
#   (V.Mean.Sum - 5) / 4 to map exactly 1 -> -1, 5 -> 0, 9 -> 1.
#
# Jockers: lexicon$y is mostly in [-1, 1] but 13 of 11,710 entries fall at
#   -2.00 or -1.05. These are not sentiment values but shifter / amplifier /
#   counterfactual coefficients (e.g., "overly" -2, "could have" -1.05,
#   "too much" -2, "unduly" -2) used by sentimentr's sliding-window aggregator
#   to OVERRIDE neighboring sentiment. They are not independently
#   sentiment-bearing words. We drop them here so they don't artificially
#   pull per-text means toward the negative extreme. (lexicon ships a
#   separate hash_valence_shifters table for words of this kind.)

suppressMessages({
  library(data.table)
})

# ---- Warriner ----
warr_csv <- "data-raw/BRM-emot-submit.csv"
stopifnot(file.exists(warr_csv))
warr <- data.table::fread(warr_csv, select = c("Word", "V.Mean.Sum"),
                          showProgress = FALSE)
setnames(warr, c("Word", "V.Mean.Sum"), c("word", "v_mean_sum"))
warr[, word := tolower(trimws(word))]
warr <- warr[!is.na(word) & nzchar(word) & !is.na(v_mean_sum)]
warr <- warr[!duplicated(word)]
warr[, valence := (v_mean_sum - 5) / 4]
stopifnot(all(warr$valence >= -1 - 1e-12 & warr$valence <= 1 + 1e-12))
warr$v_mean_sum <- NULL
Warriner_norms <- as.data.frame(warr, stringsAsFactors = FALSE)

cat(sprintf("Warriner_norms: %d rows; valence range [%.3f, %.3f]; mean %.3f\n",
            nrow(Warriner_norms), min(Warriner_norms$valence),
            max(Warriner_norms$valence), mean(Warriner_norms$valence)))

save(Warriner_norms, file = "data/Warriner_norms.rda",
     compress = "xz", compression_level = 9)

# ---- Jockers ----
if (!requireNamespace("lexicon", quietly = TRUE)) {
  stop("Package 'lexicon' is required to build Jockers_norms. ",
       "Install with install.packages('lexicon').")
}
jock <- as.data.frame(lexicon::hash_sentiment_jockers_rinker,
                       stringsAsFactors = FALSE)
names(jock) <- c("word", "valence")
jock$word <- tolower(trimws(jock$word))
jock <- jock[!is.na(jock$word) & nzchar(jock$word) & !is.na(jock$valence), ]
jock <- jock[!duplicated(jock$word), ]

# Drop entries outside [-1, 1]: these are shifter / amplifier coefficients
# (e.g., "overly", "unduly", "i wish", "could have", "too much"), not
# independent sentiment words. Including them at extreme values would
# overstate negativity in our presence-based mean.
out_of_range <- jock$valence < -1 | jock$valence > 1
n_dropped <- sum(out_of_range)
dropped_words <- jock$word[out_of_range]
jock <- jock[!out_of_range, ]
stopifnot(all(jock$valence >= -1 - 1e-12 & jock$valence <= 1 + 1e-12))
Jockers_norms <- jock

cat(sprintf("Jockers_norms: %d rows; dropped %d shifter entries; mean %.3f\n",
            nrow(Jockers_norms), n_dropped, mean(Jockers_norms$valence)))
cat("Dropped entries:", paste(dropped_words, collapse = ", "), "\n")

save(Jockers_norms, file = "data/Jockers_norms.rda",
     compress = "xz", compression_level = 9)

cat("Done.\n")
