#' @title Cohen's d from Emmeans
#'
#' @description Compute Cohen's d from emmeans pairwise comparisons
#' @param mod A fitted mixed model object
#' @param pairwises Pairwise comparisons from emmeans
#' @return Data frame with contrast info and Cohen's d values
#' @export Cohens_d_EMmeans

Cohens_d_EMmeans <- function(mod, pairwises) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for Cohens_d_EMmeans(). Install with: install.packages('lme4')")
  }
  pairwise_df <- as.data.frame(pairwises)
  estimate_col <- intersect(c("Estimate", "estimate"), names(pairwise_df))
  if (!length(estimate_col)) {
    stop("pairwises must include an estimate column.")
  }
  estimate_col <- estimate_col[[1]]

  cohens_d_sigma <- sqrt(sum(as.data.frame(lme4::VarCorr(mod))$vcov))
  Cohens <- abs(pairwise_df[[estimate_col]] / cohens_d_sigma)

  stat_cols <- intersect(
    c("Estimate", "estimate", "SE", "df", "t.ratio", "z.ratio", "p.value"),
    names(pairwise_df)
  )
  info <- pairwise_df[setdiff(names(pairwise_df), stat_cols)]
  cbind(info, Cohens = Cohens)
}
