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
  cohens_d_sigma <- sqrt(sum(as.data.frame(lme4::VarCorr(mod))$vcov))
  Cohens <- abs(as.data.frame(pairwises)$Estimate / cohens_d_sigma)
  info <- dplyr::select(as.data.frame(pairwises), -c(Estimate:p.value))
  cbind(info, Cohens)
}
