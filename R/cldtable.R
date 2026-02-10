#' @title Compact Letter Display Table
#'
#' @description Create compact letter display tables from model and emmeans
#' @param modelx A fitted model object
#' @param emmeans An emmeans object
#' @param round_digits Number of digits to round to (default 3)
#' @param stat_adjust P-value adjustment method (default "Tukey")
#' @return Data frame with CLD annotations
#' @export cldtable

cldtable <- function(modelx, emmeans, round_digits = 3, stat_adjust = "Tukey") {
  if (!requireNamespace("multcomp", quietly = TRUE)) {
    stop("Package 'multcomp' is required for cldtable(). Install with: install.packages('multcomp')")
  }
  lmveremdat <- as.data.frame(emmeans)
  lmveremdat <- lmveremdat[order(-lmveremdat[[2]]), ]
  if ("null" %in% colnames(lmveremdat)) {
    lmveremdat <- lmveremdat[, !colnames(lmveremdat) %in% "null"]
  }
  lmveremdat <- lmveremdat %>% dplyr::mutate_if(is.numeric, round, digits = round_digits)
  lmveremdat$p.value <- ifelse(
    lmveremdat$p.value %in% c("0.000", "0.00", "0.0", "0"),
    "<.001", lmveremdat$p.value
  )
  lmveremdat <- as.data.frame(lmveremdat)
  concatenated_values <- paste0("[", paste(lmveremdat[, 5], lmveremdat[, 6], sep = ", "), "]")
  lmveremdat <- as.data.frame(lmveremdat)[, -c(5, 6)]
  lmveremdat <- data.frame(lmveremdat[, 1:2], concatenated_values, lmveremdat[, 3:(ncol(lmveremdat))])
  names(lmveremdat) <- c(names(lmveremdat)[1], sub("\\~.*", "", stats::formula(modelx))[[2]],
                         "95% CI", "SE", "df", names(lmveremdat)[6], "p")
  model_means_cld <- multcomp::cld(object = emmeans, adjust = stat_adjust,
                                   Letters = letters, alpha = 0.05, sort = TRUE, reversed = TRUE)
  lmveremdat[, 2] <- paste(lmveremdat[, 2], trimws(model_means_cld$.group), sep = "")
  return(lmveremdat)
}
