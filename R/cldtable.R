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
  lmveremdat <- as.data.frame(summary(emmeans, infer = c(TRUE, TRUE)))
  lmveremdat <- lmveremdat[order(-lmveremdat[[2]]), ]
  if ("null" %in% colnames(lmveremdat)) {
    lmveremdat <- lmveremdat[, !colnames(lmveremdat) %in% "null"]
  }
  lmveremdat <- dplyr::mutate_if(lmveremdat, is.numeric, round, digits = round_digits)

  if ("p.value" %in% names(lmveremdat)) {
    lmveremdat$p.value <- ifelse(
      lmveremdat$p.value < 0.001,
      "<.001",
      format(round(lmveremdat$p.value, round_digits), nsmall = round_digits, trim = TRUE)
    )
  }

  ci_cols <- intersect(c("lower.CL", "upper.CL"), names(lmveremdat))
  if (length(ci_cols) != 2) {
    stop("emmeans summary must include lower.CL and upper.CL columns.")
  }

  concatenated_values <- paste0(
    "[",
    paste(lmveremdat[[ci_cols[1]]], lmveremdat[[ci_cols[2]]], sep = ", "),
    "]"
  )
  lmveremdat <- lmveremdat[, setdiff(names(lmveremdat), ci_cols), drop = FALSE]

  group_col <- names(lmveremdat)[1]
  estimate_col <- names(lmveremdat)[2]
  outcome_name <- all.vars(stats::formula(modelx))[1]

  leading_cols <- c(group_col, estimate_col)
  remaining_cols <- setdiff(names(lmveremdat), leading_cols)
  lmveremdat <- data.frame(
    lmveremdat[, leading_cols, drop = FALSE],
    check.names = FALSE,
    "95% CI" = concatenated_values,
    lmveremdat[, remaining_cols, drop = FALSE]
  )
  names(lmveremdat)[names(lmveremdat) == estimate_col] <- outcome_name
  if ("p.value" %in% names(lmveremdat)) {
    names(lmveremdat)[names(lmveremdat) == "p.value"] <- "p"
  }

  model_means_cld <- multcomp::cld(object = emmeans, adjust = stat_adjust,
                                   Letters = letters, alpha = 0.05, sort = TRUE, reversed = TRUE)
  letter_map <- stats::setNames(trimws(model_means_cld$.group), as.character(model_means_cld[[group_col]]))
  lmveremdat[[outcome_name]] <- paste0(
    lmveremdat[[outcome_name]],
    unname(letter_map[as.character(lmveremdat[[group_col]])])
  )
  return(lmveremdat)
}
