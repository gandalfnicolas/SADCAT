test_that("Cohens_d_EMmeans handles current emmeans column names", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("emmeans")

  dat <- transform(lme4::sleepstudy, day_group = factor(ifelse(Days < 5, "early", "late")))
  mod <- lme4::lmer(Reaction ~ day_group + (1 | Subject), data = dat)
  emm <- emmeans::emmeans(mod, ~ day_group)
  pairwises <- summary(pairs(emm))

  out <- Cohens_d_EMmeans(mod, pairwises)

  expect_s3_class(out, "data.frame")
  expect_true("contrast" %in% names(out))
  expect_true("Cohens" %in% names(out))
  expect_equal(nrow(out), 1)
  expect_gt(out$Cohens[[1]], 0)
})

test_that("cldtable formats summary output with confidence intervals and p-values", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("multcomp")

  dat <- transform(lme4::sleepstudy, day_group = factor(ifelse(Days < 5, "early", "late")))
  mod <- lme4::lmer(Reaction ~ day_group + (1 | Subject), data = dat)
  emm <- emmeans::emmeans(mod, ~ day_group)

  out <- suppressMessages(cldtable(mod, emm))

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_true("95% CI" %in% names(out))
  expect_true("p" %in% names(out))
  expect_true("Reaction" %in% names(out))
  expect_true(all(grepl("^\\[", out[["95% CI"]])))
  expect_true(all(nzchar(out[["Reaction"]])))
})
