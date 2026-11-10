test_that("many_lm", {

skip_if_not_installed("lmhelprs")

library(lmhelprs)

mod <-
"
med ~ iv + cov1
dv ~ med + iv*mod + cov2
"

fit <- many_lm(
  mod,
  data_test_medmod
)
fit_med <- lm(
  med ~ iv + cov1,
  data_test_medmod
)
fit_dv <- lm(
  dv ~  med + iv*mod + cov2,
  data_test_medmod
)

lm_beta_med <- lm_betaselect(
  fit[[1]],
  to_standardize = c("cov1"),
  do_boot = FALSE
)
lm_beta_dv <- lm_betaselect(
  fit[[2]],
  not_to_standardize = c("dv"),
  do_boot = FALSE
)
lm_beta_med2 <- lm_betaselect(
  fit_med,
  to_standardize = c("cov1"),
  do_boot = FALSE
)
lm_beta_dv2 <- lm_betaselect(
  fit_dv,
  not_to_standardize = c("dv"),
  do_boot = FALSE
)

expect_equal(
  coef(lm_beta_med),
  coef(lm_beta_med2)
)

expect_equal(
  coef(lm_beta_dv),
  coef(lm_beta_dv2)
)


lm_beta_med <- lm_betaselect(
  fit[[1]],
  to_standardize = c("cov1"),
  iseed = 1,
  parallel = FALSE,
  progress = FALSE,
  bootstrap = 10
)
lm_beta_dv <- lm_betaselect(
  fit[[2]],
  not_to_standardize = c("dv"),
  iseed = 1,
  parallel = FALSE,
  progress = FALSE,
  bootstrap = 10
)
lm_beta_med2 <- lm_betaselect(
  fit_med,
  to_standardize = c("cov1"),
  iseed = 1,
  parallel = FALSE,
  progress = FALSE,
  bootstrap = 10
)
lm_beta_dv2 <- lm_betaselect(
  fit_dv,
  not_to_standardize = c("dv"),
  iseed = 1,
  parallel = FALSE,
  progress = FALSE,
  bootstrap = 10
)

expect_equal(
  suppressWarnings(confint(lm_beta_med)),
  suppressWarnings(confint(lm_beta_med2))
)

expect_equal(
  suppressWarnings(confint(lm_beta_dv)),
  suppressWarnings(confint(lm_beta_dv2))
)

out1 <- lm_betaselect(
  fit,
  progress = FALSE
)
summary(out1)



})
