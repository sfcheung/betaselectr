skip_on_cran()

library(testthat)

dat <- data_test_mod_cat

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "iv", do_boot = FALSE)
lm_beta_x_0 <- lm(dv ~ iv*mod + cov1 + cat1, dat)
lm_beta_x_1 <- lm_betaselect(lm_beta_x_0, to_standardize = "iv", do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "iv", do_boot = FALSE)
lm_beta_x_0 <- lm(dv ~ iv*mod + cov1 + cat1, dat)
lm_beta_x_1 <- lm_betaselect(lm_beta_x_0, to_standardize = "iv", do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "dv", do_boot = FALSE)
lm_beta_x_0 <- lm(dv ~ iv*mod + cov1 + cat1, dat)
lm_beta_x_1 <- lm_betaselect(lm_beta_x_0, to_standardize = "dv", do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "mod", do_boot = FALSE)
lm_beta_x_0 <- lm(dv ~ iv*mod + cov1 + cat1, dat)
lm_beta_x_1 <- lm_betaselect(lm_beta_x_0, to_standardize = "mod", do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = c("mod", "iv"), do_boot = FALSE)
lm_beta_x_0 <- lm(dv ~ iv*mod + cov1 + cat1, dat)
lm_beta_x_1 <- lm_betaselect(lm_beta_x_0, to_standardize = c("mod", "iv"), do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = c("mod", "dv"), do_boot = FALSE)
lm_beta_x_0 <- lm(dv ~ iv*mod + cov1 + cat1, dat)
lm_beta_x_1 <- lm_betaselect(lm_beta_x_0, to_standardize = c("mod", "dv"), do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = c("iv", "dv"), do_boot = FALSE)
lm_beta_x_0 <- lm(dv ~ iv*mod + cov1 + cat1, dat)
lm_beta_x_1 <- lm_betaselect(lm_beta_x_0, to_standardize = c("iv", "dv"), do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, do_boot = FALSE)
lm_beta_x_0 <- lm(dv ~ iv*mod + cov1 + cat1, dat)
lm_beta_x_1 <- lm_betaselect(lm_beta_x_0, do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

lm_beta_x <- lm_betaselect(dv ~ I(iv^2)*mod + I(1/ cov1) + cat1, dat, do_boot = FALSE)
lm_beta_x_0 <- lm(dv ~ I(iv^2)*mod + I(1/ cov1) + cat1, dat)
lm_beta_x_1 <- lm_betaselect(lm_beta_x_0, do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)
