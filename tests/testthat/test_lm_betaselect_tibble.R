skip_if_not_installed("tibble")
library(testthat)

test_that("tibble", {

dat <- data_test_mod_cat
dat2 <- tibble::as_tibble(dat)

lm_beta1 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "iv", do_boot = FALSE)
lm_beta2 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat2, to_standardize = "iv", do_boot = FALSE)
expect_equal(
  coef(lm_beta1),
  coef(lm_beta2)
)

lm_beta1 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, not_to_standardize = "iv", do_boot = FALSE)
lm_beta2 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat2, not_to_standardize = "iv", do_boot = FALSE)
expect_equal(
  coef(lm_beta1),
  coef(lm_beta2)
)

lm_beta1 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "dv", do_boot = FALSE)
lm_beta2 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat2, to_standardize = "dv", do_boot = FALSE)
expect_equal(
  coef(lm_beta1),
  coef(lm_beta2)
)

lm_beta1 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = ".all.", do_boot = FALSE)
lm_beta2 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat2, to_standardize = ".all.", do_boot = FALSE)
expect_equal(
  coef(lm_beta1),
  coef(lm_beta2)
)

lm_beta1 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = c("mod", "iv"), do_boot = FALSE)
lm_beta2 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat2, to_standardize = c("mod", "iv"), do_boot = FALSE)
expect_equal(
  coef(lm_beta1),
  coef(lm_beta2)
)

lm_beta1 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = c("mod", "dv"), do_boot = FALSE)
lm_beta2 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat2, to_standardize = c("mod", "dv"), do_boot = FALSE)
expect_equal(
  coef(lm_beta1),
  coef(lm_beta2)
)

lm_beta1 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = c("iv", "dv"), do_boot = FALSE)
lm_beta2 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat2, to_standardize = c("iv", "dv"), do_boot = FALSE)
expect_equal(
  coef(lm_beta1),
  coef(lm_beta2)
)

lm_beta1 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, do_boot = FALSE)
lm_beta2 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat2, do_boot = FALSE)
expect_equal(
  coef(lm_beta1),
  coef(lm_beta2)
)

lm_beta1 <- lm_betaselect(dv ~ I(iv^2)*mod + I(1/ cov1) + cat1, dat, do_boot = FALSE)
lm_beta2 <- lm_betaselect(dv ~ I(iv^2)*mod + I(1/ cov1) + cat1, dat2, do_boot = FALSE)
expect_equal(
  coef(lm_beta1),
  coef(lm_beta2)
)

})
