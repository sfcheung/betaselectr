library(testthat)

test_that("extra variables", {

dat <- data_test_mod_cat

lm_beta <- lm_betaselect(dv ~ iv*mod + cat1, dat, do_boot = FALSE)
expect_length(
  intersect(
    "cov1",
    lm_beta$lm_betaselect$to_standardize
  ),
  0
)

})
