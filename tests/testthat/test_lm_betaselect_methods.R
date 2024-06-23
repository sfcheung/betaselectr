# Adapted from stdmod

library(testthat)

dat_tmp <- data_test_mod_cat
dat_tmp$iv <- scale(data_test_mod_cat$iv, scale = FALSE, center = TRUE)[, 1]
dat_tmp$mod <- scale(data_test_mod_cat$mod, scale = sd(data_test_mod_cat$mod), center = FALSE)[, 1]

lm_raw <- lm(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat)
lm_inline_raw <- lm(dv ~ I(iv^2)*mod + I(1/ cov1) + cat1,
                    data_test_mod_cat)


lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = FALSE)
lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = FALSE)
lm_beta_y <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "dv", do_boot = FALSE)
lm_beta_w <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "mod", do_boot = FALSE)
lm_beta_xw <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = c("mod", "iv"), do_boot = FALSE)
lm_beta_yw <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = c("mod", "dv"), do_boot = FALSE)
lm_beta_xy <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = c("iv", "dv"), do_boot = FALSE)
lm_beta_xyw <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, do_boot = FALSE)
lm_beta_inline <- lm_betaselect(dv ~ I(iv^2)*mod + I(1/ cov1) + cat1, data_test_mod_cat, do_boot = FALSE)

test_that("coef", {
    expect_identical(lm_beta_x$coefficients,
                     coef(lm_beta_x))
    expect_identical(lm_raw$coefficients,
                     coef(lm_beta_x, type = "raw"))
    expect_identical(lm_beta_xyw$coefficients,
                     coef(lm_beta_xyw))
    expect_identical(lm_raw$coefficients,
                     coef(lm_beta_xyw, type = "raw"))
    expect_identical(lm_beta_inline$coefficients,
                     coef(lm_beta_inline))
    expect_identical(lm_inline_raw$coefficients,
                     coef(lm_beta_inline, type = "raw"))
  })
