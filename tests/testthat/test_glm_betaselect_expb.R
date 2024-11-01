library(testthat)


test_that("transform b", {
  lm_beta_x <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat_binary, to_standardize = "iv", do_boot = FALSE, family = binomial)
  lm_beta_x_boot <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat_binary, to_standardize = "iv", do_boot = TRUE, bootstrap = 6, parallel = FALSE, iseed = 5678, progress = FALSE, family = binomial)

  tmp1 <- summary(lm_beta_x)
  tmp2 <- summary(lm_beta_x, transform_b = exp, transform_b_name = "Exp(B)")
  expect_identical(tmp1$coefficients_transformed,
                   tmp2$coefficients_transformed)
  expect_equal(exp(tmp1$coefficients[, 1:3]),
               tmp2$coefficients_transformed,
               ignore_attr = TRUE)

  tmp1 <- suppressWarnings(summary(lm_beta_x_boot))
  tmp2 <- suppressWarnings(summary(lm_beta_x_boot, transform_b = exp, transform_b_name = "Exp(B)"))
  expect_identical(tmp1$coefficients_transformed,
                   tmp2$coefficients_transformed)

  expect_equal(exp(tmp1$coefficients[, 1:3]),
               tmp2$coefficients_transformed,
               ignore_attr = TRUE)

})

