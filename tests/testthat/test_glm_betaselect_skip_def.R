library(testthat)

dat <- data_test_mod_cat

dat$dv <- ifelse(dat$dv > mean(dat$dv),
                 yes = 1,
                 no = 0)
test_that("skip response", {
  expect_error(glm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "dv", do_boot = FALSE, family = binomial),
               "y values", fixed = TRUE)
  expect_error(glm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, do_boot = FALSE, family = binomial),
               "y values", fixed = TRUE)
  expect_no_error(glm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, skip_response = TRUE, do_boot = FALSE, family = binomial))
})
