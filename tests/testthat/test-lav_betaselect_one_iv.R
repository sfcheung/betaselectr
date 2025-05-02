# testthat::test_file("./tests/testthat/test_std_selected_lavaan.R")

# One IV

library(testthat)
library(lavaan)
library(manymome)

dat <- HolzingerSwineford1939
mod <-
"
x1 ~ x2
"

fit <- sem(mod,
           dat,
           fixed.x = FALSE)

est <- parameterEstimates(fit,
                          standardized = TRUE,
                          ci = FALSE)
std <- standardizedSolution(fit)
std_nox <- standardizedSolution(fit, type = "std.nox")
std_lv <- standardizedSolution(fit, type = "std.lv")

test_that("Standardized coefficients and SEs", {
  i <- c(1)
  fit_est <- coef(fit)
  fit_vcov <- vcov(fit)
  fit_std <- lapply(i, function(x) gen_std_i(fit = fit, i = x))
  std_se <- sapply(fit_std,
                   FUN = std_se_delta,
                   fit_est = fit_est,
                   fit_vcov = fit_vcov)
  std_est <- sapply(fit_std,
                    function(x) x(fit_est))
  expect_equal(std_est,
               std[i, "est.std"],
               ignore_attr = TRUE)
  expect_equal(std_se,
               std[i, "se"],
               ignore_attr = TRUE)
})

fit2 <- sem(mod,
            dat[c(1:100, 150:250), ],
            fixed.x = FALSE)
std2 <- standardizedSolution(fit2)
std2_nox <- standardizedSolution(fit2, type = "std.nox")

test_that("Alternate values", {
  fit_std_2 <- gen_std_i(fit = fit, i = 1)
  expect_equal(as.vector(fit_std_2(coef(fit2))),
               std2[1, "est.std"],
               ignore_attr = TRUE)
})

# Delta method

test_that("All est", {
  out <- lav_betaselect(fit, standardized = TRUE, skip_categorical_x = FALSE)
  expect_equal(out$std.all,
               out$std.p,
               ignore_attr = TRUE)
  expect_output(print(out, standardized_only = FALSE),
                "Estimates")
  expect_output(print(out, standardized_only = TRUE),
                "BetaSelect")
})

# Check skipping the search for product terms

test_that("All est", {
  out1 <- lav_betaselect(fit, progress = FALSE)
  out2 <- lav_betaselect(fit, find_product_terms = FALSE, progress = FALSE)
  expect_equal(out1,
               out2,
               ignore_attr = TRUE)
})


# (which(std_nox$est.std != std$est.std))
#  4  5  6 11 12 13 14 15 16

test_that("Standardized coefficients: No X", {
  i <- c(1)
  fit_est <- coef(fit)
  fit_vcov <- vcov(fit)
  fit_std <- lapply(i, function(x) gen_std_i(fit = fit, i = x, to_standardize = "x1"))
  std_se <- sapply(fit_std,
                   FUN = std_se_delta,
                   fit_est = fit_est,
                   fit_vcov = fit_vcov)
  std_est <- sapply(fit_std,
                    function(x) x(fit_est))
  expect_equal(std_est,
               std_nox[i, "est.std"],
               ignore_attr = TRUE)
  expect_equal(std_se,
               std_nox[i, "se"],
               ignore_attr = TRUE)
})
