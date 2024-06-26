# testthat::test_file("./tests/testthat/test_std_selected_lavaan_user_2.R")

library(testthat)
library(lavaan)
library(manymome)

dat <- HolzingerSwineford1939

mod <-
"
x2 ~ a*x1
x3 ~ b*x2
ab := a*b
"

fit <- sem(mod,
           dat)

est <- parameterEstimates(fit,
                          standardized = TRUE,
                          ci = FALSE)
std <- standardizedSolution(fit)
std_nox <- standardizedSolution(fit, type = "std.nox")

test_that("User parameters", {
  system.time(out_noy <- lav_betaselect(fit,
                                        standardized = TRUE,
                                        not_to_standardize = "x3",
                                        std_se = "delta",
                                        ci = TRUE,
                                        progress = FALSE))
  ind <- indirect_effect(x = "x1",
                         y = "x3",
                         m = "x2",
                         fit = fit,
                         standardized_x = TRUE)
  expect_equal(out_noy$std.p[6],
               coef(ind),
               ignore_attr = TRUE)
})
