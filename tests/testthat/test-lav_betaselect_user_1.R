skip_on_cran()
# Long test

# testthat::test_file("./tests/testthat/test_std_selected_lavaan_user_1.R")

library(testthat)
library(lavaan)
# library(manymome)

dat <- HolzingerSwineford1939

mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + d5*x5 + d6*x6
f3 =~ x7 + d8*x8 + d9*x9
f2 ~ a*f1
f3 ~ b*f2
ab := a*b
d1 := d5 - d9
d2 := d8 - d6
dd := d1 * d2
"

fit <- cfa(mod,
           dat)

est <- parameterEstimates(fit,
                          standardized = TRUE,
                          ci = FALSE)
std <- standardizedSolution(fit)
std_nox <- standardizedSolution(fit, type = "std.nox")
std_lv <- standardizedSolution(fit, type = "std.lv")

test_that("User parameters", {
  system.time(out <- lav_betaselect(fit,
                                    standardized = TRUE,
                                    std_se = "delta",
                                    ci = TRUE,
                                    progress = FALSE))
  expect_equal(out$std.all,
               out$std.p,
               ignore_attr = TRUE)
  # std[1:5, c("lhs", "op", "rhs", "se", "ci.lower", "ci.upper")]
  # out[1:5, c("lhs", "op", "rhs", "std.p.se", "std.p.ci.lower", "std.p.ci.upper")]
  # std[24:27, c("lhs", "op", "rhs", "se", "ci.lower", "ci.upper")]
  # out[24:27, c("lhs", "op", "rhs", "std.p.se", "std.p.ci.lower", "std.p.ci.upper")]

  expect_equal(out$std.p.se,
               std$se,
               ignore_attr = TRUE,
               tolerance = 1e-4)
  expect_equal(out$std.p.ci.lower,
               std$ci.lower,
               ignore_attr = TRUE,
               tolerance = 1e-4)
  expect_equal(out$std.p.ci.upper,
               std$ci.upper,
               ignore_attr = TRUE,
               tolerance = 1e-4)
  expect_output(print(out, output = "text"),
                "Estimates")
  expect_output(print(out, output = "text", standardized_only = TRUE),
                "Estimates")
})
