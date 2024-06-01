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

suppressWarnings(fit <- sem(mod,
                            dat,
                            se = "boot",
                            bootstrap = 100,
                            iseed = 1234))

est <- parameterEstimates(fit,
                          standardized = TRUE,
                          ci = FALSE)
std <- standardizedSolution(fit)
std_nox <- standardizedSolution(fit, type = "std.nox")
std_lv <- standardizedSolution(fit, type = "std.lv")


mod_chk <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + d5*x5 + d6*x6
f3 =~ x7 + d8*x8 + d9*x9
f2 ~ a*f1
f3 ~ b*f2
f1 ~~ v1*f1
abstd := a*b*sqrt(v1)
"

suppressWarnings(fit_chk <- sem(mod_chk,
                                dat,
                                se = "boot",
                                bootstrap = 100,
                                iseed = 1234))

est_chk <- parameterEstimates(fit_chk,
                              standardized = TRUE,
                              ci = TRUE)

test_that("User parameters", {
  system.time(out <- lav_betaselect(fit,
                                    standardized = TRUE,
                                    to_standardize = "f1",
                                    std_se = "boot",
                                    ci = TRUE,
                                    progress = FALSE))
  out[24, ]
  est_chk[24, ]
  expect_equal(out$std.p[24],
               est_chk$est[24],
               ignore_attr = TRUE)
  expect_equal(out$std.p.se[24],
               est_chk$se[24],
               ignore_attr = TRUE)
  expect_equal(out$std.p.ci.lower[24],
               est_chk$ci.lower[24],
               ignore_attr = TRUE,
               tolerance = 1e-4)
  expect_equal(out$std.p.ci.upper[24],
               est_chk$ci.upper[24],
               ignore_attr = TRUE,
               tolerance = 1e-4)
})
