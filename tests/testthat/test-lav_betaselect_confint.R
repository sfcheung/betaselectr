skip_on_cran()

library(testthat)
library(lavaan)
library(manymome)

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

test_that("confint", {
  system.time(out <- lav_betaselect(fit,
                                    standardized = TRUE,
                                    to_standardize = "f1",
                                    std_se = "boot",
                                    ci = TRUE,
                                    progress = FALSE))
  tmp <- confint(out)
  expect_equal(out$std.p.ci.lower,
               tmp[, 1],
               ignore_attr = TRUE)
  expect_equal(out$std.p.ci.upper,
               tmp[, 2],
               ignore_attr = TRUE)
})
