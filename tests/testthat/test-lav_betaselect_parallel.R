skip_on_cran()

library(testthat)
library(lavaan)
library(manymome)

dat <- HolzingerSwineford1939

mod <-
"
x1 ~ x2
x3 ~ x1 + x2
"

suppressWarnings(fit <- sem(mod,
                            dat,
                            se = "boot",
                            bootstrap = 100,
                            parallel = "snow",
                            ncpus = 2,
                            iseed = 1234))

suppressWarnings(fit2 <- sem(mod,
                            dat,
                            iseed = 1234))

suppressWarnings(fit3 <- sem(mod,
                            dat,
                            se = "boot",
                            bootstrap = 100,
                            iseed = 1234))

test_that("parallel in boot std", {
  system.time(out1 <- lav_betaselect(fit,
                                    standardized = TRUE,
                                    std_se = "boot",
                                    ci = TRUE,
                                    parallel = "snow",
                                    ncpus = 2,
                                    progress = !is_testing()))
  system.time(out2 <- lav_betaselect(fit,
                                    standardized = TRUE,
                                    std_se = "boot",
                                    ci = TRUE,
                                    progress = !is_testing()))
  expect_equal(out1$std.p.ci.lower,
               out2$std.p.ci.lower)

  system.time(out1b <- lav_betaselect(fit2,
                                    standardized = TRUE,
                                    std_se = "boot",
                                    ci = TRUE,
                                    parallel = "snow",
                                    ncpus = 2,
                                    bootstrap = 100,
                                    iseed = 1234,
                                    progress = !is_testing()))
  expect_equal(out1$std.p.ci.lower,
               out1b$std.p.ci.lower)

})
