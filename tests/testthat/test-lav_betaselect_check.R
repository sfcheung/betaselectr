# testthat::test_file("./tests/testthat/test_std_selected_lavaan_check.R")

library(testthat)
library(lavaan)

# Multilevel model

mod_mlm <-
"
level: 1
f1 =~ y1 + y2 + y3
level: 2
f2 =~ y1 + y2 + y3
"

fit_mlm <- sem(mod_mlm,
               Demo.twolevel,
               cluster = "cluster")

mod <-
"
f1 =~ x1 + x2 + x3
"

dat_cov <- cov(HolzingerSwineford1939[, c("x1", "x2", "x3")])
fit_nodata <- cfa(mod,
                  sample.cov = dat_cov,
                  sample.nobs = 301)

dat <- HolzingerSwineford1939
dat$x1d <- cut(dat$x1, breaks = 5)
dat$x2d <- cut(dat$x2, breaks = 5)
dat$x3d <- cut(dat$x3, breaks = 5)
mod_condx <-
"
f1 =~ x1d + x2d + x3d
f1 ~ ageyr
"
fit_condx <- sem(mod_condx,
                 data = dat,
                 ordered = c("x1d", "x2d", "x3d"))

test_that("Check models", {
  expect_error(lav_betaselect(HolzingerSwineford1939))
  expect_error(lav_betaselect(fit_mlm))
  expect_error(lav_betaselect(fit_nodata))
  expect_error(lav_betaselect(fit_condx))
})
