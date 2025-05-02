# Adapted from stdmod

library(testthat)

dat <- data_test_mod_cat

transform0 <- function(data, vars) {
    for (x in vars) {
        data[x] <- scale(data[[x]])[, 1]
      }
    data
  }

lm_raw <- lm(dv ~ iv, dat)
lm_zx  <- lm(dv ~ iv, transform0(dat, c("iv")))
lm_zy  <- lm(dv ~ iv, transform0(dat, c("dv")))
lm_zxzy  <- lm(dv ~ iv, transform0(dat, c("iv", "dv")))
lm_zall  <- lm(dv ~ iv, transform0(dat, c("iv", "dv")))

dat_tmp <- dat
dat_tmp$iv <- scale(dat$iv, scale = FALSE, center = TRUE)[, 1]

lm_beta_x <- lm_betaselect(dv ~ iv, dat, to_standardize = "iv", do_boot = FALSE)
lm_beta_y <- lm_betaselect(dv ~ iv, dat, to_standardize = "dv", do_boot = FALSE)
lm_beta_xy <- lm_betaselect(dv ~ iv, dat, to_standardize = c("iv", "dv"), do_boot = FALSE)
lm_beta_all <- lm_betaselect(dv ~ iv, dat, do_boot = FALSE)

test_that("Standardize x", {
    expect_equal(
        coef(lm_beta_x), coef(lm_zx),
        ignore_attr = TRUE
      )
  })

test_that("Standardize y", {
    expect_equal(
        coef(lm_beta_y), coef(lm_zy),
        ignore_attr = TRUE
      )
  })

test_that("Standardize xy", {
    expect_equal(
        coef(lm_beta_xy), coef(lm_zxzy),
        ignore_attr = TRUE
      )
  })

test_that("Standardize all", {
    expect_equal(
        coef(lm_beta_all), coef(lm_zall),
        ignore_attr = TRUE
      )
  })

test_that("print.lm_betaselect", {
    expect_output(print(lm_beta_x),
                  "Variable(s) standardized: iv", fixed = TRUE)
    expect_output(print(lm_beta_x),
                  "betaselectr::std_data", fixed = TRUE)
    expect_output(print(lm_beta_x, type = "raw"),
                  "data = dat", fixed = TRUE)
  })

test_that("raw_output", {
    expect_identical(coef(raw_output(lm_beta_all)),
                     coef(lm_raw))
  })
