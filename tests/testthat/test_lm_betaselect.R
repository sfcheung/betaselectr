skip("WIP")
# Adapted from stdmod

library(testthat)

dat <- data_test_mod_cat

transform0 <- function(data, vars) {
    for (x in vars) {
        data[x] <- scale(data[[x]])[, 1]
      }
    data
  }

lm_raw <- lm(dv ~ iv*mod + cov1 + cat1, dat)
lm_zx  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(dat, c("iv")))
lm_zw  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(dat, c("mod")))
lm_zy  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(dat, c("dv")))
lm_zxzw  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(dat, c("iv", "mod")))
lm_zxzy  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(dat, c("iv", "dv")))
lm_zyzw  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(dat, c("dv", "mod")))
lm_zall  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(dat, c("iv", "dv", "mod", "cov1")))
lm_inline  <- lm(dv ~ iv*mod + I(sqrt(cov1)) + cat1, transform0(dat, c("iv", "dv", "mod")))

dat_tmp <- dat
dat_tmp$iv <- scale(dat$iv, scale = FALSE, center = TRUE)[, 1]
dat_tmp$mod <- scale(dat$mod, scale = sd(dat$mod), center = FALSE)[, 1]

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "iv")

lm_beta_x <- lm_betaselect(lm_raw, to_standardize = "iv")
lm_beta_y <- lm_betaselect(lm_raw, to_standardize = "dv")
lm_beta_w <- lm_betaselect(lm_raw, to_standardize = "mod")
lm_beta_xw <- lm_betaselect(lm_raw, to_standardize = c("mod", "iv"))
lm_beta_yw <- lm_betaselect(lm_raw, to_standardize = c("mod", "dv"))
lm_beta_xy <- lm_betaselect(lm_raw, to_standardize = c("iv", "dv"))
lm_beta_xyw <- lm_betaselect(lm_raw)

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

test_that("Standardize w", {
    expect_equal(
        coef(lm_beta_w), coef(lm_zw),
        ignore_attr = TRUE
      )
  })

test_that("Standardize xy", {
    expect_equal(
        coef(lm_beta_xy), coef(lm_zxzy),
        ignore_attr = TRUE
      )
  })

test_that("Standardize xw", {
    expect_equal(
        coef(lm_beta_xw), coef(lm_zxzw),
        ignore_attr = TRUE
      )
  })

test_that("Standardize yw", {
    expect_equal(
        coef(lm_beta_yw), coef(lm_zyzw),
        ignore_attr = TRUE
      )
  })

test_that("Standardize x, y, and w", {
    expect_equal(
        coef(lm_beta_xyw), coef(lm_zall),
        ignore_attr = TRUE
      )
  })