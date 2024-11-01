skip_on_cran()
# A long test with parallel

# Adapted from stdmod

library(testthat)

transform0 <- function(data, vars) {
    for (x in vars) {
        data[x] <- scale(data[[x]])[, 1]
      }
    data
  }

lm_raw <- lm(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat)
lm_zx  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(data_test_mod_cat, c("iv")))
lm_zw  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(data_test_mod_cat, c("mod")))
lm_zy  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(data_test_mod_cat, c("dv")))
lm_zxzw  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(data_test_mod_cat, c("iv", "mod")))
lm_zxzy  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(data_test_mod_cat, c("iv", "dv")))
lm_zyzw  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(data_test_mod_cat, c("dv", "mod")))
lm_zall  <- lm(dv ~ iv*mod + cov1 + cat1, transform0(data_test_mod_cat, c("iv", "dv", "mod", "cov1")))
lm_inline  <- lm(dv ~ iv*mod + I(sqrt(cov1)) + cat1, transform0(data_test_mod_cat, c("iv", "dv", "mod")))

dat_tmp <- data_test_mod_cat
n <- nrow(dat_tmp)
set.seed(5678)
i <- replicate(6, sample(n, size = n, replace = TRUE), simplify = FALSE)
dat_tmp <- dat_tmp[i[[5]], ]

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, bootstrap = 6, iseed = 5678, progress = FALSE)
# lm_beta_y <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "dv", do_boot = TRUE, bootstrap = 6, iseed = 5678, progress = FALSE, parallel = TRUE, ncpus = 2)
lm_beta_w <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "mod", do_boot = TRUE, bootstrap = 6, iseed = 5678, progress = FALSE)
lm_beta_xw <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = c("mod", "iv"), do_boot = TRUE, bootstrap = 6, iseed = 5678, progress = FALSE)
lm_beta_yw <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = c("mod", "dv"), do_boot = TRUE, bootstrap = 6, iseed = 5678, progress = FALSE)
lm_beta_xy <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = c("iv", "dv"), do_boot = TRUE, bootstrap = 6, iseed = 5678, progress = FALSE)
lm_beta_xyw <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, do_boot = TRUE, bootstrap = 6, iseed = 5678, progress = FALSE)

test_that("Standardize x", {
    tmp1 <- lm_beta_x$lm_betaselect$boot_out[[5]]$coef_std
    tmp2 <- coef(update(lm_zx, data = transform0(dat_tmp, c("iv"))))
    expect_equal(
        tmp1, tmp2,
        ignore_attr = TRUE
      )
  })

# test_that("Standardize y", {
#     tmp1 <- lm_beta_y$lm_betaselect$boot_out[[5]]$coef_std
#     tmp2 <- coef(update(lm_zx, data = transform0(dat_tmp, c("dv"))))
#     expect_equal(
#         tmp1, tmp2,
#         ignore_attr = TRUE
#       )
#   })

test_that("Standardize w", {
    tmp1 <- lm_beta_w$lm_betaselect$boot_out[[5]]$coef_std
    tmp2 <- coef(update(lm_zw, data = transform0(dat_tmp, c("mod"))))
    expect_equal(
        tmp1, tmp2,
        ignore_attr = TRUE
      )
  })

test_that("Standardize xy", {
    tmp1 <- lm_beta_xy$lm_betaselect$boot_out[[5]]$coef_std
    tmp2 <- coef(update(lm_zxzy, data = transform0(dat_tmp, c("iv", "dv"))))
    expect_equal(
        tmp1, tmp2,
        ignore_attr = TRUE
      )
  })

test_that("Standardize xw", {
    tmp1 <- lm_beta_xw$lm_betaselect$boot_out[[5]]$coef_std
    tmp2 <- coef(update(lm_zxzw, data = transform0(dat_tmp, c("iv", "mod"))))
    expect_equal(
        tmp1, tmp2,
        ignore_attr = TRUE
      )
  })

test_that("Standardize yw", {
    tmp1 <- lm_beta_yw$lm_betaselect$boot_out[[5]]$coef_std
    tmp2 <- coef(update(lm_zyzw, data = transform0(dat_tmp, c("dv", "mod"))))
    expect_equal(
        tmp1, tmp2,
        ignore_attr = TRUE
      )
  })

test_that("Standardize x, y, and w", {
    tmp1 <- lm_beta_xyw$lm_betaselect$boot_out[[5]]$coef_std
    tmp2 <- coef(update(lm_zall, data = transform0(dat_tmp, c("iv", "dv", "mod", "cov1"))))
    expect_equal(
        tmp1, tmp2,
        ignore_attr = TRUE
      )
  })
