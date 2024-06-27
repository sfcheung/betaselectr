skip_on_cran()
# A long test with parallel

# Adapted from stdmod

library(testthat)

dat <- data_test_mod_cat

transform0 <- function(data, vars) {
    for (x in vars) {
        data[x] <- scale(data[[x]])[, 1]
      }
    data
  }

dat$dv <- ifelse(dat$dv > mean(dat$dv),
                 yes = 1,
                 no = 0)

lm_raw <- glm(dv ~ iv*mod + cov1 + cat1, dat, family = binomial)
lm_zx  <- glm(dv ~ iv*mod + cov1 + cat1, transform0(dat, c("iv")), family = binomial)
lm_zw  <- glm(dv ~ iv*mod + cov1 + cat1, transform0(dat, c("mod")), family = binomial)
lm_zxzw  <- glm(dv ~ iv*mod + cov1 + cat1, transform0(dat, c("iv", "mod")), family = binomial)
lm_zall  <- glm(dv ~ iv*mod + cov1 + cat1, transform0(dat, c("iv", "mod", "cov1")), family = binomial)
lm_inline  <- glm(dv ~ I(iv^2)*mod + I(1 / cov1) + cat1, transform0(dat, c("iv", "mod", "cov1")), family = binomial)

data_test_mod_cat$dv <- ifelse(data_test_mod_cat$dv > mean(data_test_mod_cat$dv),
                               yes = 1,
                               no = 0)
dat_tmp <- data_test_mod_cat
dat_tmp$dv <- ifelse(dat_tmp$dv > mean(dat_tmp$dv),
                     yes = 1,
                     no = 0)
n <- nrow(dat_tmp)
set.seed(5678)
i <- replicate(6, sample(n, size = n, replace = TRUE), simplify = FALSE)
dat_tmp <- dat_tmp[i[[5]], ]

lm_beta_x <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, bootstrap = 6, iseed = 5678, progress = FALSE, family = binomial)
lm_beta_w <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "mod", do_boot = TRUE, bootstrap = 6, iseed = 5678, progress = FALSE, family = binomial)
lm_beta_xw <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = c("mod", "iv"), do_boot = TRUE, bootstrap = 6, iseed = 5678, progress = FALSE, family = binomial)

test_that("Standardize x", {
    tmp1 <- lm_beta_x$lm_betaselect$boot_out[[5]]$coef_std
    tmp2 <- coef(update(lm_zx, data = transform0(dat_tmp, c("iv"))))
    expect_equal(
        tmp1, tmp2,
        ignore_attr = TRUE
      )
  })

test_that("Standardize w", {
    tmp1 <- lm_beta_w$lm_betaselect$boot_out[[5]]$coef_std
    tmp2 <- coef(update(lm_zw, data = transform0(dat_tmp, c("mod"))))
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
