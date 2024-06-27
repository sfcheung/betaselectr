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

dat_tmp <- dat
dat_tmp$iv <- scale(dat$iv, scale = FALSE, center = TRUE)[, 1]
dat_tmp$mod <- scale(dat$mod, scale = sd(dat$mod), center = FALSE)[, 1]

lm_beta_x <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "iv", do_boot = FALSE, family = binomial)
lm_beta_w <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "mod", do_boot = FALSE, family = binomial)
lm_beta_xw <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = c("mod", "iv"), do_boot = FALSE, family = binomial)
lm_beta_inline <- glm_betaselect(dv ~ I(iv^2)*mod + I(1/ cov1) + cat1, dat, not_to_standardize = "dv", do_boot = FALSE, family = binomial)

test_that("Standardize x", {
    expect_equal(
        coef(lm_beta_x), coef(lm_zx),
        ignore_attr = TRUE
      )
  })

test_that("Standardize w", {
    expect_equal(
        coef(lm_beta_w), coef(lm_zw),
        ignore_attr = TRUE
      )
  })

test_that("Standardize xw", {
    expect_equal(
        coef(lm_beta_xw), coef(lm_zxzw),
        ignore_attr = TRUE
      )
  })

test_that("Inline terms", {
    expect_equal(
        coef(lm_beta_inline), coef(lm_inline),
        ignore_attr = TRUE
      )
  })

test_that("print.glm_betaselect", {
    expect_output(print(lm_beta_x),
                  "Variable(s) standardized: iv", fixed = TRUE)
    expect_output(print(lm_beta_x),
                  "betaselectr::std_data", fixed = TRUE)
    expect_output(print(lm_beta_x, type = "raw"),
                  "data = dat", fixed = TRUE)
  })

test_that("raw_output", {
    expect_identical(coef(raw_output(lm_beta_xw)),
                     coef(lm_raw))
  })
