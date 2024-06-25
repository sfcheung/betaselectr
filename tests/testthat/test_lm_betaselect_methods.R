# Adapted from stdmod

library(testthat)
library(boot)

dat_tmp <- data_test_mod_cat
dat_tmp$dv <- scale(data_test_mod_cat$dv)[, 1]
dat_tmp$mod <- scale(data_test_mod_cat$mod, scale = sd(data_test_mod_cat$mod), center = FALSE)[, 1]

lm_raw <- lm(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat)
lm_raw2 <- lm(dv ~ iv + mod + cov1 + cat1, data_test_mod_cat)
lm_inline_raw <- lm(dv ~ I(iv^2)*mod + I(1/ cov1) + cat1,
                    data_test_mod_cat)
lm_raw_dv <- lm(dv ~ iv*mod + cov1 + cat1, dat_tmp)

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = FALSE)
lm_beta_x2 <- lm_betaselect(dv ~ iv + mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = FALSE)
lm_beta_y <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "dv", do_boot = FALSE)
lm_beta_w <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "mod", do_boot = FALSE)
lm_beta_xw <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = c("mod", "iv"), do_boot = FALSE)
lm_beta_yw <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = c("mod", "dv"), do_boot = FALSE)
lm_beta_xy <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = c("iv", "dv"), do_boot = FALSE)
lm_beta_xyw <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, do_boot = FALSE)
lm_beta_inline <- lm_betaselect(dv ~ I(iv^2)*mod + I(1/ cov1) + cat1, data_test_mod_cat, do_boot = FALSE)
lm_beta_xyw_boot <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, do_boot = TRUE, bootstrap = 100, iseed = 1234, progress = FALSE)

set.seed(1234)
n <- nrow(data_test_mod_cat)
i <- replicate(100, sample(n, size = n, replace = TRUE), simplify = FALSE)
tmp <- sapply(i, function(xx) {
          coef(lm(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat[xx, ]))
        })
vcov_raw_chk <- cov(t(tmp))
set.seed(1234)
lm_raw_boot <- boot(data_test_mod_cat,
                    function(d, i) {
                        coef(lm(dv ~ iv*mod + cov1 + cat1, d[i, ]))
                      },
                    R = 100,
                    simple = TRUE)


test_that("coef", {
    expect_identical(lm_beta_x$coefficients,
                     coef(lm_beta_x))
    expect_identical(lm_raw$coefficients,
                     coef(lm_beta_x, type = "raw"))
    expect_identical(lm_beta_xyw$coefficients,
                     coef(lm_beta_xyw))
    expect_identical(lm_raw$coefficients,
                     coef(lm_beta_xyw, type = "raw"))
    expect_identical(lm_beta_inline$coefficients,
                     coef(lm_beta_inline))
    expect_identical(lm_inline_raw$coefficients,
                     coef(lm_beta_inline, type = "raw"))
  })

test_that("vcov", {
    expect_warning(expect_warning(vcov(lm_beta_x), "changed"),
                   "should not")
    expect_warning(vcov(lm_beta_x, method = "ls"))
    expect_equal(vcov(lm_beta_xyw_boot, method = "boot"),
                 vcov(lm_beta_xyw_boot))
    expect_warning(vcov(lm_beta_xyw_boot, method = "ls"),)
    expect_equal(vcov(lm_beta_xyw_boot, method = "ls", type = "raw"),
                 vcov(lm_raw))
    expect_equal(vcov(lm_beta_xyw_boot, method = "boot", type = "raw"),
                 vcov_raw_chk)
  })


test_that("confint", {
    expect_warning(expect_warning(confint(lm_beta_x), "changed"),
                   "should not")
    expect_warning(confint(lm_beta_x, method = "ls"))
    expect_equal(confint(lm_beta_xyw_boot, method = "boot", level = .80,
                         parm = c("(Intercept)", "cat1gp2")),
                 confint(lm_beta_xyw_boot, level = .80,
                         parm = c("(Intercept)", "cat1gp2")))
    expect_warning(confint(lm_beta_xyw_boot, method = "ls"),)
    expect_equal(confint(lm_beta_xyw_boot, method = "ls", type = "raw",
                 level = .75, parm = "iv"),
                 confint(lm_raw, level = .75, parm = "iv"))
    expect_equal(as.vector(confint(lm_beta_xyw_boot, method = "boot", type = "raw", parm = "mod", level = .90)),
                 boot.ci(lm_raw_boot, type = "perc", index = 3, conf = .90)$perc[4:5])
  })

test_that("anova", {
    expect_equal(anova(lm_beta_x),
                 anova(lm_raw))
    expect_equal(anova(lm_beta_x, lm_beta_x2),
                 anova(lm_raw, lm_raw2))
    expect_equal(anova(lm_beta_xyw, type = "raw"),
                 anova(lm_raw))
    expect_equal(anova(lm_beta_x, lm_beta_x2, type = "raw"),
                 anova(lm_raw, lm_raw2))
  })

test_that("summary", {
    lm_beta_x_lm <- lm_beta_x
    class(lm_beta_x_lm) <- "lm"
    expect_warning(summary(lm_beta_x),
                   "changed")
    expect_equal(summary(lm_beta_x, type = "raw", se_method = "lm")$coefficients,
                 summary(lm_raw)$coefficients)
    expect_equal(summary(lm_beta_x, type = "beta", se_method = "lm")$coefficients,
                 summary(lm_beta_x_lm)$coefficients)
    expect_no_error(summary(lm_beta_xyw_boot))
    expect_equal(summary(lm_beta_xyw_boot, ci = TRUE, level = .90)$coefficients[, 2:3],
                 confint(lm_beta_xyw_boot, level = .90),
                 ignore_attr = TRUE)
    expect_equal(summary(lm_beta_xyw_boot, ci = TRUE, se_method = "lm", level = .90)$coefficients[, 2:3],
                 confint(lm_beta_xyw_boot, level = .90, method = "ls", warn = FALSE),
                 ignore_attr = TRUE)
  })

test_that("print.summary", {
    expect_output(print(summary(lm_beta_x, type = "raw", se_method = "lm")),
                  "*before*", fixed = TRUE)
    expect_output(print(summary(lm_beta_x, type = "beta", se_method = "lm")),
                  "should not be used", fixed = TRUE)
    expect_output(print(summary(lm_beta_xyw_boot)),
                  "asymmetric", fixed = TRUE)
    expect_output(print(summary(lm_beta_xyw_boot, ci = TRUE, level = .90)),
                  "Percentile", fixed = TRUE)
    expect_output(print(summary(lm_beta_xyw_boot, ci = TRUE, se_method = "lm", level = .90)),
                  "should not be used")
  })

test_that("logLik", {
    expect_equal(logLik(lm_beta_x),
                 logLik(lm_beta_x, type = "raw"))
    expect_equal(logLik(lm_beta_y),
                 logLik(lm_raw_dv))
    expect_equal(logLik(lm_beta_y, type = "raw"),
                 logLik(lm_raw))
    expect_equal(logLik(lm_beta_w),
                 logLik(lm_raw))
    expect_equal(logLik(lm_beta_w, type = "raw"),
                 logLik(lm_raw))
  })

test_that("extractAIC", {
    expect_equal(extractAIC(lm_beta_y),
                 extractAIC(lm_raw_dv))
    expect_equal(extractAIC(lm_beta_y),
                 extractAIC(lm_raw_dv))
    expect_equal(extractAIC(lm_beta_y, type = "raw"),
                 extractAIC(lm_raw))
  })

test_that("deviance", {
    expect_equal(deviance(lm_beta_y),
                 deviance(lm_raw_dv))
    expect_equal(deviance(lm_beta_y),
                 deviance(lm_raw_dv))
    expect_equal(deviance(lm_beta_xyw, type = "raw"),
                 deviance(lm_raw))
  })

