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
                 logLik(raw_output(lm_beta_x)))
    expect_equal(logLik(lm_beta_y),
                 logLik(lm_raw_dv))
    expect_equal(logLik(raw_output(lm_beta_y)),
                 logLik(lm_raw))
    expect_equal(logLik(lm_beta_w),
                 logLik(lm_raw))
    expect_equal(logLik(raw_output(lm_beta_w)),
                 logLik(lm_raw))
  })

test_that("extractAIC", {
    expect_equal(extractAIC(lm_beta_y),
                 extractAIC(lm_raw_dv))
    expect_equal(extractAIC(lm_beta_y),
                 extractAIC(lm_raw_dv))
    expect_equal(extractAIC(raw_output(lm_beta_y)),
                 extractAIC(lm_raw))
  })

test_that("deviance", {
    expect_equal(deviance(lm_beta_y),
                 deviance(lm_raw_dv))
    expect_equal(deviance(lm_beta_y),
                 deviance(lm_raw_dv))
    expect_equal(deviance(raw_output(lm_beta_xyw)),
                 deviance(lm_raw))
  })

test_that("fitted", {
    expect_equal(fitted(lm_beta_y),
                 fitted(lm_raw_dv))
    expect_equal(fitted(lm_beta_y),
                 fitted(lm_raw_dv))
    expect_equal(fitted(raw_output(lm_beta_xyw)),
                 fitted(lm_raw))
  })

test_that("plot.lm", {
    skip("To be tested in an interactive section")
    # Should be tested in an interactive session
    plot(lm_beta_y)
    plot(get_raqw(lm_beta_y))
  })

test_that("predict", {
    expect_equal(predict(lm_beta_y),
                 predict(lm_raw_dv))
    expect_equal(predict(lm_beta_y),
                 predict(lm_raw_dv))
    expect_equal(predict(lm_beta_xyw, model_type = "raw"),
                 predict(lm_raw))
    expect_equal(predict(lm_beta_y, interval = "confidence"),
                 predict(lm_raw_dv, interval = "confidence"))
    expect_equal(predict(lm_beta_xyw, model_type = "raw"),
                 predict(lm_raw))
    expect_equal(predict(lm_beta_y, newdata = data_test_mod_cat[10:20, ]),
                 predict(lm_raw_dv, newdata = dat_tmp[10:20, ]))
  })

# add1

lm_raw_0 <- lm(dv ~ iv + mod, data_test_mod_cat)
lm_raw_1a <- lm(dv ~ iv + mod + cov1, data_test_mod_cat)
lm_raw_1b <- lm(dv ~ iv + mod + cat1, data_test_mod_cat)
add1(lm_raw_0, ~ . + cov1 + cat1)
extractAIC(lm_raw_1a)
extractAIC(lm_raw_1b)
anova(lm_raw_1a)["Residuals", "Sum Sq"]
anova(lm_raw_1b)["Residuals", "Sum Sq"]
drop1(lm_raw_1b)
drop1(lm_raw_1a, ~ cov1)
extractAIC(lm_raw_0)
anova(lm_raw_0)["Residuals", "Sum Sq"]

dat_tmp2 <- data_test_mod_cat
dat_tmp2$iv <- scale(dat_tmp2$iv)[, 1]
dat_tmp2$cov1 <- scale(dat_tmp2$cov1)[, 1]
dat_tmp2$dv <- scale(dat_tmp2$dv)[, 1]
lm_beta_manual_0 <- lm(dv ~ iv + mod, dat_tmp2)
lm_beta_manual_1a <- lm(dv ~ iv + mod + cov1, dat_tmp2)
lm_beta_manual_1b <- lm(dv ~ iv + mod + cat1, dat_tmp2)
add1(lm_beta_manual_0, ~ . + cov1 + cat1)
extractAIC(lm_beta_manual_1a)
extractAIC(lm_beta_manual_1b)
anova(lm_beta_manual_1a)["Residuals", "Sum Sq"]
anova(lm_beta_manual_1b)["Residuals", "Sum Sq"]
drop1(lm_beta_manual_1b)
drop1(lm_beta_manual_1a, ~ cov1)
extractAIC(lm_beta_manual_0)
anova(lm_beta_manual_0)["(lm_beta_manual_0siduals", "Sum Sq"]

test_that("add1() and drop1()", {
    lm_beta_0 <- lm_betaselect(dv ~ iv + mod, data_test_mod_cat, to_standardize = c("iv", "dv", "cov1"), progress = FALSE, do_boot = FALSE)
    lm_beta_1a <- lm_betaselect(dv ~ iv + mod + cov1, data_test_mod_cat, to_standardize = c("iv", "dv", "cov1"), progress = FALSE, do_boot = FALSE)
    lm_beta_1b <- lm_betaselect(dv ~ iv + mod + cat1, data_test_mod_cat, to_standardize = c("iv", "dv", "cov1"), progress = FALSE, do_boot = FALSE)
    add1_out <- add1(lm_beta_0, ~ . + cov1 + cat1)
    expect_equal(add1_out["cov1", "AIC"],
                 extractAIC(lm_beta_manual_1a)[2])
    expect_equal(add1_out["cat1", "AIC"],
                 extractAIC(lm_beta_manual_1b)[2])
    expect_equal(add1_out["cov1", "RSS"],
                 anova(lm_beta_manual_1a)["Residuals", "Sum Sq"])
    expect_equal(add1_out["cat1", "RSS"],
                 anova(lm_beta_manual_1b)["Residuals", "Sum Sq"])
    drop1_out1b <- drop1(lm_beta_1b)
    drop1_out1a <- drop1(lm_beta_1a, ~ cov1)
    expect_equal(drop1_out1b["cat1", "AIC"],
                 extractAIC(lm_beta_manual_0)[2])
    expect_equal(drop1_out1b["cat1", "RSS"],
                 anova(lm_beta_manual_0)["Residuals", "Sum Sq"])
  })

lm_beta_u0 <- lm_betaselect(dv ~ iv, data_test_mod_cat, to_standardize = "iv", do_boot = FALSE)
lm_beta_u1 <- lm_betaselect(dv ~ iv + mod + cov1, data_test_mod_cat, to_standardize = "iv", do_boot = FALSE)
lm_beta_u2 <- lm_betaselect(dv ~ iv*mod + cov1, data_test_mod_cat, to_standardize = "iv", do_boot = FALSE)
lm_beta_u3 <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = FALSE)
lm_beta_u4 <- lm_betaselect(dv ~ iv + mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = FALSE)
lm_beta_u5 <- lm_betaselect(dv ~ iv + mod + cov1, data_test_mod_cat, to_standardize = "dv", do_boot = FALSE)
lm_beta_u6 <- lm_betaselect(dv ~ iv + mod + cov1, data_test_mod_cat[20:50, ], to_standardize = "iv", do_boot = FALSE)

test_that("getCall", {
    expect_equal(as.character(getCall(lm_beta_u1)[[1]])[3],
                 "lm_betaselect")
    expect_equal(as.character(getCall(lm_beta_u1, what = "beta")[[1]])[3],
                 "lm")
    expect_equal(as.character(as.list(getCall(lm_beta_u1, what = "beta")$data)[[1]])[3],
                 "std_data")
    expect_equal(as.character(getCall(lm_beta_u1, what = "raw")[[1]])[3],
                 "lm")
  })

test_that("update", {
    lm_beta_tmp <- update(lm_beta_u1, ~ . + cat1)
    expect_equal(coef(lm_beta_tmp),
                 coef(lm_beta_u4))
    lm_beta_tmp <- update(lm_beta_u4, ~ . - cat1)
    expect_equal(coef(lm_beta_tmp),
                 coef(lm_beta_u1))
    lm_beta_tmp <- update(lm_beta_u0, ~ . + mod + cov1 + cat1)
    expect_equal(coef(lm_beta_tmp),
                 coef(lm_beta_u4))
    lm_beta_tmp <- update(lm_beta_u4, ~ . - cat1 - cov1 - mod)
    expect_equal(coef(lm_beta_tmp),
                 coef(lm_beta_u0))
    lm_beta_tmp <- update(lm_beta_u1, ~ . + iv*mod)
    expect_equal(sort(coef(lm_beta_tmp)),
                 sort(coef(lm_beta_u2)))
    lm_beta_tmp <- update(lm_beta_u1, to_standardize = "dv")
    expect_equal(sort(coef(lm_beta_tmp)),
                 sort(coef(lm_beta_u5)))
    lm_beta_tmp <- update(lm_beta_u0, ~ . + mod + cov1, data = data_test_mod_cat[20:50, ])
    expect_equal(sort(coef(lm_beta_tmp)),
                 sort(coef(lm_beta_u6)))
  })
