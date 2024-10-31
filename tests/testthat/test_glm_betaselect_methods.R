# Adapted from stdmod

library(testthat)
library(boot)

data(data_test_mod_cat_binary)
# data_test_mod_cat$dv <- ifelse(data_test_mod_cat$dv > mean(data_test_mod_cat$dv),
#                                yes = 1,
#                                no = 0)

dat <- data_test_mod_cat_binary

transform0 <- function(data, vars) {
    for (x in vars) {
        data[x] <- scale(data[[x]])[, 1]
      }
    data
  }


lm_raw <- glm(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat_binary, family = binomial)
lm_raw2 <- glm(dv ~ iv + mod + cov1 + cat1, data_test_mod_cat_binary, family = binomial)
lm_zx  <- glm(dv ~ iv*mod + cov1 + cat1, transform0(data_test_mod_cat_binary, c("iv")), family = binomial)
lm_zw  <- glm(dv ~ iv*mod + cov1 + cat1, transform0(data_test_mod_cat_binary, c("mod")), family = binomial)
lm_zxzw  <- glm(dv ~ iv*mod + cov1 + cat1, transform0(data_test_mod_cat_binary, c("iv", "mod")), family = binomial)
lm_zall  <- glm(dv ~ iv*mod + cov1 + cat1, transform0(data_test_mod_cat_binary, c("iv", "mod", "cov1")), family = binomial)
lm_inline  <- glm(dv ~ I(iv^2)*mod + I(1 / cov1) + cat1, transform0(data_test_mod_cat_binary, c("iv", "mod", "cov1")), family = binomial)
lm_inline_raw  <- glm(dv ~ I(iv^2)*mod + I(1 / cov1) + cat1, data_test_mod_cat_binary, family = binomial)

dat_tmp <- data_test_mod_cat_binary
# dat_tmp$iv <- scale(dat$iv, scale = FALSE, center = TRUE)[, 1]
# dat_tmp$mod <- scale(dat$mod, scale = sd(dat$mod), center = FALSE)[, 1]
dat_tmp$iv <- scale(dat$iv)[, 1]
lm_raw_x <- glm(dv ~ iv*mod + cov1 + cat1, dat_tmp, family = binomial)

lm_beta_x <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat_binary, to_standardize = "iv", do_boot = FALSE, family = binomial)
lm_beta_x2 <- glm_betaselect(dv ~ iv + mod + cov1 + cat1, data_test_mod_cat_binary, to_standardize = "iv", do_boot = FALSE, family = binomial)
lm_beta_w <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat_binary, to_standardize = "mod", do_boot = FALSE, family = binomial)
lm_beta_xw <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat_binary, to_standardize = c("mod", "iv"), do_boot = FALSE, family = binomial)
lm_beta_inline <- glm_betaselect(dv ~ I(iv^2)*mod + I(1/ cov1) + cat1, data_test_mod_cat_binary, not_to_standardize = "dv", do_boot = FALSE, family = binomial)
lm_beta_xyw_boot <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat_binary, not_to_standardize = "dv", do_boot = TRUE, bootstrap = 100, iseed = 1234, progress = FALSE, family = binomial)

set.seed(1234)
n <- nrow(data_test_mod_cat_binary)
i <- replicate(100, sample(n, size = n, replace = TRUE), simplify = FALSE)
tmp <- sapply(i, function(xx) {
          coef(glm(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat_binary[xx, ], family = binomial))
        })
vcov_raw_chk <- cov(t(tmp))
set.seed(1234)
lm_raw_boot <- boot(data_test_mod_cat_binary,
                    function(d, i) {
                        coef(glm(dv ~ iv*mod + cov1 + cat1, d[i, ], family = binomial))
                      },
                    R = 100,
                    simple = TRUE)

test_that("coef", {
    expect_identical(lm_beta_x$coefficients,
                     coef(lm_beta_x))
    expect_identical(lm_raw$coefficients,
                     coef(lm_beta_x, type = "raw"))
    expect_identical(lm_beta_xw$coefficients,
                     coef(lm_beta_xw))
    expect_identical(lm_raw$coefficients,
                     coef(lm_beta_xw, type = "raw"))
    expect_identical(lm_beta_inline$coefficients,
                     coef(lm_beta_inline))
    expect_identical(lm_inline_raw$coefficients,
                     coef(lm_beta_inline, type = "raw"))
  })

test_that("vcov", {
    # expect_warning(expect_warning(vcov(lm_beta_x), "changed"),
    #                "should not")
    expect_warning(vcov(lm_beta_x, method = "default"))
    expect_equal(vcov(lm_beta_xyw_boot, method = "boot"),
                 vcov(lm_beta_xyw_boot))
    expect_warning(vcov(lm_beta_xyw_boot, method = "default"),)
    expect_equal(vcov(lm_beta_xyw_boot, method = "default", type = "raw"),
                 vcov(lm_raw))
    expect_equal(vcov(lm_beta_xyw_boot, method = "boot", type = "raw"),
                 vcov_raw_chk)
  })

test_that("confint", {
    # expect_warning(expect_warning(suppressMessages(confint(lm_beta_x)), "changed"),
    #                "should not")
    expect_warning(suppressMessages(confint(lm_beta_x, method = "ls")))
    expect_equal(confint(lm_beta_xyw_boot, method = "boot", level = .80,
                         parm = c("(Intercept)", "cat1gp2")),
                 confint(lm_beta_xyw_boot, level = .80,
                         parm = c("(Intercept)", "cat1gp2")))
    expect_warning(suppressMessages(confint(lm_beta_xyw_boot, method = "ls")))
    expect_equal(suppressMessages(confint(lm_beta_xyw_boot, method = "ls", type = "raw",
                 level = .75, parm = "iv")),
                 suppressMessages(confint(lm_raw, level = .75, parm = "iv")))
    expect_equal(as.vector(confint(lm_beta_xyw_boot, method = "boot", type = "raw", parm = "mod", level = .90)),
                 boot.ci(lm_raw_boot, type = "perc", index = 3, conf = .90)$perc[4:5])
  })

test_that("anova", {
    expect_equal(anova(lm_beta_x),
                 anova(lm_raw))
    expect_equal(anova(lm_beta_x, lm_beta_x2),
                 anova(lm_raw, lm_raw2))
    expect_equal(anova(lm_beta_xw, type = "raw"),
                 anova(lm_raw))
    expect_equal(anova(lm_beta_x, lm_beta_x2, type = "raw"),
                 anova(lm_raw, lm_raw2))
  })

test_that("summary", {
    lm_beta_x_lm <- lm_beta_x
    class(lm_beta_x_lm) <- "glm"
    # expect_warning(summary(lm_beta_x),
                  #  "changed")
    expect_equal(summary(lm_beta_x, type = "raw", se_method = "default", ci = FALSE)$coefficients,
                 summary(lm_raw)$coefficients)
    expect_equal(summary(lm_beta_x, type = "beta", se_method = "default", ci = FALSE)$coefficients,
                 summary(lm_beta_x_lm)$coefficients)
    expect_no_error(summary(lm_beta_xyw_boot))
    expect_equal(summary(lm_beta_xyw_boot, ci = TRUE, level = .90)$coefficients[, 2:3],
                 confint(lm_beta_xyw_boot, level = .90),
                 ignore_attr = TRUE)
    expect_equal(suppressMessages(summary(lm_beta_xyw_boot, ci = TRUE, se_method = "default", level = .90)$coefficients[, 2:3]),
                 suppressMessages(confint(lm_beta_xyw_boot, level = .90, method = "default", warn = FALSE)),
                 ignore_attr = TRUE)
  })

test_that("print.summary", {
    expect_output(print(summary(lm_beta_x, type = "raw", se_method = "default")),
                  "*before*", fixed = TRUE)
    expect_output(print(summary(lm_beta_x, type = "beta", se_method = "default")),
                  "should not be used", fixed = TRUE)
    expect_output(print(summary(lm_beta_xyw_boot)),
                  "asymmetric", fixed = TRUE)
    expect_output(print(summary(lm_beta_xyw_boot, ci = TRUE, level = .90)),
                  "Percentile", fixed = TRUE)
    expect_output(print(suppressMessages(summary(lm_beta_xyw_boot, ci = TRUE, se_method = "default", level = .90))),
                  "should not be used")
  })

test_that("logLik", {
    expect_equal(logLik(lm_beta_x),
                 logLik(raw_output(lm_beta_x)))
    expect_equal(logLik(lm_beta_w),
                 logLik(lm_raw))
    expect_equal(logLik(raw_output(lm_beta_w)),
                 logLik(lm_raw))
  })

test_that("extractAIC", {
    expect_equal(extractAIC(lm_beta_w),
                 extractAIC(lm_raw))
    expect_equal(extractAIC(lm_beta_x),
                 extractAIC(lm_raw))
    expect_equal(extractAIC(raw_output(lm_beta_x)),
                 extractAIC(lm_raw))
  })

test_that("deviance", {
    expect_equal(deviance(lm_beta_x),
                 deviance(lm_raw))
    expect_equal(deviance(lm_beta_w),
                 deviance(lm_raw))
    expect_equal(deviance(raw_output(lm_beta_xw)),
                 deviance(lm_raw))
  })

test_that("fitted", {
    expect_equal(fitted(lm_beta_x),
                 fitted(lm_raw))
    expect_equal(fitted(lm_beta_w),
                 fitted(lm_raw))
    expect_equal(fitted(raw_output(lm_beta_xw)),
                 fitted(lm_raw))
  })

test_that("plot.lm", {
    skip("To be tested in an interactive section")
    # Should be tested in an interactive session
    plot(lm_beta_x, which = 1)
    plot(raw_output(lm_beta_x), which = 1)
  })

test_that("predict", {
    expect_equal(predict(lm_beta_xw, model_type = "raw"),
                 predict(lm_raw))
    dat_tmp3 <- data_test_mod_cat_binary[10:20, ]
    dat_tmp3$iv <- scale(dat_tmp3$iv)[, 1]
    expect_equal(predict(lm_beta_x, newdata = data_test_mod_cat_binary[10:20, ]),
                 predict(lm_raw_x, newdata = dat_tmp3))
  })

# add1

lm_raw_0 <- glm(dv ~ iv + mod, data_test_mod_cat_binary, family = binomial)
lm_raw_1a <- glm(dv ~ iv + mod + cov1, data_test_mod_cat_binary, family = binomial)
lm_raw_1b <- glm(dv ~ iv + mod + cat1, data_test_mod_cat_binary, family = binomial)
add1(lm_raw_0, ~ . + cov1 + cat1)
extractAIC(lm_raw_1a)
extractAIC(lm_raw_1b)
# anova(lm_raw_1a)["Residuals", "Sum Sq"]
# anova(lm_raw_1b)["Residuals", "Sum Sq"]
drop1(lm_raw_1b)
drop1(lm_raw_1a, ~ cov1)
extractAIC(lm_raw_0)
# anova(lm_raw_0)["Residuals", "Sum Sq"]

dat_tmp2 <- data_test_mod_cat_binary
dat_tmp2$iv <- scale(dat_tmp2$iv)[, 1]
dat_tmp2$cov1 <- scale(dat_tmp2$cov1)[, 1]
# dat_tmp2$dv <- scale(dat_tmp2$dv)[, 1]
lm_beta_manual_0 <- glm(dv ~ iv + mod, dat_tmp2, family = binomial)
lm_beta_manual_1a <- glm(dv ~ iv + mod + cov1, dat_tmp2, family = binomial)
lm_beta_manual_1b <- glm(dv ~ iv + mod + cat1, dat_tmp2, family = binomial)
add1(lm_beta_manual_0, ~ . + cov1 + cat1)
extractAIC(lm_beta_manual_1a)
extractAIC(lm_beta_manual_1b)
# anova(lm_beta_manual_1a)["Residuals", "Sum Sq"]
# anova(lm_beta_manual_1b)["Residuals", "Sum Sq"]
drop1(lm_beta_manual_1b)
drop1(lm_beta_manual_1a, ~ cov1)
extractAIC(lm_beta_manual_0)
# anova(lm_beta_manual_0)["(lm_beta_manual_0siduals", "Sum Sq"]

test_that("add1() and drop1()", {
    lm_beta_0 <- glm_betaselect(dv ~ iv + mod, data_test_mod_cat_binary, to_standardize = c("iv", "cov1"), progress = FALSE, do_boot = FALSE, family = binomial)
    lm_beta_1a <- glm_betaselect(dv ~ iv + mod + cov1, data_test_mod_cat_binary, to_standardize = c("iv", "cov1"), progress = FALSE, do_boot = FALSE, family = binomial)
    lm_beta_1b <- glm_betaselect(dv ~ iv + mod + cat1, data_test_mod_cat_binary, to_standardize = c("iv", "cov1"), progress = FALSE, do_boot = FALSE, family = binomial)
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

lm_beta_u0 <- glm_betaselect(dv ~ iv, data_test_mod_cat_binary, to_standardize = "iv", do_boot = FALSE, family = binomial)
lm_beta_u1 <- glm_betaselect(dv ~ iv + mod + cov1, data_test_mod_cat_binary, to_standardize = "iv", do_boot = FALSE, family = binomial)
lm_beta_u2 <- glm_betaselect(dv ~ iv*mod + cov1, data_test_mod_cat_binary, to_standardize = "iv", do_boot = FALSE, family = binomial)
lm_beta_u3 <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat_binary, to_standardize = "iv", do_boot = FALSE, family = binomial)
lm_beta_u4 <- glm_betaselect(dv ~ iv + mod + cov1 + cat1, data_test_mod_cat_binary, to_standardize = "iv", do_boot = FALSE, family = binomial)
lm_beta_u5 <- glm_betaselect(dv ~ iv + mod + cov1, data_test_mod_cat_binary, to_standardize = "mod", do_boot = FALSE, family = binomial)
lm_beta_u6 <- glm_betaselect(dv ~ iv + mod + cov1, data_test_mod_cat_binary[20:50, ], to_standardize = "iv", do_boot = FALSE, family = binomial)

test_that("getCall", {
    expect_equal(as.character(getCall(lm_beta_u1)[[1]])[3],
                 "lm_betaselect")
    expect_equal(as.character(getCall(lm_beta_u1, what = "beta")[[1]])[3],
                 "glm")
    expect_equal(as.character(as.list(getCall(lm_beta_u1, what = "beta")$data)[[1]])[3],
                 "std_data")
    expect_equal(as.character(getCall(lm_beta_u1, what = "raw")[[1]])[3],
                 "glm")
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
    lm_beta_tmp <- update(lm_beta_u1, to_standardize = "mod")
    expect_equal(sort(coef(lm_beta_tmp)),
                 sort(coef(lm_beta_u5)))
    lm_beta_tmp <- update(lm_beta_u0, ~ . + mod + cov1, data = data_test_mod_cat_binary[20:50, ])
    expect_equal(sort(coef(lm_beta_tmp)),
                 sort(coef(lm_beta_u6)))
  })
