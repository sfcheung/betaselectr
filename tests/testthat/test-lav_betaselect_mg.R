skip_on_cran()

# Long tests

# testthat::test_file("./tests/testthat/test_std_selected_lavaan_mg.R")

library(testthat)
library(lavaan)
library(manymome)

dat <- HolzingerSwineford1939
dat$age_gp <- dat$ageyr
dat <- dat[(dat$age_gp >= 12) & (dat$age_gp <= 15), ]
table(dat$age_gp)
tmp <- factor2var(dat$age_gp)
dat[, c("age13", "age14", "age15")] <- tmp
head(dat)
dat$age13b <- dat$age13

mod <-
"
f1 =~ x1 + x2 + x3
f1 ~ age13 + age14 + age15
"

fit <- sem(mod,
           dat,
           fixed.x = FALSE,
           group = "school")

est <- parameterEstimates(fit,
                          standardized = TRUE,
                          ci = FALSE)
std <- standardizedSolution(fit)
std_nox <- standardizedSolution(fit, type = "std.nox")
std_lv <- standardizedSolution(fit, type = "std.lv")

test_that("Standardized coefficient", {
  i <- c(2, 29, 38, 31, 33)
  fit_est <- coef(fit)
  fit_vcov <- vcov(fit)
  fit_std <- lapply(i, function(x) gen_std_i(fit = fit, i = x))
  std_se <- sapply(fit_std,
                   FUN = std_se_delta,
                   fit_est = fit_est,
                   fit_vcov = fit_vcov)
  std_est <- sapply(fit_std,
                    function(x) x(fit_est))
  expect_equal(std_est,
               std[i, "est.std"],
               ignore_attr = TRUE)
  expect_equal(std_se,
               std[i, "se"],
               ignore_attr = TRUE)
})

fit2 <- sem(mod,
            dat[c(1:100, 150:250), ],
            fixed.x = FALSE,
            group = "school")
std2 <- standardizedSolution(fit2)
std2_nox <- standardizedSolution(fit2, type = "std.nox")

test_that("Alternate values", {
  fit_std_2 <- gen_std_i(fit = fit, i = 29)
  expect_equal(as.vector(fit_std_2(coef(fit2))),
               std2[29, "est.std"],
               ignore_attr = TRUE)
})

# Delta method

test_that("All est", {
  out <- lav_betaselect(fit, standardized = TRUE, skip_categorical_x = FALSE)
  i <- out$op %in% c("~~", "~", "=~")
  expect_equal(out$std.all[i],
               out$std.p[i],
               ignore_attr = TRUE)
})

test_that("Intercepts", {
  out <- lav_betaselect(fit, standardized = TRUE, skip_categorical_x = FALSE, std_intercept = TRUE)
  i <- out$op %in% c("~1")
  expect_equal(out$std.all[i],
               out$std.p[i],
               ignore_attr = TRUE)
})

# (which(std_nox$est.std != std$est.std))
#  4  5  6 11 12 13 14 15 16 20 21 22 27 28 29 34 35 36 37 38 39 43 44 45

test_that("Standardized coefficients: No X", {
  i <- c(4, 28, 38)
  fit_est <- coef(fit)
  fit_vcov <- vcov(fit)
  fit_std <- lapply(i, function(x) gen_std_i(fit = fit, i = x, to_standardize = "f1"))
  std_se <- sapply(fit_std,
                   FUN = std_se_delta,
                   fit_est = fit_est,
                   fit_vcov = fit_vcov)
  std_est <- sapply(fit_std,
                    function(x) x(fit_est))
  expect_equal(std_est,
               std_nox[i, "est.std"],
               ignore_attr = TRUE)
  expect_equal(std_se,
               std_nox[i, "se"],
               ignore_attr = TRUE)
})

# (which(std_lv$est.std != std$est.std))
#  1  2  3  4  5  6  7  8  9 11 12 13 14 15 16 17 18 19 20 21 22 24 25 26 27 28 29 30 31 32 34 35 36 37 38 39 40 41 42 43 44 45

test_that("Standardized coefficients: lv", {
  i <- c(25)
  fit_est <- coef(fit)
  fit_vcov <- vcov(fit)
  fit_std <- lapply(i, function(x) gen_std_i(fit = fit, i = x, to_standardize = "f1"))
  std_se <- sapply(fit_std,
                   FUN = std_se_delta,
                   fit_est = fit_est,
                   fit_vcov = fit_vcov)
  std_est <- sapply(fit_std,
                    function(x) x(fit_est))
  expect_equal(std_est,
               std_lv[i, "est.std"],
               ignore_attr = TRUE)
  expect_equal(std_se,
               std_lv[i, "se"],
               ignore_attr = TRUE)
})

# Automatically skip ageXX
test_that("to_standardize", {
  std_out <- lav_betaselect(fit)
  std_nox_out1 <- lav_betaselect(fit,
                                      to_standardize = c("f1", "x1", "x2", "x3"))
  std_nox_out2 <- lav_betaselect(fit,
                                      not_to_standardize = c("age13", "age14", "age15"))
  expect_equal(std_nox_out1$std.p, std_nox_out2$std.p,
               ignore_attr = TRUE)
  expect_equal(std_nox_out1$std.p, std_out$std.p,
               ignore_attr = TRUE)
  expect_output(print(std_out, standardized_only = FALSE),
                "Estimates")
  expect_output(print(std_out, standardized_only = TRUE),
                "BetaSelect")
})
