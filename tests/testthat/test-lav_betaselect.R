# testthat::test_file("./tests/testthat/test_std_selected_lavaan.R")

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
           fixed.x = FALSE)

est <- parameterEstimates(fit,
                          standardized = TRUE,
                          ci = FALSE)
std <- standardizedSolution(fit)
std_nox <- standardizedSolution(fit, type = "std.nox")
std_lv <- standardizedSolution(fit, type = "std.lv")

test_that("Standardized coefficients and SEs", {
  i <- c(2, 5, 8, 11, 12)
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
            fixed.x = FALSE)
std2 <- standardizedSolution(fit2)
std2_nox <- standardizedSolution(fit2, type = "std.nox")

test_that("Alternate values", {
  fit_std_2 <- gen_std_i(fit = fit, i = 2)
  expect_equal(as.vector(fit_std_2(coef(fit2))),
               std2[2, "est.std"],
               ignore_attr = TRUE)
})

# Delta method

test_that("All est", {
  out <- lav_betaselect(fit, standardized = TRUE, skip_categorical_x = FALSE)
  expect_equal(out$std.all,
               out$std.p,
               ignore_attr = TRUE)
  expect_output(print(out, output = "text"),
                "Estimates")
  expect_output(print(out, output = "text", standardized_only = TRUE),
                "Standardized")
})



# (which(std_nox$est.std != std$est.std))
#  4  5  6 11 12 13 14 15 16

test_that("Standardized coefficients: No X", {
  i <- c(4, 11, 15)
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
#   1  2  3  4  5  6  7  8  9 11 12 13 14 15 16

test_that("Standardized coefficients: lv", {
  i <- c(2)
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

