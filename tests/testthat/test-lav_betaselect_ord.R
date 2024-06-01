skip_on_cran()

# testthat::test_file("./tests/testthat/test_std_selected_lavaan_ord.R")

library(testthat)
library(lavaan)
# library(manymome)
library(numDeriv)

dat <- HolzingerSwineford1939
# Dichotomous
dat$x1 <- ifelse(dat$x1 > mean(dat$x1), 1, 0)
dat$x2 <- ifelse(dat$x2 > mean(dat$x2), 1, 0)
dat$x3 <- ifelse(dat$x3 > mean(dat$x3), 1, 0)
# Three categories
dat$x4 <- as.numeric(cut(dat$x4, breaks = 3))
dat$x5 <- as.numeric(cut(dat$x5, breaks = 3))
dat$x6 <- as.numeric(cut(dat$x6, breaks = 3))
head(dat)

mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f3 =~ x7 + x8 + x9
f1 ~ f2 + f3
"

fit <- sem(mod,
           dat,
           ordered = c("x1", "x2", "x3",
                       "x4", "x5", "x6"))

lavNames(fit, "ov.ord")
lavNames(fit, "ov.ind")

est <- parameterEstimates(fit, standardized = TRUE)#
# est[, c(1, 2, 3, 4, 11)]

out <- lav_betaselect(fit, standardized = TRUE)
# out[, c(1, 2, 3, 4, 10, 11, 12)]

test_that("Ordinal variables", {
  i <- which(round(out$std.all, 3) != round(out$std.p, 3))
  expect_equal(length(i), 0)
})

fit_gp <- sem(mod,
              dat,
              ordered = c("x1", "x2", "x3",
                          "x4", "x5", "x6"),
              group = "school",
              group.equal = c("loadings"))

est <- parameterEstimates(fit_gp, standardized = TRUE)#
# est[, c(1, 2, 3, 4, 11)]

out <- lav_betaselect(fit_gp, standardized = TRUE)
# out[, c(1, 2, 3, 4, 10, 11, 12)]

test_that("Ordinal variables: Multigroup", {
  i <- which(round(out$std.all, 3) != round(out$std.p, 3))
  expect_equal(length(i), 0)
  expect_output(print(out, output = "text"),
                "Estimates")
  expect_output(print(out, output = "text", standardized_only = TRUE),
                "Estimates")
})
