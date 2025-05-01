skip_on_cran()
# Parallel processing
# Long test

# testthat::test_file("./tests/testthat/test_std_selected_lavaan_mod_boot_1.R")

library(manymome)

#Load a test data of 500 cases
data(data_test_medmod)
library(lavaan)
dat <- data_test_medmod
dat$iv <- dat$iv - mean(dat$iv)
dat$mod <- dat$mod - mean(dat$mod)
dat$iv_mod <- dat$iv * dat$mod
head(dat)

mod <-
"
med ~ iv + mod + iv_mod + cov1
dv ~ med + cov2
"
fit <- sem(mod,
           data = dat)

test_that("Get do_boot results", {

  fit_boot <- sem(mod,
                  data = dat,
                  se = "boot",
                  bootstrap = 50,
                  iseed = 34567)
  boot_out <- do_boot(fit,
                      R = 50,
                      seed = 34567,
                      parallel = FALSE,
                      progress = FALSE)

  out <- lav_betaselect(fit,
                        standardized = TRUE,
                        not_to_standardize = c("dv", "med", "cov2"),
                        progress = FALSE,
                        std_se = "bootstrap",
                        boot_out = boot_out)

  out_boot <- lav_betaselect(fit_boot,
                             standardized = TRUE,
                             not_to_standardize = c("dv", "med", "cov2"),
                             progress = FALSE,
                             std_se = "bootstrap")

  expect_equal(round(out[5:8, "std.p.se"], 2),
               round(out_boot[5:8, "se"], 2),
               tolerance = 1e-2,
               ignore_attr = TRUE)
  expect_output(print(out, standardized_only = FALSE),
                "Estimates")
  expect_output(print(out, standardized_only = TRUE),
                "BetaSelect")
})

test_that("boot_out error", {
  expect_error(lav_betaselect(fit,
                              standardized = TRUE,
                              not_to_standardize = c("dv", "med", "cov2"),
                              std_se = "bootstrap",
                              boot_out = 123),
               regexp = "boot_out")
})

