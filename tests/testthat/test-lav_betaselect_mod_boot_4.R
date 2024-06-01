skip_on_cran()
# Parallel processing
# Long test

# testthat::test_file("./tests/testthat/test_std_selected_lavaan_mod_boot_3.R")

library(manymome)

#Load a test data of 500 cases
data(data_test_medmod)
library(lavaan)
dat <- data_test_medmod
dat$group <- rep(c("gp1", "gp2"), nrow(dat))[seq_len(nrow(dat))]
dat$iv_mod <- dat$iv * dat$mod
head(dat)

mod <-
"
dv ~ med + cov2
"
fit_gp <- sem(mod,
              data = dat,
              group = "group")

test_that("Get do_boot results", {
  fit_boot_gp <- sem(mod,
                    data = dat,
                    group = "group",
                    se = "boot",
                    bootstrap = 50,
                    iseed = 234567)

  out_boot_gp <- lav_betaselect(fit_boot_gp,
                                standardized = TRUE,
                                not_to_standardize = c("dv", "med", "cov2"),
                                progress = FALSE,
                                std_se = "bootstrap")

  out_boot_lav_gp <- lav_betaselect(fit_gp,
                                    standardized = TRUE,
                                    not_to_standardize = c("dv", "med", "cov2"),
                                    progress = FALSE,
                                    std_se = "bootstrap",
                                    bootstrap = 50,
                                    iseed = 234567)

  expect_equal(round(out_boot_lav_gp[c(1:4, 10:13), "std.p.se"], 2),
               round(out_boot_gp[c(1:4, 10:13), "se"], 2),
               tolerance = 1e-3,
               ignore_attr = TRUE)

})
