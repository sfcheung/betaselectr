skip_on_cran()
# Parallel processing
# Long test

# testthat::test_file("./tests/testthat/test_std_selected_lavaan_mod_boot_2.R")

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

  boot_out_gp <- do_boot(fit_gp,
                        R = 50,
                        seed = 234567,
                        parallel = FALSE,
                        progress = FALSE)

  out_gp <- lav_betaselect(fit_gp,
                                standardized = TRUE,
                                not_to_standardize = c("dv", "med", "cov2"),
                                progress = FALSE,
                                std_se = "bootstrap",
                                boot_out = boot_out_gp)

  out_boot_gp <- lav_betaselect(fit_boot_gp,
                                    standardized = TRUE,
                                    not_to_standardize = c("dv", "med", "cov2"),
                                    progress = FALSE,
                                    std_se = "bootstrap")
  a <- round(out_gp[c(1:4, 10:13), "std.p.se"], 2)
  b <- round(out_boot_gp[c(1:4, 10:13), "se"], 2)
  i <- !is.na(a)
  expect_equal(a[i],
               b[i],
               ignore_attr = TRUE)
})
