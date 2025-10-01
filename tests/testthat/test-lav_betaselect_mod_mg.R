skip_on_cran()
# Parallel processing
# Long test

# testthat::test_file("./tests/testthat/test_std_selected_lavaan_mod_boot_1.R")

library(manymome)

#Load a test data of 500 cases
data(data_test_medmod)
library(lavaan)
dat <- data_test_medmod
dat$iv_mod <- dat$iv * dat$mod
head(dat)
dat$gp <- rep(c("gp2", "gp1"), size = nrow(dat) / 2)

dat1_c <- scale(dat[dat$gp == "gp2", -8],
               center = TRUE,
               scale = FALSE)
dat2_c <- scale(dat[dat$gp == "gp1", -8],
               center = TRUE,
               scale = FALSE)
dat1_c <- as.data.frame(dat1_c)
dat2_c <- as.data.frame(dat2_c)
dat1_c$gp <- "gp2"
dat2_c$gp <- "gp1"
dat_c <- rbind(dat1_c, dat2_c)
dat_c$iv_mod <- dat_c$iv * dat_c$mod
colMeans(dat_c[, -8])

apply(dat_c[, -8],
      MARGIN = 2,
      sd)

dat1_z <- scale(dat[dat$gp == "gp2", -8],
               center = TRUE,
               scale = TRUE)
dat2_z <- scale(dat[dat$gp == "gp1", -8],
               center = TRUE,
               scale = TRUE)
dat1_z <- as.data.frame(dat1_z)
dat2_z <- as.data.frame(dat2_z)
dat1_z$gp <- "gp2"
dat2_z$gp <- "gp1"
dat_z <- rbind(dat1_z, dat2_z)
dat_z$iv_mod <- dat_z$iv * dat_z$mod
colMeans(dat_z[, -8])
apply(dat_z[, -8],
      MARGIN = 2,
      sd)

mod <-
"
med ~ iv + mod + iv_mod + cov1
dv ~ med + cov2
"
fit <- sem(mod,
           data = dat,
           likelihood = "wishart",
           group = "gp")
fit_c <- sem(mod,
             data = dat_c,
             likelihood = "wishart",
             group = "gp")
fit_z <- sem(mod,
             data = dat_z,
             likelihood = "wishart",
             group = "gp")

std <- standardizedSolution(fit)
std <- setNames(std$est.std, lav_partable_labels(std))
std_c <- standardizedSolution(fit_c)
std_c <- setNames(std_c$est.std, lav_partable_labels(std_c))
std_z <- standardizedSolution(fit_z)
std_z <- setNames(std_z$est.std, lav_partable_labels(std_z))

test_that("Check variables are centered", {
  prods <- find_all_products(fit_c,
                             parallel = FALSE,
                             progress = FALSE)
  expect_true(all(!check_centered(fit,
                              prods = prods)))
  expect_true(all(check_centered(fit_c,
                             prods = prods)))
  expect_true(all(check_centered(fit_z,
                             prods = prods)))
})

test_that("Coefficient of the component terms", {

  chk_names <- c("med~mod", "med~iv", "med~iv_mod",
                 "med~mod.g2", "med~iv.g2", "med~iv_mod.g2")

  coef(fit)[chk_names]
  coef(fit_c)[chk_names]
  coef(fit_z)[chk_names]
  std_z[chk_names]

  expect_error(out <- lav_betaselect(fit,
                                     standardized = TRUE,
                                     not_to_standardize = c("dv", "cov2"),
                                     progress = FALSE,
                                     check_mean_centering = TRUE))
  expect_no_error(out <- lav_betaselect(fit,
                                     standardized = TRUE,
                                     not_to_standardize = c("dv", "cov2"),
                                     progress = FALSE,
                                     check_mean_centering = FALSE))

  out_c <- lav_betaselect(fit_c,
                          standardized = TRUE,
                          not_to_standardize = c("dv", "cov2"),
                          progress = FALSE)


  # Should be equal
  expect_equal(coef(out)[chk_names],
               coef(out_c)[chk_names])

})
