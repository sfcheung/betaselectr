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

dat_c <- scale(dat,
               center = TRUE,
               scale = FALSE)
dat_c <- as.data.frame(dat_c)
dat_c$iv_mod <- dat_c$iv * dat_c$mod
colMeans(dat_c)
apply(dat_c,
      MARGIN = 2,
      sd)

dat_z <- scale(dat,
               center = TRUE,
               scale = TRUE)
dat_z <- as.data.frame(dat_z)
dat_z$iv_mod <- dat_z$iv * dat_z$mod
colMeans(dat_z)
apply(dat_z,
      MARGIN = 2,
      sd)

mod <-
"
med ~ iv + mod + iv_mod + cov1
dv ~ med + cov2
"
fit <- sem(mod,
           data = dat,
           likelihood = "wishart")
fit_c <- sem(mod,
             data = dat_c,
             likelihood = "wishart")
fit_z <- sem(mod,
             data = dat_z,
             likelihood = "wishart")

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
  expect_false(check_centered(fit,
                              prods = prods))
  expect_true(check_centered(fit_c,
                             prods = prods))
  expect_true(check_centered(fit_z,
                             prods = prods))
})

test_that("Coefficient of the component terms", {

  chk_names <- c("med~mod", "med~iv", "med~iv_mod")

  coef(fit)[chk_names]
  coef(fit_c)[chk_names]
  coef(fit_z)[chk_names]
  std_z[chk_names]

  expect_error(out <- lav_betaselect(fit,
                                     standardized = TRUE,
                                     not_to_standardize = c("dv", "cov2"),
                                     progress = FALSE))
  out_c <- lav_betaselect(fit_c,
                          standardized = TRUE,
                          not_to_standardize = c("dv", "cov2"),
                          progress = FALSE)


  # Should be equal
  expect_equal(coef(out_c)[chk_names],
               coef(fit_z)[chk_names])

})
