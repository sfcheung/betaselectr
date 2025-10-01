skip_on_cran()

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
           likelihood = "wishart",
           meanstructure = TRUE)
fit_c <- sem(mod,
             data = dat_c,
             likelihood = "wishart",
             meanstructure = TRUE)
fit_z <- sem(mod,
             data = dat_z,
             likelihood = "wishart",
             meanstructure = TRUE)

std <- standardizedSolution(fit)
std <- setNames(std$est.std, lav_partable_labels(std))
std_c <- standardizedSolution(fit_c)
std_c <- setNames(std_c$est.std, lav_partable_labels(std_c))
std_z <- standardizedSolution(fit_z)
std_z <- setNames(std_z$est.std, lav_partable_labels(std_z))

test_that("Intercepts", {

  chk_names <- c("med~1", "dv~1")

  coef(fit)[chk_names]
  coef(fit_c)[chk_names]
  coef(fit_z)[chk_names]
  std_z[chk_names]

  out_c1 <- lav_betaselect(
                fit,
                standardized = TRUE,
                not_to_standardize = c("dv", "cov2"),
                progress = FALSE,
                check_mean_centering = FALSE,
                std_intercept = TRUE)
  coef(out_c1)[chk_names]
  std[chk_names]
  out_c2 <- lav_betaselect(
                fit,
                standardized = TRUE,
                to_standardize = c("dv", "med", "cov2"),
                progress = FALSE,
                check_mean_centering = FALSE,
                std_intercept = TRUE)


  # Should be equal
  expect_equal(coef(out_c1)["med~1"],
               std["med~1"])
  expect_equal(coef(out_c2)[chk_names],
               std[chk_names])

})
