library(testthat)

test_that("use glm as the input", {

dat <- data_test_mod_cat
dat$dv <- ifelse(dat$dv > mean(dat$dv),
                 yes = 1,
                 no = 0)

lm_beta_x <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "iv", do_boot = FALSE, family = binomial)
lm_beta_x_0 <- glm(dv ~ iv*mod + cov1 + cat1, dat, family = binomial)
lm_beta_x_1 <- glm_betaselect(lm_beta_x_0, to_standardize = "iv", do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

lm_beta_x <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "iv", do_boot = FALSE, family = binomial)
lm_beta_x_0 <- glm(dv ~ iv*mod + cov1 + cat1, dat, family = binomial)
lm_beta_x_1 <- glm_betaselect(lm_beta_x_0, to_standardize = "iv", do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

lm_beta_x <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = "mod", do_boot = FALSE, family = binomial)
lm_beta_x_0 <- glm(dv ~ iv*mod + cov1 + cat1, dat, family = binomial)
lm_beta_x_1 <- glm_betaselect(lm_beta_x_0, to_standardize = "mod", do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

lm_beta_x <- glm_betaselect(dv ~ iv*mod + cov1 + cat1, dat, to_standardize = c("mod", "iv"), do_boot = FALSE, family = binomial)
lm_beta_x_0 <- glm(dv ~ iv*mod + cov1 + cat1, dat, family = binomial)
lm_beta_x_1 <- glm_betaselect(lm_beta_x_0, to_standardize = c("mod", "iv"), do_boot = FALSE)
expect_equal(
  coef(lm_beta_x),
  coef(lm_beta_x_1)
)
expect_equal(
  getCall(lm_beta_x),
  getCall(lm_beta_x_1)
)

})

