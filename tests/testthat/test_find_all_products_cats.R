library(testthat)
library(lavaan)
library(manymome)
library(numDeriv)

# From manymome

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
dat <- cbind(dat, factor2var(dat$city, prefix = "city", add_rownames = FALSE))

mod <-
"
m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
y ~ m2 + m3 + x + w4 + x:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
fit2 <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, group = "city")


prods <- find_all_products(fit)
prods2 <- find_all_products(fit2)

test_that("Find prods", {
  expect_true(setequal(names(prods),
                       c("x:gpgp2", "x:gpgp3", "x:w4")))
  expect_true(setequal(names(prods2),
                       c("x:gpgp2", "x:gpgp3", "x:w4")))
})

test_that("Find cat", {
  expect_true(setequal(find_categorical(fit),
                       c("gpgp3", "gpgp2")))
  expect_true(setequal(find_categorical(fit2),
                       c("gpgp3", "gpgp2")))
})
