# Generate data

set.seed(2345)
n <- 200
iv <- rnorm(n)
cov1 <- rnorm(n)
mod <- .40 * iv + sqrt(1 - .40^2) * rnorm(n)
med <- .30 * iv + .20 * mod + .30 * iv * mod + .00 * cov1 + (.80) * rnorm(n)
cov2 <- rnorm(n)
dv <- .20 * iv + .30 * med + .05 * cov2 + (.50) * rnorm(n)
dat <- data.frame(dv = 3 * dv + 10,
                  iv = 3 * iv + 15,
                  mod = 5 * mod + 25,
                  med = 10 * med + 45,
                  cov1 = 8 * cov1 + 35,
                  cov2 = 4 * cov2 + 20)
psych::describe(dat)
data_test_medmod <- dat
usethis::use_data(data_test_medmod, overwrite = TRUE)
