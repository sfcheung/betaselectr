# Adapted from stdmod

set.seed(41543)
n <- 500
x <- rnorm(n)
w <- .4 * x + sqrt(1 - .4^2) * rnorm(n)
v1 <- rnorm(n)
cat1 <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE, prob = c(.2, .3, .5))
y <- .2 * x + .3 * w + .4 * x * w + .12 * v1 +
      sapply(cat1,
             switch,
             gp1 = 0,
             gp2 = .4,
             gp3 = .6) +
      rnorm(n)
dat <- data.frame(dv = y,
                  iv = x,
                  mod = w,
                  cov1 = v1,
                  cat1,
                  stringsAsFactors = FALSE)
out <- lm(dv ~ iv * mod + cov1 + cat1,
          dat)
summary(out)
apply(dat[, 1:4], 2, sd)
colMeans(dat[, 1:4])
b <- 1 / c(5, 2, 4, 2)
a <- c(20, 15, 50, 20) * -b
dat0 <- scale(dat[, 1:4],
              center = a,
              scale = b)
dat0 <- round(dat0, 2)
apply(dat0, 2, sd)
colMeans(dat0)
apply(dat0, 2, range)
dat1 <- data.frame(dat0, cat = dat$cat1)
head(dat1)
out <- lm(dv ~ iv * mod + cov1 + cat1,
          dat1)
summary(out)
summary(lm.beta::lm.beta(out))
data_test_mod_cat2 <- dat
usethis::use_data(data_test_mod_cat2, overwrite = TRUE)
