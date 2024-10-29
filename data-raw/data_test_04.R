# Adapted from stdmod

set.seed(5415431)
n <- 300
x <- rnorm(n)
w <- .4 * x + sqrt(1 - .4^2) * rnorm(n)
v1 <- rnorm(n)
cat1 <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE, prob = c(.2, .3, .5))
y <- .2 * x + .3 * w + .35 * x * w + .12 * v1 +
      sapply(cat1,
             switch,
             gp1 = 0,
             gp2 = .4,
             gp3 = .6) +
      rnorm(n, 0, 1.4)
dat <- data.frame(dv = y,
                  iv = x,
                  mod = w,
                  cov1 = v1,
                  cat1,
                  stringsAsFactors = FALSE)
out <- lm(dv ~ iv * mod + cov1 + cat1,
          dat)
summary(out)
dat$dv <- ifelse(dat$dv > mean(dat$dv), 1, 0)
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
dat1 <- data.frame(dat0, cat1 = dat$cat1)
dat1$dv <- dat$dv
head(dat1)
out <- glm(dv ~ iv * mod + cov1 + cat1,
           dat1,
           family = binomial())
summary(out)
dat1z <- dat1
dat1z$cat1gp2 <- ifelse(dat1z$cat1 == "gp2", 1, 0)
dat1z$cat1gp3 <- ifelse(dat1z$cat1 == "gp3", 1, 0)
dat1z$cat1 <- NULL
dat1z$iv_x_mod <- dat1z$iv * dat1z$mod
dat1z <- as.data.frame(scale(dat1z))
dat1z$dv <- dat$dv
head(dat1z)
outz <- glm(dv ~ iv + mod + cov1 + cat1gp2 + cat1gp3 + iv_x_mod,
            dat1z,
            family = binomial())
summary(outz)
summary(out)
data_test_mod_cat_binary <- dat1
usethis::use_data(data_test_mod_cat_binary, overwrite = TRUE)
