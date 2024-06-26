# Adapted from stdmod

set.seed(5678)
n <- 500
x <- rnorm(n, 15, 2)
w <- rnorm(n, 100, 5)
vs <- cbind(v1 = rnorm(n), v2 = rnorm(n))
vs <- vs %*% diag(c(3, 5)) + matrix(c(10, 50), n, 2, byrow = TRUE)
v1 <- vs[, 1]
cat1 <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
colnames(vs) <- paste0("v", seq_len(ncol(vs)))
y <- 10 + 3 * x + 5 * w + 4 * x * w + 2 * v1 +
      sapply(cat1,
             switch,
             gp1 = 1,
             gp2 = 4,
             gp3 = 2) +
      rnorm(n, 0, 700)
dat <- data.frame(dv = y,
                  iv = x,
                  mod = w,
                  cov1 = v1,
                  cat1,
                  stringsAsFactors = FALSE)
out <- lm(dv ~ iv * mod + cov1 + cat1,
          dat)
summary(lm.beta::lm.beta(out))
data_test_mod_cat <- dat
usethis::use_data(data_test_mod_cat, overwrite = TRUE)
