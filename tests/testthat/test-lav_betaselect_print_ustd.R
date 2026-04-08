library(manymome)

test_that("print.lav: Ustd", {

#Load a test data of 500 cases
data(data_test_medmod)
library(lavaan)
dat <- data_test_medmod
dat$iv_mod <- dat$iv * dat$mod
head(dat)

mod <-
"
med ~ iv + mod + iv_mod + cov1
dv ~ med + cov2
"
fit <- sem(mod,
           data = dat)

out <- lav_betaselect(
          fit,
          standardized = TRUE,
          not_to_standardize = c("dv", "cov2"),
          progress = !is_testing())

out

tmp <- capture.output(print(out, show_Bs.by = TRUE, show_ustd = TRUE))
tmp2 <- grep("U$", tmp, value = TRUE)
i <- regexpr(" Selected ", tmp)
i2 <- max(i)
i3 <- max(attr(i, "match.length"))
tmp3 <- substr(tmp2, i2, i2 + i3)
tmp3 <- trimws(tmp3)
expect_all_true(sapply(tmp3, nchar) == 0)

tmp <- capture.output(print(out, show_Bs.by = TRUE))
expect_all_false(grepl("UStd", tmp))

})

