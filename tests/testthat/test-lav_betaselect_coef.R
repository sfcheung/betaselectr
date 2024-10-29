library(testthat)
library(lavaan)
library(manymome)

dat <- HolzingerSwineford1939
dat$age_gp <- dat$ageyr
dat <- dat[(dat$age_gp >= 12) & (dat$age_gp <= 15), ]
table(dat$age_gp)
tmp <- factor2var(dat$age_gp)
dat[, c("age13", "age14", "age15")] <- tmp
head(dat)
dat$age13b <- dat$age13

mod <-
"
f1 =~ x1 + x2 + x3
f1 ~ age13 + age14 + age15
"

fit <- sem(mod,
           dat,
           fixed.x = FALSE,
           group = "school")

test_that("coef: Multigroup", {
  out <- lav_betaselect(fit,
                        standardized = TRUE,
                        skip_categorical_x = FALSE,
                        progress = FALSE)
  tmp <- coef(out)
  expect_equal(out$std.p,
               tmp,
               ignore_attr = TRUE)
  tmp <- coef(out, drop_na = TRUE)
  expect_equal(out$std.p[!is.na(out$std.p)],
               tmp,
               ignore_attr = TRUE)
})

dat <- HolzingerSwineford1939

mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + d5*x5 + d6*x6
f3 =~ x7 + d8*x8 + d9*x9
f2 ~ a*f1
f3 ~ b*f2
ab := a*b
d1 := d5 - d9
d2 := d8 - d6
dd := d1 * d2
"

fit <- cfa(mod,
           dat)

test_that("User parameters", {
  out <- lav_betaselect(fit,
                        standardized = TRUE,
                        progress = FALSE)
  tmp <- coef(out)
  expect_equal(out$std.p,
               tmp,
               ignore_attr = TRUE)
  tmp <- coef(out, drop_na = TRUE)
  expect_equal(out$std.p[!is.na(out$std.p)],
               tmp,
               ignore_attr = TRUE)
})
