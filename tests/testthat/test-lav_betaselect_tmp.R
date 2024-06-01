skip("WIP")

#Load a test data of 500 cases
data(test_modmed)
library(lavaan)
mod <-
"
med ~ iv + mod + iv:mod + cov1
dv ~ med + cov2
"
fit <- sem(mod, data = test_modmed)

# Standardize all variables

out <- lav_betaselect(fit, standardized = TRUE, std_se = "delta")
out
std <- standardizedSolution(fit)

out3 <- lav_betaselect(fit, standardized = TRUE, output = "text")
out3

# Difference
i <- which(!sapply(std$est.std / out$std.p, function(x) isTRUE(all.equal(x, 1))))
print(merge(out[i, c("lhs", "op", "rhs", "std.p")],
            std[i, c("lhs", "op", "rhs", "est.std")],
            sort = FALSE),
      digits = 3)

# Standardize some variables

out2 <- lav_betaselect(fit, standardized = TRUE, not_to_standardize = c("cov2", "dv"))
out2
i2 <- which(!sapply(std$est.std / out2$std.p, function(x) isTRUE(all.equal(x, 1))))

# Difference
out2[i2, c("lhs", "op", "rhs", "std.p")]
std[i2, c("lhs", "op", "rhs", "est.std")]

# Compare with results with variables standardized first
# Differences are expected because model implied SDs are used

n <- nrow(test_modmed)
test_modmed_cov <- cov(test_modmed)
test_modmedz <- as.data.frame(scale(test_modmed,
                                  scale = sqrt(diag(test_modmed_cov))))
fitz <- sem(mod, data = test_modmedz)
estz <- parameterEstimates(fitz)
estz[1:6, c("lhs", "op", "rhs", "est")]
out[1:6, c("lhs", "op", "rhs", "std.p")]
round(estz$est / out$std.p, 3)

observed_cov <- cov(test_modmed)
fit_implied_cov <- lavInspect(fit, "implied")$cov
(fit_implied_cov[colnames(observed_cov), colnames(observed_cov)] * n / (n - 1)) / observed_cov

fit_samptsta <- lavInspect(fit, "sampstat")$cov
(fit_samptsta[colnames(observed_cov), colnames(observed_cov)] * n / (n - 1) / observed_cov)

# Bootstrapping

fit_boot <- sem(mod, data = test_modmed,
                se = "boot",
                bootstrap = 100,
                iseed = 1234)
out_boot <- lav_betaselect(fit_boot, standardized = TRUE,
                           std_se = "boot",
                           std_ci = TRUE,
                           progress = FALSE)
