# How lm_betaselect() and glm_betaselect() Work

## Goal

This technical appendix describes how a \\\beta\_{Select}\\ is computed
in
[`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
and
[`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
from the package [betaselectr](https://sfcheung.github.io/betaselectr/).

## Beta-Select (\\\beta\_{Select}\\)

Suppose this is the linear regression model:

\\ y = B_0 + B_1x_1 + B_2x_2 + B_3w + B_4x_2w + e \\

If only some of the variables are selected to be standardized, then only
the two functions will simply standardize the selected variables using
sample means and SDs, and refit the model.

For example, if only \\y\\ and \\x_2\\ are standardized, then both
[`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
and
[`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
will standardize \\y\\ and \\x_2\\, and then fit the model as usual. The
coefficients in the resulting model is then the \\\beta{s}\_{Select}\\
requested.

For a model to be fitted by [`glm()`](https://rdrr.io/r/stats/glm.html),
such as a logistic regression model, the outcome variable should not be
standardized.

## Standard Error, \\p\\-Values, and Confidence Interval

Although formulas for delta method standard errors (Pesigan et al.,
2023; Rao, 1973) for standardized coefficients in multiple regression
are available, they assumes that *all* variables are standardized. To
our knowledge, formulas are not yet available for coefficients with only
selected variables standardized, and for the coefficients of product
terms. Therefore, for now, only nonparametric bootstrapping is
supported.

### Nonparametric Bootstrapping

If nonparametric bootstrapping (Efron & Tibshirani, 1993) is used to
compute the standard error of a \\\beta\_{Select}\\, then \\R\\
bootstrap samples will be drawn, selected variables standardized, and
then the model is fitted using [`lm()`](https://rdrr.io/r/stats/lm.html)
or [`glm()`](https://rdrr.io/r/stats/glm.html). The standard error is
the standard deviation of the \\R\\ bootstrap estimates of the
regression model. The \\p\\-value is computed using the method proposed
by Asparouhov & Muthén (2021). The confidence interval can be formed by
either the percentile method (the default) or the bias-corrected method.

## Miscellaneous

If missing data is present, listwise deletion will be used, using only
the variables in the model, to determin the cases to be used for
computing the means and standard deviations for the standardization.

If all variables are to be standardized and no higher order terms such
as product terms are present, then existing methods, such as those
available in Pesigan et al. (2023), can also be used. The package
`betaselectr` is for cases in which only some of the variables are to be
standardized and/or the model has one or more product term.

## References

Asparouhov, T., & Muthén, B. O. (2021). *Bootstrap *p*-value
computation*.
<https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf>

Efron, B., & Tibshirani, R. (1993). *An Introduction to the bootstrap*.
Chapman & Hall/CRC.

Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023). betaDelta and
betaSandwich: Confidence intervals for standardized regression
coefficients in R. *Multivariate Behavioral Research*, *58*(6),
1183–1186. <https://doi.org/10.1080/00273171.2023.2201277>

Rao, C. R. (1973). *Large sample theory and methods*. John Wiley & Sons,
Inc.
