# How lav_betaselect() Works

## Goal

This technical appendix describes how a \\\beta\_{Select}\\ is computed
in
[`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md)
from the package [betaselectr](https://sfcheung.github.io/betaselectr/)
(Sun et al., 2026).

## Beta-Select (\\\beta\_{Select}\\)

Unlike multiple linear regression and generalized linear model,
refitting a structural equation model with some variables standardized
is not a suitable approach because (a) the standardization may not be
consistent with the model being fitted (the sample standard deviation
and the model implied standard deviation may be different) (b) doing so
does not allow the delta method to take into account the sampling error
in the standard deviations. Therefore, \\\beta{s}\_{Select}\\ in
structural equation model are computed as function of model parameters.

### A Predictor Not Involved In a Product Term

Let’s consider a predictor not involved in a product term.

This is an example, with the effect of \\x_1\\ not moderated and \\x_1\\
also not a moderator:

\\ y = B_0 + B_1x_1 + B_2x_2 + B_3w + B_4x_2w + e \\

The general form of the standardized coefficient of \\x_1\\ is:

\\ \beta\_{Select} = B_1\frac{SD\_{x_1}}{SD_y} \\

where \\SD\_{x_1}\\ and \\SD_y\\ are the standard deviations of \\x_1\\
and \\y\\, respectively.

If only \\x_1\\ is standardized, then

\\ \beta\_{Select} = B_1{SD\_{x_1}} \\

If only \\y\\ is standardized, then

\\ \beta\_{Select} = B_1\frac{1}{SD_y} \\

The function
[`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md)
will compute the \\\beta\_{Select}\\ of a predictor based on the
variables being standardized.

### A Product Term

\\B_4x_2w\\ in the following model is a product term (interaction term),
representint the moderation effect of \\w\\ on the effect of \\x_2\\ on
\\y\\:

\\ y = B_0 + B_1x_1 + B_2x_2 + B_3w + B_4x_2w + e \\

The general form of the standardized coefficient of the product term
\\B_4x_2w\\ is:

\\ \beta\_{Select} = B_4\frac{SD\_{x_2}SD\_{w}}{SD_y} \\

where \\SD\_{x_2}\\, \\SD\_{w}\\, and \\SD_y\\ are the standard
deviations of \\x_2\\, \\w\\, and \\y\\, respectively.

If only \\x_2\\ is standardized, then

\\ \beta\_{Select} = B_4SD\_{x_2} \\

If only \\y\\ is standardized, then

\\ \beta\_{Select} = B_4\frac{1}{SD_y} \\

If only \\w\\ is standardized, then

\\ \beta\_{Select} = B_4SD\_{w} \\

If only \\x_2\\ and \\w\\ are standardized, then

\\ \beta\_{Select} = B_4SD\_{x_2}SD\_{w} \\

If only \\y\\ and \\w\\ are standardized, then

\\ \beta\_{Select} = B_4\frac{SD_w}{SD_y} \\

If only \\y\\ and \\x_2\\ are standardized, then

\\ \beta\_{Select} = B_4\frac{SD\_{x_2}}{SD_y} \\

The function
[`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md)
will compute the \\\beta\_{Select}\\ of a predictor based on the
variables being standardized.

### A Predictor Involved In a Product Term

Let’s consider a predictor involved in a product term.

In the following model, the effect of \\x_2\\ is moderated by \\w\\, and
the conditional effect of \\x_2\\ when \\w = 0\\ is given by \\B_2\\:

\\ y = B_0 + B_1x_1 + B_2x_2 + B_3w + B_4x_2w + e \\

The general form of the standardized coefficient of \\x_2\\ is:

\\ \beta\_{Select} = (B_2 + B_4M_w)\frac{SD\_{x_2}}{SD_y} \\

where \\SD\_{x_2}\\ and \\SD_y\\ are the standard deviations of \\x_2\\
and \\y\\, respectively, and \\M_w\\ is the mean of \\w\\.

If only \\x_2\\ is standardized, then

\\ \beta\_{Select} = (B_2 + B_4M_w)SD\_{x_2} \\

If only \\y\\ is standardized, then

\\ \beta\_{Select} = (B_2 + B_4M_w)\frac{1}{SD_y} \\

The function
[`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md)
will compute the \\\beta\_{Select}\\ based on the variables being
standardized.

## Standard Error, \\p\\-Values, and Confidence Interval

### The Delta Method

If the delta method (Rao, 1973) is used to compute the standard error of
a \\\beta\_{Select}\\, the \\\beta\_{Select}\\ will be treated as a
function of the model parameters, and the point estimates and the
variance-covariance matrix of these estimates returned by
[`lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) will be used to
derive the asymptotical standard error of the \\\beta\_{Select}\\. The
\\p\\-value and the confidence interval will then be computed using the
standard normal distribution.

### Nonparametric Bootstrapping

If nonparametric bootstrapping (Efron & Tibshirani, 1993) is used to
compute the standard error of a \\\beta\_{Select}\\, then \\R\\
bootstrap samples will be drawn, and the \\\beta\_{Select}\\ will be
computed in each sample using one of the formulas above. The standard
error is the standard deviation of the \\R\\ bootstrap estimates of the
\\\beta\_{Select}\\. The \\p\\-value is computed using the method
proposed by Asparouhov & Muthén (2021). The confidence interval can be
formed by either the percentile method (the default) or the
bias-corrected method.

## Miscellaneous

For a structural equation model, the model implied standard deviations,
instead of the sample standard deviations, are used. This ensures that
the standardization conducted is consistent with the model being fitted.

For example, if a multigroup model is fitted and equality constraints
are imposed on the standard deviations, then the model implied common
standard deviation, which can be different from the full sample standard
deviation, will be used in the standardization.

Moreover, if missing data is present and method such as maximum
likelihood (ML) is used, then this method also allows using the ML
estimates of the standard deviations, instead of listwise or pairwise
estimates of them.

## References

Asparouhov, T., & Muthén, B. O. (2021). *Bootstrap *p*-value
computation*.
<https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf>

Efron, B., & Tibshirani, R. (1993). *An Introduction to the bootstrap*.
Chapman & Hall/CRC.

Rao, C. R. (1973). *Large sample theory and methods*. John Wiley & Sons,
Inc.

Sun, R. wei, Chang, F., Yang, W., Cheung, S. F., & Cheung, S.-H. (2026).
Betaselectr: Selective (and proper) standardization in structural
equation models. *Multivariate Behavioral Research*.
<https://doi.org/10.1080/00273171.2026.2672692>
