# Summary of an 'glm_betaselect'-Class Object

The `summary` method for `glm_betaselect`-class objects.

## Usage

``` r
# S3 method for class 'glm_betaselect'
summary(
  object,
  dispersion = NULL,
  correlation = FALSE,
  symbolic.cor = FALSE,
  trace = FALSE,
  test = c("LRT", "Rao"),
  se_method = c("boot", "bootstrap", "z", "glm", "default"),
  ci = TRUE,
  level = 0.95,
  boot_type = c("perc", "bc"),
  boot_pvalue_type = c("asymmetric", "norm"),
  type = c("beta", "standardized", "raw", "unstandardized"),
  print_raw = c("none", "before_ci", "after_ci"),
  transform_b = NULL,
  transform_b_name = NULL,
  ...
)

# S3 method for class 'summary.glm_betaselect'
print(
  x,
  est_digits = 3,
  symbolic.cor = x$symbolic.cor,
  signif.stars = getOption("show.signif.stars"),
  show.residuals = FALSE,
  z_digits = 3,
  pvalue_less_than = 0.001,
  ...
)
```

## Arguments

- object:

  The output of
  [`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md).

- dispersion:

  The dispersion parameter. If `NULL`, then it is extracted from the
  object. If a scalar, it will be used as the dispersion parameter. See
  [`stats::summary.glm()`](https://rdrr.io/r/stats/summary.glm.html) for
  details.

- correlation:

  If `TRUE`, the correlation matrix of the estimates will be returned.
  The same argument in
  [`stats::summary.glm()`](https://rdrr.io/r/stats/summary.glm.html).
  Default is `FALSE`.

- symbolic.cor:

  If `TRUE`, correlations are printed in symbolic form as in
  [`stats::summary.glm()`](https://rdrr.io/r/stats/summary.glm.html).
  Default is `FALSE`.

- trace:

  Logical. Whether profiling will be traced when forming the confidence
  interval if `se_method` is `"default"`, `"z"`, or `"glm"`. Ignored if
  `ci` is `FALSE`. See
  [`stats::confint.glm()`](https://rdrr.io/r/stats/confint.html) for
  details.

- test:

  The test used for `se_method` is `"default"`, `"z"`, or `"glm"`.
  Ignored if `ci` is `FALSE`. See
  [`stats::confint.glm()`](https://rdrr.io/r/stats/confint.html) for
  details.

- se_method:

  The method used to compute the standard errors and confidence
  intervals (if requested). If bootstrapping was requested when calling
  [`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  and this argument is set to `"bootstrap"` or `"boot"`, the bootstrap
  standard errors are returned. If bootstrapping was not requested or if
  this argument is set to `"z"`, `"glm"`, or `"default"`, then the usual
  `glm` standard errors are returned. Default is `"boot"`.

- ci:

  Logical. Whether confidence intervals are computed. Default is
  `FALSE.`

- level:

  The level of confidence, default is .95, returning the 95% confidence
  interval.

- boot_type:

  The type of bootstrap confidence intervals, if requested. Currently,
  it supports `"perc"`, percentile bootstrap confidence intervals, and
  `"bc"`, bias-corrected bootstrap confidence interval.

- boot_pvalue_type:

  The type of *p*-values if `se_method` is `"boot"` or `"bootstrap"`. If
  `"norm"`, then the *z* score is used to compute the *p*-value using a
  standard normal distribution. If `"asymmetric"`, the default, then the
  method presented in Asparouhov and Muthén (2021) is used to compute
  the *p*-value based on the bootstrap distribution.

- type:

  String. If `"unstandardized"` or `"raw"`, the output *before*
  standardization are used If `"beta"` or `"standardized"`, then the
  output *after* selected variables standardized are returned. Default
  is `"beta"`.

- print_raw:

  Control whether the estimates before selected standardization are
  printed when `type` is `"beta"` or `"standardized"`. If `"none"`, the
  default, then it will not be printed. If set to `"before_ci"` and `ci`
  is `TRUE`, then will be inserted to the left of the confidence
  intervals. If set to
  "after_ci"`and`ci`is`TRUE`, then will be printed to the right of the confidence intervals. If `ci`is`FALSE\`,
  then will be printed to the right of the standardized estimates.

- transform_b:

  The function to be used to transform the confidence limits. For
  example, if set to `exp`, the confidence limits will be exponentiated.
  Users need to decide whether the transformed limits are meaningful.
  Default is `NULL`.

- transform_b_name:

  If `transform_b` is a function, then this is the name of the
  transformed coefficients. Default is `"Estimate(Transformed)"`

- ...:

  Additional arguments passed to other methods.

- x:

  The output of `summary.glm_betaselect()`.

- est_digits:

  The number of digits after the decimal to be displayed for the
  coefficient estimates, their standard errors, and confidence intervals
  (if present). Note that the values will be rounded to this number of
  digits before printing. If all digits at this position are zero for
  all values, the values may be displayed with fewer digits. Note that
  the coefficient table is printed by
  [`stats::printCoefmat()`](https://rdrr.io/r/stats/printCoefmat.html).
  If some numbers are vary large, the number of digits after the decimal
  may be smaller than `est_digits` due to a limit on the column width.

- signif.stars:

  Whether "stars" (asterisks) are printed to denote the level of
  significance achieved for each coefficient. Default is `TRUE`.

- show.residuals:

  If `TRUE`, a summary of the deviance residuals will be printed.
  Default is `FALSE`.

- z_digits:

  The number of digits after the decimal to be displayed for the *z* or
  similar statistic (in the column `"z value"`).

- pvalue_less_than:

  If a *p*-value is less than this value, it will be displayed with
  `"<(this value)".` For example, if `pvalue_less_than` is .001, the
  default, *p*-values less than .001 will be displayed as `<.001`. This
  value also determines the printout of the *p*-value of the *F*
  statistic. (This argument does what `eps.Pvalue` does in
  [`stats::printCoefmat()`](https://rdrr.io/r/stats/printCoefmat.html).)

## Value

It returns an object of class `summary.glm_betaselect`, which is similar
to the output of
[`stats::summary.glm()`](https://rdrr.io/r/stats/summary.glm.html), with
additional information on the standardization and bootstrapping, if
requested.

The `print`-method of `summary.glm_betaselect` is called for its side
effect. The object `x` is returned invisibly.

## Details

By default, it returns a `summary.glm_betaselect`-class object for the
results with selected variables standardized. By setting `type` to
`"raw"` or `"unstandardized"`, it returns the summary for the results
*before* standardization.

The `print` method of `summary.glm_betaselect`-class objects is adapted
from `stdmod::print.summary.std_selected()`.

## References

Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
Retrieved from
https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf

## See also

[`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
                              yes = 1,
                              no = 0)
# bootstrap should be set to 2000 or 5000 in real studies
logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
                                  data = data_test_mod_cat,
                                  family = binomial,
                                  to_standardize = "iv",
                                  do_boot = TRUE,
                                  bootstrap = 100,
                                  iseed = 1234)
summary(logistic_beta_x)
#> Call to glm_betaselect():
#> betaselectr::lm_betaselect(formula = p ~ iv * mod + cov1 + cat1, 
#>     family = binomial, data = data_test_mod_cat, to_standardize = "iv", 
#>     do_boot = TRUE, bootstrap = 100, iseed = 1234, model_call = "glm")
#> 
#> Variable(s) standardized: iv 
#> 
#> Call:
#> stats::glm(formula = p ~ iv * mod + cov1 + cat1, family = binomial, 
#>     data = betaselectr::std_data(data = data_test_mod_cat, to_standardize = "iv"))
#> 
#> Coefficients:
#>             Estimate CI.Lower CI.Upper Std. Error z value Pr(Boot)    
#> (Intercept)  -16.205  -23.001  -10.040      2.966  -5.464   <0.001 ***
#> iv            -1.148  -10.721    4.958      4.066  -0.282     0.74    
#> mod            0.165    0.105    0.229      0.030   5.557   <0.001 ***
#> cov1          -0.004   -0.077    0.098      0.042  -0.103     0.78    
#> cat1gp2       -0.211   -0.706    0.294      0.290  -0.725     0.50    
#> cat1gp3       -0.189   -0.772    0.297      0.271  -0.697     0.54    
#> iv:mod         0.031   -0.028    0.129      0.041   0.755     0.38    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 692.86  on 499  degrees of freedom
#> Residual deviance: 440.10  on 493  degrees of freedom
#> AIC: 454.1
#> 
#> Number of Fisher Scoring iterations: 5
#> 
#> Transformed Parameter Estimates:
#>             Exp(B) CI.Lower CI.Upper
#> (Intercept)  0.000    0.000    0.000
#> iv           0.317    0.000  143.965
#> mod          1.180    1.110    1.257
#> cov1         0.996    0.926    1.103
#> cat1gp2      0.810    0.494    1.341
#> cat1gp3      0.828    0.463    1.346
#> iv:mod       1.031    0.973    1.137
#> 
#> Note:
#> - Results *after* standardization are reported.
#> - Nonparametric bootstrapping conducted.
#> - The number of bootstrap samples is 100.
#> - Standard errors are bootstrap standard errors.
#> - Z values are computed by 'Estimate / Std. Error'.
#> - The bootstrap p-values are asymmetric p-values by Asparouhov and
#>   Muthén (2021).
#> - Percentile bootstrap 95.0% confidence interval reported.
```
