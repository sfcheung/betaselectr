# Summary of an 'lm_betaselect'-Class Object

The `summary` method for `lm_betaselect`-class objects.

## Usage

``` r
# S3 method for class 'lm_betaselect'
summary(
  object,
  correlation = FALSE,
  symbolic.cor = FALSE,
  se_method = c("boot", "bootstrap", "t", "lm", "ls"),
  ci = TRUE,
  level = 0.95,
  boot_type = c("perc", "bc"),
  boot_pvalue_type = c("asymmetric", "norm"),
  type = c("beta", "standardized", "raw", "unstandardized"),
  print_raw = c("none", "before_ci", "after_ci"),
  ...
)

# S3 method for class 'summary.lm_betaselect'
print(
  x,
  est_digits = 3,
  symbolic.cor = x$symbolic.cor,
  signif.stars = getOption("show.signif.stars"),
  tz_digits = 3,
  pvalue_less_than = 0.001,
  ...
)
```

## Arguments

- object:

  The output of
  [`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md).

- correlation:

  If `TRUE`, the correlation matrix of the estimates will be returned.
  The same argument in
  [`stats::summary.lm()`](https://rdrr.io/r/stats/summary.lm.html).
  Default is `FALSE`.

- symbolic.cor:

  If `TRUE`, correlations are printed in symbolic form as in
  [`stats::summary.lm()`](https://rdrr.io/r/stats/summary.lm.html).
  Default is `FALSE`.

- se_method:

  The method used to compute the standard errors and confidence
  intervals (if requested). If bootstrapping was requested when calling
  [`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  and this argument is set to `"bootstrap"` or `"boot"`, the bootstrap
  standard errors are returned. If bootstrapping was not requested or if
  this argument is set to `"t"`, `"lm"`, or `"ls"`, then the usual `lm`
  standard errors are returned. Default is `"boot"`.

- ci:

  Logical. Whether confidence intervals are computed. Default is `TRUE`.

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

- ...:

  Additional arguments passed to other methods.

- x:

  The output of `summary.lm_betaselect()`.

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
  This value also determines the number of digits for displayed
  R-squared.

- signif.stars:

  Whether "stars" (asterisks) are printed to denote the level of
  significance achieved for each coefficient. Default is `TRUE`.

- tz_digits:

  The number of digits after the decimal to be displayed for the *t* or
  similar statistic (in the column `"t value"` or `"z value"`). This
  value also determines the number of digits for the *F* statistic for
  the R-squared.

- pvalue_less_than:

  If a *p*-value is less than this value, it will be displayed with
  `"<(this value)".` For example, if `pvalue_less_than` is .001, the
  default, *p*-values less than .001 will be displayed as `<.001`. This
  value also determines the printout of the *p*-value of the *F*
  statistic. (This argument does what `eps.Pvalue` does in
  [`stats::printCoefmat()`](https://rdrr.io/r/stats/printCoefmat.html).)

## Value

It returns an object of class `summary.lm_betaselect`, which is similar
to the output of
[`stats::summary.lm()`](https://rdrr.io/r/stats/summary.lm.html), with
additional information on the standardization and bootstrapping, if
requested.

The `print`-method of `summary.lm_betaselect` is called for its side
effect. The object `x` is returned invisibly.

## Details

By default, it returns a `summary.lm_betaselect`-class object for the
results with selected variables standardized. By setting `type` to
`"raw"` or `"unstandardized"`, it return the summary for the results
*before* standardization.

The `print` method of `summary.lm_betaselect`-class objects is adapted
from `stdmod::print.summary.std_selected()`.

## References

Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
Retrieved from
https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf

## See also

[`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

data(data_test_mod_cat)

# bootstrap should be set to 2000 or 5000 in real studies
lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
                           data = data_test_mod_cat,
                           to_standardize = "iv",
                           do_boot = TRUE,
                           bootstrap = 100,
                           iseed = 1234)

summary(lm_beta_x)
#> Call to lm_betaselect():
#> betaselectr::lm_betaselect(formula = dv ~ iv * mod + cov1 + cat1, 
#>     data = data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, 
#>     bootstrap = 100, iseed = 1234)
#> 
#> Variable(s) standardized: iv 
#> 
#> Call:
#> stats::lm(formula = dv ~ iv * mod + cov1 + cat1, data = betaselectr::std_data(data = data_test_mod_cat, 
#>     to_standardize = "iv"))
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1987.03  -463.99     0.25   455.14  2152.48 
#> 
#> Coefficients:
#>              Estimate  CI.Lower  CI.Upper Std. Error z value Pr(Boot)    
#> (Intercept)   790.550  -430.112  1905.572    584.405   1.353     0.20    
#> iv            -94.302 -1569.880   981.851    625.286  -0.151     0.80    
#> mod            57.578    45.592    70.397      6.031   9.547   <0.001 ***
#> cov1           10.024    -5.933    35.460      9.457   1.060     0.18    
#> cat1gp2      -112.588  -263.297    32.218     74.809  -1.505     0.10    
#> cat1gp3       -53.106  -209.176    88.391     73.524  -0.722     0.56    
#> iv:mod          8.661    -1.968    22.601      6.203   1.396     0.14    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 681.1 on 493 degrees of freedom
#> 
#> R-squared                : 0.602
#> Adjusted R-squared       : 0.597
#> ANOVA test of R-squared : F(6, 493) = 124.344, p < 0.001
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
summary(lm_beta_x, ci = TRUE)
#> Call to lm_betaselect():
#> betaselectr::lm_betaselect(formula = dv ~ iv * mod + cov1 + cat1, 
#>     data = data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, 
#>     bootstrap = 100, iseed = 1234)
#> 
#> Variable(s) standardized: iv 
#> 
#> Call:
#> stats::lm(formula = dv ~ iv * mod + cov1 + cat1, data = betaselectr::std_data(data = data_test_mod_cat, 
#>     to_standardize = "iv"))
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1987.03  -463.99     0.25   455.14  2152.48 
#> 
#> Coefficients:
#>              Estimate  CI.Lower  CI.Upper Std. Error z value Pr(Boot)    
#> (Intercept)   790.550  -430.112  1905.572    584.405   1.353     0.20    
#> iv            -94.302 -1569.880   981.851    625.286  -0.151     0.80    
#> mod            57.578    45.592    70.397      6.031   9.547   <0.001 ***
#> cov1           10.024    -5.933    35.460      9.457   1.060     0.18    
#> cat1gp2      -112.588  -263.297    32.218     74.809  -1.505     0.10    
#> cat1gp3       -53.106  -209.176    88.391     73.524  -0.722     0.56    
#> iv:mod          8.661    -1.968    22.601      6.203   1.396     0.14    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 681.1 on 493 degrees of freedom
#> 
#> R-squared                : 0.602
#> Adjusted R-squared       : 0.597
#> ANOVA test of R-squared : F(6, 493) = 124.344, p < 0.001
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
summary(lm_beta_x, boot_pvalue_type = "norm")
#> Call to lm_betaselect():
#> betaselectr::lm_betaselect(formula = dv ~ iv * mod + cov1 + cat1, 
#>     data = data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, 
#>     bootstrap = 100, iseed = 1234)
#> 
#> Variable(s) standardized: iv 
#> 
#> Call:
#> stats::lm(formula = dv ~ iv * mod + cov1 + cat1, data = betaselectr::std_data(data = data_test_mod_cat, 
#>     to_standardize = "iv"))
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1987.03  -463.99     0.25   455.14  2152.48 
#> 
#> Coefficients:
#>              Estimate  CI.Lower  CI.Upper Std. Error z value Pr(>|z|)    
#> (Intercept)   790.550  -430.112  1905.572    584.405   1.353    0.176    
#> iv            -94.302 -1569.880   981.851    625.286  -0.151    0.880    
#> mod            57.578    45.592    70.397      6.031   9.547   <0.001 ***
#> cov1           10.024    -5.933    35.460      9.457   1.060    0.289    
#> cat1gp2      -112.588  -263.297    32.218     74.809  -1.505    0.132    
#> cat1gp3       -53.106  -209.176    88.391     73.524  -0.722    0.470    
#> iv:mod          8.661    -1.968    22.601      6.203   1.396    0.163    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 681.1 on 493 degrees of freedom
#> 
#> R-squared                : 0.602
#> Adjusted R-squared       : 0.597
#> ANOVA test of R-squared : F(6, 493) = 124.344, p < 0.001
#> 
#> Note:
#> - Results *after* standardization are reported.
#> - Nonparametric bootstrapping conducted.
#> - The number of bootstrap samples is 100.
#> - Standard errors are bootstrap standard errors.
#> - Z values are computed by 'Estimate / Std. Error'.
#> - The bootstrap p-values are based on standard normal distribution
#>   using z values.
#> - Percentile bootstrap 95.0% confidence interval reported.
summary(lm_beta_x, type = "raw")
#> Call to lm_betaselect():
#> betaselectr::lm_betaselect(formula = dv ~ iv * mod + cov1 + cat1, 
#>     data = data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, 
#>     bootstrap = 100, iseed = 1234)
#> 
#> Variable(s) standardized: iv 
#> 
#> Call:
#> stats::lm(formula = dv ~ iv * mod + cov1 + cat1, data = data_test_mod_cat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1987.03  -463.99     0.25   455.14  2152.48 
#> 
#> Coefficients:
#>              Estimate  CI.Lower  CI.Upper Std. Error z value Pr(Boot)    
#> (Intercept)  1488.568 -6589.967 12784.458    584.405   2.547     0.20    
#> iv            -46.545  -819.064   480.699    625.286  -0.074     0.80    
#> mod            -6.530  -117.793    72.268      6.031  -1.083   <0.001 ***
#> cov1           10.024    -5.933    35.460      9.457   1.060     0.18    
#> cat1gp2      -112.588  -263.297    32.218     74.809  -1.505     0.10    
#> cat1gp3       -53.106  -209.176    88.391     73.524  -0.722     0.56    
#> iv:mod          4.275    -0.963    11.792      6.203   0.689     0.14    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 681.1 on 493 degrees of freedom
#> 
#> R-squared                : 0.602
#> Adjusted R-squared       : 0.597
#> ANOVA test of R-squared : F(6, 493) = 124.344, p < 0.001
#> 
#> Note:
#> - Results *before* standardization are reported.
#> - Nonparametric bootstrapping conducted.
#> - The number of bootstrap samples is 100.
#> - Standard errors are bootstrap standard errors.
#> - Z values are computed by 'Estimate / Std. Error'.
#> - The bootstrap p-values are asymmetric p-values by Asparouhov and
#>   Muthén (2021).
#> - Percentile bootstrap 95.0% confidence interval reported.
```
