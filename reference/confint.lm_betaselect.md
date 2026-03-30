# Confidence Interval for 'lm_betaselect' or 'glm_betaselect' Objects

Return the confidence interval of the regression coefficients in the
output of
[`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
or
[`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md).

## Usage

``` r
# S3 method for class 'lm_betaselect'
confint(
  object,
  parm,
  level = 0.95,
  method = c("boot", "bootstrap", "ls"),
  type = c("beta", "standardized", "raw", "unstandardized"),
  warn = TRUE,
  boot_type = c("perc", "bc"),
  ...
)

# S3 method for class 'glm_betaselect'
confint(
  object,
  parm,
  level = 0.95,
  trace = FALSE,
  test = c("LRT", "Rao"),
  method = c("boot", "bootstrap", "default", "ls"),
  type = c("beta", "standardized", "raw", "unstandardized"),
  warn = TRUE,
  boot_type = c("perc", "bc"),
  transform_b = NULL,
  ...
)
```

## Arguments

- object:

  The output of
  [`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  or
  [`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md).

- parm:

  The terms for which the confidence intervals are returned. If missing,
  the confidence intervals of all terms will be returned.

- level:

  The level of confidence, default is .95, returning the 95% confidence
  interval.

- method:

  The method used to compute the confidence intervals/ If bootstrapping
  was requested when calling
  [`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  and this argument is set to `"bootstrap"` or `"boot"`, the bootstrap
  confidence intervals are returned. If bootstrapping was not requested
  or if this argument is set to `"ls"`, then the usual `lm` confidence
  intervals are returned, with a warning raised unless `type` is `"raw"`
  or `"unstandardized".` Default is `"boot"`.

- type:

  String. If `"unstandardized"` or `"raw"`, the confidence intervals of
  the coefficients *before* standardization are returned. If `"beta"` or
  `"standardized"`, then the confidence intervals of the coefficients
  *after* selected variables standardized are returned. Default is
  `"beta"`.

- warn:

  Logical. Whether a warning will be raised is OLS (or WLS) confidence
  intervals are requested for the model with some variables standardized
  (i.e., `type` is `"beta"` or `"standardized"`). Default is `TRUE`.

- boot_type:

  The type of bootstrap confidence intervals. Currently, it supports
  `"perc"`, percentile bootstrap confidence intervals, and `"bc"`,
  bias-corrected bootstrap confidence interval.

- ...:

  Optional arguments. Ignored.

- trace:

  Logical. Whether profiling will be traced. See
  [`stats::confint.glm()`](https://rdrr.io/r/stats/confint.html) for
  details. ignored if `method` is `"boot"` or `"bootstrap"`.

- test:

  The test used for profiling. See
  [stats::confint.glm](https://rdrr.io/r/stats/confint.html) for
  details. ignored if `method` is `"boot"` or `"bootstrap"`.

- transform_b:

  The function to be used to transform the confidence limits. For
  example, if set to `exp`, the confidence limits will be exponentiated.
  Users need to decide whether the transformed limits are meaningful.
  Default is `NULL`.

## Value

A *p* by 2 matrix of the confidence intervals, *p* being the number of
coefficients.

## Details

The type of confidence intervals depends on the object. If bootstrapping
was requested, by default it returns the percentile bootstrap confidence
intervals. Otherwise, it returns the default confidence intervals.

Support for other type of confidence intervals may be added in the
future.

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
confint(lm_beta_x)
#>                    2.50%     97.50%
#> (Intercept)  -430.111608 1905.57231
#> iv          -1569.880043  981.85133
#> mod            45.591866   70.39718
#> cov1           -5.932809   35.46045
#> cat1gp2      -263.296976   32.21821
#> cat1gp3      -209.176316   88.39063
#> iv:mod         -1.968484   22.60068
confint(lm_beta_x, method = "ls")
#> Warning: With standardization, the variance-covariance matrix using OLS or WLS should be used with caution.
#>                    2.5 %     97.5 %
#> (Intercept)  -427.704232 2008.80410
#> iv          -1288.701874 1100.09791
#> mod            45.577790   69.57831
#> cov1           -9.964016   30.01258
#> cat1gp2      -262.002717   36.82678
#> cat1gp3      -200.713438   94.50063
#> iv:mod         -3.254568   20.57662
confint(lm_beta_x, type = "raw")
#>                     2.50%      97.50%
#> (Intercept) -6589.9670647 12784.45818
#> iv           -819.0639644   480.69905
#> mod          -117.7931940    72.26797
#> cov1           -5.9328088    35.46045
#> cat1gp2      -263.2969756    32.21821
#> cat1gp3      -209.1763158    88.39063
#> iv:mod         -0.9629292    11.79166


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

confint(logistic_beta_x, method = "default")
#> Warning: With standardization, the non-bootstrap confidence interval from 'lm()' or 'glm()' should be used with caution.
#> Waiting for profiling to be done...
#>                    2.5 %       97.5 %
#> (Intercept) -21.56476101 -11.16194809
#> iv           -7.59527964   5.29854659
#> mod           0.11492731   0.21870418
#> cov1         -0.08055438   0.07209425
#> cat1gp2      -0.78982230   0.36485622
#> cat1gp3      -0.76628720   0.38430391
#> iv:mod       -0.03353504   0.09611326
confint(logistic_beta_x, type = "raw")
#>                    2.50%      97.50%
#> (Intercept) -53.61484192 64.18330931
#> iv           -5.44412802  2.47097821
#> mod          -0.80287920  0.38267105
#> cov1         -0.07685971  0.09826079
#> cat1gp2      -0.70584857  0.29355782
#> cat1gp3      -0.77215570  0.29715148
#> iv:mod       -0.01392397  0.06450518
```
