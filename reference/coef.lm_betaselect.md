# Coefficients of Beta-Select in Linear Models

Return the estimates of coefficients in an `lm_betaselect`-class or
`glm_betaselect`-class object.

## Usage

``` r
# S3 method for class 'lm_betaselect'
coef(
  object,
  complete = FALSE,
  type = c("beta", "standardized", "raw", "unstandardized"),
  ...
)

# S3 method for class 'glm_betaselect'
coef(
  object,
  complete = FALSE,
  type = c("beta", "standardized", "raw", "unstandardized"),
  ...
)
```

## Arguments

- object:

  The output of
  [`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  or
  [`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md),
  or an `lm_betaselect`-class or `glm_betaselect`-class object.

- complete:

  If `TRUE`, it returns the full vector of coefficients, including those
  of terms dropped in an over-determined system. See
  [`stats::coef()`](https://rdrr.io/r/stats/coef.html) for further
  information. Default is `FALSE`.

- type:

  String. If `"unstandardized"` or `"raw"`, the coefficients *before*
  standardization are returned. If `"beta"` or `"standardized"`, then
  the coefficients *after* selected variables standardized are returned.
  Default is `"beta"`.

- ...:

  Other arguments. Ignored.

## Value

A numeric vector: The estimate of regression coefficients.

## Details

By default, it extracts the regression coefficients *after* the selected
variables have been standardized. If requested, it can also return the
regression coefficients *before* standardization.

## See also

[`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
and
[`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
data(data_test_mod_cat)

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
                           data = data_test_mod_cat,
                           to_standardize = "iv")
coef(lm_beta_x)
#> (Intercept)          iv         mod        cov1     cat1gp2     cat1gp3 
#>  790.549933  -94.301982   57.578053   10.024281 -112.587966  -53.106405 
#>      iv:mod 
#>    8.661027 
coef(lm_beta_x, type = "raw")
#> (Intercept)          iv         mod        cov1     cat1gp2     cat1gp3 
#> 1488.568116  -46.545177   -6.530403   10.024281 -112.587966  -53.106405 
#>      iv:mod 
#>    4.274873 


data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
                              yes = 1,
                              no = 0)
logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
                                  data = data_test_mod_cat,
                                  family = binomial,
                                  to_standardize = "iv")
coef(logistic_beta_x)
#>   (Intercept)            iv           mod          cov1       cat1gp2 
#> -16.205361985  -1.148482578   0.165201690  -0.004369295  -0.210517691 
#>       cat1gp3        iv:mod 
#>  -0.189052500   0.030878129 
coef(logistic_beta_x, type = "raw")
#>  (Intercept)           iv          mod         cov1      cat1gp2      cat1gp3 
#> -7.704355870 -0.566863225 -0.063356556 -0.004369295 -0.210517691 -0.189052500 
#>       iv:mod 
#>  0.015240698 
```
