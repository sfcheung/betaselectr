# Coefficients of a 'lav_betaselect'-Class Object

Return the betas-select in a 'lav_betaselect'-class object.

## Usage

``` r
# S3 method for class 'lav_betaselect'
coef(object, drop_na = FALSE, ...)
```

## Arguments

- object:

  The output of
  [`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md).

- drop_na:

  Logical. Whether betas-select with `NA` are dropped. Default is
  `FALSE`.

- ...:

  Optional arguments. Not used.

## Value

A numeric vector: The betas-select in the object. The names of
parameters follow the convention in `lavaan`.

## Details

It just extracts and returns the column `est` from the object: the
betas-select, with selected variables standardized.

## See also

[`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
library(lavaan)
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.
# Need to mean-center iv and mod
data_test_medmod$iv <- data_test_medmod$iv - mean(data_test_medmod$iv)
data_test_medmod$mod <- data_test_medmod$mod - mean(data_test_medmod$mod)
mod <-
"
med ~ iv + mod + iv:mod
dv ~ med + iv
"
fit <- sem(mod,
           data_test_medmod,
           fixed.x = TRUE)
summary(fit)
#> lavaan 0.6-21 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         7
#> 
#>   Number of observations                           200
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 2.685
#>   Degrees of freedom                                 2
#>   P-value (Chi-square)                           0.261
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   med ~                                               
#>     iv                0.661    0.217    3.047    0.002
#>     mod               0.325    0.128    2.535    0.011
#>     iv:mod            0.286    0.039    7.248    0.000
#>   dv ~                                                
#>     med               0.093    0.011    8.298    0.000
#>     iv                0.229    0.039    5.917    0.000
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .med              61.851    6.185   10.000    0.000
#>    .dv                2.104    0.210   10.000    0.000
#> 
fit_beta <- lav_betaselect(fit,
                           to_standardize = c("iv", "dv"))
coef(fit_beta)
#>         med~iv        med~mod     med~iv:mod         dv~med          dv~iv 
#>     1.84456919     0.32536288     0.79715974     0.04874437     0.33334073 
#>       med~~med         dv~~dv         iv~~iv        iv~~mod     iv~~iv:mod 
#>    61.85098096     0.57409180     1.00000000     1.89369830     0.62963826 
#>       mod~~mod    mod~~iv:mod iv:mod~~iv:mod 
#>    23.12940750     5.73310611    27.01455323 
```
