# Confidence Intervals for a 'lav_betaselect'-Class Object

Return the confidence intervals of betas-select in the output of
[`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md).

## Usage

``` r
# S3 method for class 'lav_betaselect'
confint(object, parm, level = 0.95, ...)
```

## Arguments

- object:

  The output of
  [`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md).

- parm:

  Ignored due to the complexity in the naming. The confidence intervals
  of all parameters are always returned.

- level:

  The level of confidence. Ignored because the intervals should be
  formed when calling
  [`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md).

- ...:

  Optional arguments. Ignored.

## Value

A two-column matrix of the confidence intervals.

## Details

The type of confidence intervals depends on the call to
[`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md).
This function does not recompute the confidence interval.

## See also

[`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
library(lavaan)
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
confint(fit_beta)
#>                [,1] [,2]
#> med~iv           NA   NA
#> med~mod          NA   NA
#> med~iv:mod       NA   NA
#> dv~med           NA   NA
#> dv~iv            NA   NA
#> med~~med         NA   NA
#> dv~~dv           NA   NA
#> iv~~iv           NA   NA
#> iv~~mod          NA   NA
#> iv~~iv:mod       NA   NA
#> mod~~mod         NA   NA
#> mod~~iv:mod      NA   NA
#> iv:mod~~iv:mod   NA   NA
```
