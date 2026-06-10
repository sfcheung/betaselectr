# Test Dataset with Moderator and Mediator

This dataset has one mediator, one moderator, one independent variable,
one dependent variable, and two control variables.

## Usage

``` r
data_test_medmod
```

## Format

A data frame with 200 rows and five variables:

- dv:

  Dependent variable, continuous

- iv:

  Independent variable, continuous

- mod:

  Moderator, continuous

- med:

  Mediator, continuous

- cov1:

  Control variable, continuous

- cov2:

  Control variable, continuous

## Examples

``` r

library(lavaan)
mod <-
"
med ~ iv + mod + iv:mod + cov1 + cov2
dv ~ med + iv + cov1 + cov2
"
fit <- sem(mod,
           data_test_medmod)
summary(fit)
#> lavaan 0.6-21 ended normally after 2 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        11
#> 
#>   Number of observations                           200
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 2.303
#>   Degrees of freedom                                 2
#>   P-value (Chi-square)                           0.316
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Regressions:
#>                    Estimate   Std.Err  z-value  P(>|z|)
#>   med ~                                                
#>     iv                -6.373    0.985   -6.473    0.000
#>     mod               -3.899    0.614   -6.346    0.000
#>     iv:mod             0.286    0.039    7.340    0.000
#>     cov1              -0.093    0.070   -1.327    0.185
#>     cov2               0.242    0.133    1.823    0.068
#>   dv ~                                                 
#>     med                0.092    0.011    8.098    0.000
#>     iv                 0.227    0.038    5.896    0.000
#>     cov1              -0.006    0.013   -0.454    0.650
#>     cov2               0.030    0.025    1.230    0.219
#> 
#> Variances:
#>                    Estimate   Std.Err  z-value  P(>|z|)
#>    .med               60.292    6.029   10.000    0.000
#>    .dv                 2.087    0.209   10.000    0.000
#> 

```
