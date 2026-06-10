# Test Dataset with Moderator and Categorical Variables (Version 2)

This dataset has one predictor, one moderator, one control variable, one
dependent variable, and a categorical variable.

Similar to `data_test_mod_cat` but generated from another population.

## Usage

``` r
data_test_mod_cat2
```

## Format

A data frame with 300 rows and five variables:

- dv:

  Dependent variable, continuous

- iv:

  Independent variable, continuous

- mod:

  Moderator, continuous

- cov1:

  Control variable, continuous

- cat1:

  String variable with these values: "gp1", "gp2", and "gp3"

## Examples

``` r

lm_out <- lm(dv ~ iv * mod + cov1 + cat1, data_test_mod_cat)
summary(lm_out)
#> 
#> Call:
#> lm(formula = dv ~ iv * mod + cov1 + cat1, data = data_test_mod_cat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1987.03  -463.99     0.25   455.14  2152.48 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)
#> (Intercept) 1488.568   4540.789   0.328    0.743
#> iv           -46.545    300.046  -0.155    0.877
#> mod           -6.530     45.372  -0.144    0.886
#> cov1          10.024     10.173   0.985    0.325
#> cat1gp2     -112.588     76.046  -1.481    0.139
#> cat1gp3      -53.106     75.126  -0.707    0.480
#> iv:mod         4.275      2.993   1.428    0.154
#> 
#> Residual standard error: 681.1 on 493 degrees of freedom
#> Multiple R-squared:  0.6021, Adjusted R-squared:  0.5973 
#> F-statistic: 124.3 on 6 and 493 DF,  p-value: < 2.2e-16
#> 

```
