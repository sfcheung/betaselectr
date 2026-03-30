# Test Dataset with a Binary Outcome Variable

This dataset has one predictor, one moderator, one control variable, one
binary dependent variable, and a categorical variable.

## Usage

``` r
data_test_mod_cat_binary
```

## Format

A data frame with 300 rows and five variables:

- dv:

  Dependent variable, binary: 0, 1

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
glm_out <- glm(dv ~ iv * mod + cov1 + cat1, data_test_mod_cat_binary, family = binomial())
summary(glm_out)
#> 
#> Call:
#> glm(formula = dv ~ iv * mod + cov1 + cat1, family = binomial(), 
#>     data = data_test_mod_cat_binary)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept) 24.36566    9.83244   2.478 0.013209 *  
#> iv          -1.83370    0.67576  -2.714 0.006657 ** 
#> mod         -0.52322    0.19848  -2.636 0.008385 ** 
#> cov1        -0.02286    0.06073  -0.376 0.706562    
#> cat1gp2      0.89002    0.36257   2.455 0.014100 *  
#> cat1gp3      1.28291    0.34448   3.724 0.000196 ***
#> iv:mod       0.03815    0.01364   2.797 0.005163 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 415.03  on 299  degrees of freedom
#> Residual deviance: 390.91  on 293  degrees of freedom
#> AIC: 404.91
#> 
#> Number of Fisher Scoring iterations: 4
#> 

```
