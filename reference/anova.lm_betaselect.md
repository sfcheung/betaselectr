# ANOVA Tables For 'lm_betaselect' and 'glm_betaselect' Objects

Return the analysis of variance tables for the outputs of
[`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
and
[`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md).

## Usage

``` r
# S3 method for class 'lm_betaselect'
anova(object, ..., type = c("beta", "standardized", "raw", "unstandardized"))

# S3 method for class 'glm_betaselect'
anova(
  object,
  ...,
  type = c("beta", "standardized", "raw", "unstandardized"),
  dispersion = NULL,
  test = NULL
)
```

## Arguments

- object:

  The output of
  [`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  or
  [`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md).

- ...:

  Additional outputs of
  [`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  or
  [`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md).

- type:

  String. If `"unstandardized"` or `"raw"`, the output *before*
  standardization are used If `"beta"` or `"standardized"`, then the
  output *after* selected variables standardized are returned. Default
  is `"beta"`.

- dispersion:

  To be passed to
  [`stats::anova.glm()`](https://rdrr.io/r/stats/anova.glm.html). The
  dispersion parameter. Default ia `NULL` and it is extracted from the
  model.

- test:

  String. The test to be conducted. Please refer to
  [`stats::anova.glm()`](https://rdrr.io/r/stats/anova.glm.html) for
  details.

## Value

It returns an object of class `anova`, which is identical to the output
of [`stats::anova()`](https://rdrr.io/r/stats/anova.html) in structure.

## Details

By default, it calls
[`stats::anova()`](https://rdrr.io/r/stats/anova.html) on the results
with selected variables standardized. By setting `type` to `"raw"` or
`"unstandardized"`, it calls
[`stats::anova()`](https://rdrr.io/r/stats/anova.html) on the results
*before* standardization.

## See also

[`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

data(data_test_mod_cat)

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
                           data = data_test_mod_cat,
                           to_standardize = "iv",
                           do_boot = FALSE)
anova(lm_beta_x)
#> Analysis of Variance Table
#> 
#> Response: dv
#>            Df    Sum Sq   Mean Sq  F value Pr(>F)    
#> iv          1 302739738 302739738 652.6310 <2e-16 ***
#> mod         1  40936174  40936174  88.2481 <2e-16 ***
#> cov1        1    341505    341505   0.7362 0.3913    
#> cat1        2   1118110    559055   1.2052 0.3005    
#> iv:mod      1    946107    946107   2.0396 0.1539    
#> Residuals 493 228690783    463876                    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
anova(lm_beta_x, type = "raw")
#> Analysis of Variance Table
#> 
#> Response: dv
#>            Df    Sum Sq   Mean Sq  F value Pr(>F)    
#> iv          1 302739738 302739738 652.6310 <2e-16 ***
#> mod         1  40936174  40936174  88.2481 <2e-16 ***
#> cov1        1    341505    341505   0.7362 0.3913    
#> cat1        2   1118110    559055   1.2052 0.3005    
#> iv:mod      1    946107    946107   2.0396 0.1539    
#> Residuals 493 228690783    463876                    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
                              yes = 1,
                              no = 0)
logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
                                  data = data_test_mod_cat,
                                  family = binomial,
                                  to_standardize = "iv")
anova(logistic_beta_x)
#> Analysis of Deviance Table
#> 
#> Model: binomial, link: logit
#> 
#> Response: p
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>        Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#> NULL                     499     692.86              
#> iv      1  206.034       498     486.83 < 2.2e-16 ***
#> mod     1   45.127       497     441.70 1.847e-11 ***
#> cov1    1    0.034       496     441.66    0.8531    
#> cat1    2    0.675       494     440.99    0.7136    
#> iv:mod  1    0.886       493     440.10    0.3465    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
anova(logistic_beta_x, type = "raw")
#> Analysis of Deviance Table
#> 
#> Model: binomial, link: logit
#> 
#> Response: p
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>        Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#> NULL                     499     692.86              
#> iv      1  206.034       498     486.83 < 2.2e-16 ***
#> mod     1   45.127       497     441.70 1.847e-11 ***
#> cov1    1    0.034       496     441.66    0.8531    
#> cat1    2    0.675       494     440.99    0.7136    
#> iv:mod  1    0.886       493     440.10    0.3465    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
