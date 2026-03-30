# Betas-Select in a 'lavaan'-Model

Can standardize selected variables in a `lavaan` model without refitting
the models, can handle product term correctly and skip categorical
predictors in standardization.

## Usage

``` r
lav_betaselect(
  object,
  to_standardize = ".all.",
  not_to_standardize = NULL,
  skip_categorical_x = TRUE,
  output = c("data.frame", "text"),
  std_se = c("none", "delta", "bootstrap"),
  std_z = TRUE,
  std_pvalue = TRUE,
  std_ci = TRUE,
  level = 0.95,
  progress = TRUE,
  boot_out = NULL,
  bootstrap = 100L,
  store_boot_est = TRUE,
  parallel = c("no", "snow", "multicore"),
  ncpus = parallel::detectCores(logical = FALSE) - 1,
  cl = NULL,
  iseed = NULL,
  find_product_terms = TRUE,
  check_mean_centering = FALSE,
  std_intercept = FALSE,
  ...,
  delta_method = c("lavaan", "numDeriv"),
  vector_form = TRUE
)
```

## Arguments

- object:

  The output of `lavaan` model fit functions, such as
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- to_standardize:

  A string vector, which should be the names of the variables to be
  standardized. Default is `".all."`, indicating all variables are to be
  standardized (but see `skip_categorical_x`).

- not_to_standardize:

  A string vector, which should be the names of the variables that
  should not be standardized. This argument is useful when most
  variables, except for a few, are to be standardized. This argument
  cannot be ued with `to_standardize` at the same time. Default is
  `NULL`, and only `to_standardize` is used.

- skip_categorical_x:

  Logical. If `TRUE`, the default, all categorical predictors, defined
  as variables with only two possible values in the data analyzed, will
  be skipped in standardization. This overrides the argument
  `to_standardize`. That is, a categorical predictor will not be
  standardized even if listed in `to_standardize`, unless users set this
  argument to `FALSE`.

- output:

  The format of the output. Not used because the format of the printout
  is now controlled by the `print`-method of the output of this
  function. Kept for backward compatibility.

- std_se:

  String. If set to `"none"`, the default, standard errors will not be
  computed for the standardized solution. If set to `"delta"`, delta
  method will be used to compute the standard errors. If set to
  `"bootstrap"`, then what it does depends whether `boot_out` is set. If
  `boot_out` is to an output of
  [`manymome::do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.html),
  its content will be used. If `boot_out` is `NULL` *and* bootstrap
  estimates are available in `object` (e.g., bootstrapping is requested
  when fitting the model in `lavaan`), then the stored bootstrap
  estimates will be sued. If not available, the bootstrapping will be
  conducted using
  [`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html),
  using arguments `bootstrap`, `parallel`, `ncpus`, `cl`, and `iseed`.\`

- std_z:

  Logical. If `TRUE` and `std_se` is not set to `"none"`, standard error
  will be computed using the method specified in `std_se`. Default is
  `TRUE`.

- std_pvalue:

  Logical. If `TRUE`, `std_se` is not set to `"none"`, and `std_z` is
  `TRUE`, *p*-values will be computed using the method specified in
  `std_se`. For bootstrapping, the method proposed by Asparouhov and
  Muthén (2021) is used. Default is `TRUE`.

- std_ci:

  Logical. If `TRUE` and `std_se` is not set to `"none"`, confidence
  intervals will be computed using the method specified in `std_se`.
  Default is `FALSE.`

- level:

  The level of confidence of the confidence intervals. Default is .95.
  It will be used in the confidence intervals of both the unstandardized
  and standardized solution.

- progress:

  Logical. If `TRUE`, progress bars will be displayed for long process.

- boot_out:

  If `std_se` is `"bootstrap"` and this argument is set to an output of
  [`manymome::do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.html),
  its output will be used in computing statistics such as standard
  errors and confidence intervals. This allows users to use methods
  other than bootstrapping when fitting the model, while they can still
  request bootstrapping for the standardized solution.

- bootstrap:

  If `std_se` is `"bootstrap"` but bootstrapping is not requested when
  fitting the model and `boot_out` is not set,
  [`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html)
  will be called to do bootstrapping. This argument is the number of
  bootstrap samples to draw. Default is 100. Should be set to 5000 or
  even 10000 for stable results.

- store_boot_est:

  Logical. If `std_se` is `"bootstrap"` and this argument is `TRUE`, the
  default, the bootstrap estimates of the standardized solution will be
  stored in the attribute `"boot_est"`. These estimates can be used for
  diagnosis of the bootstrapping. If `FALSE`, then the bootstrap
  estimates will not be stored.

- parallel:

  If `std_se` is `"bootstrap"` but bootstrapping is not requested when
  fitting the model and `boot_out` is not set,
  [`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html)
  will be called to do bootstrapping. This argument is to be passed to
  [`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html).
  Default is `"no"`.

- ncpus:

  If `std_se` is `"bootstrap"` but bootstrapping is not requested when
  fitting the model and `boot_out` is not set,
  [`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html)
  will be called to do bootstrapping. This argument is to be passed to
  [`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html).
  Default is `parallel::detectCores(logical = FALSE) - 1`. Ignored if
  `parallel` is `"no"`.

- cl:

  If `std_se` is `"bootstrap"` but bootstrapping is not requested when
  fitting the model and `boot_out` is not set,
  [`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html)
  will be called to do bootstrapping. This argument is to be passed to
  [`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html).
  Default is `NULL`. Ignored if `parallel` is `"no"`.

- iseed:

  If `std_se` is `"bootstrap"` but bootstrapping is not requested when
  fitting the model and `boot_out` is not set,
  [`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html)
  will be called to do bootstrapping. This argument is to be passed to
  [`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html)
  to set the seed for the random resampling. Default is `NULL`. Should
  be set to an integer for reproducible results. Ignored if `parallel`
  is `"no"`.

- find_product_terms:

  String. If it is certain that a model does not have product terms,
  setting this to `FALSE` will skip the search, which is time consuming
  for a models with many paths and/or many variables. Default is `TRUE`,
  and the function will automatically identify product terms, if any.

- check_mean_centering:

  Logical. If `TRUE`, it will check whether variables involved in a
  product term has been mean-centered. If not, an error will be raised.

- std_intercept:

  Logical. If `TRUE`, intercepts of `y` variables will also be computed
  based on the variables standardized.

- ...:

  Optional arguments to be passed to the
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html),
  which will be use to generate the output.

- delta_method:

  The method used to compute delta-method standard errors. For internal
  use and should not be changed.

- vector_form:

  The internal method used to compute standardized solution. For
  internal use and should not be changed.

## Value

A `lav_betaselect`-class object, which is a data frame storing the
parameter estimates, similar in form to the output of
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).

## Details

This function lets users select which variables to be standardized when
computing the standardized solution. It has the following features:

- It automatically skips predictors which has only two unique values,
  assuming that they are dummy variables.

- It does not standardize product term, which is incorrect. Instead, it
  computes the product term with its component variables standardized
  first.

- It can be used to generate bootstrap confidence intervals for the
  standardized solution (Falk, 2018). Bootstrap confidence interval is
  better than doing standardization *before* fitting a model because it
  correctly takes into account the sampling variance of the standard
  deviations. It is also better than delta-method confidence interval
  because it takes into account the usually asymmetric distribution of
  parameters after standardization, such as standardized loadings and
  correlations.

- For comparison, it can also report delta-method standard errors and
  confidence intervals if requested.

### Problems With Common Approaches

In most SEM programs, users have limited control on which variables to
standardize when requesting the standardized solution. The solution may
be uninterpretable or misleading in these conditions:

- Dummy variables are standardized and their coefficients cannot be
  interpreted as the difference between two groups on the outcome
  variables.

- Product terms (interaction terms) are standardized and they cannot be
  interpreted as the changes in the effects of focal variables when the
  moderators change (Cheung, Cheung, Lau, Hui, & Vong, 2022).

- Variables with meaningful units can be more difficult to interpret
  when they are standardized (e.g., age).

Moreover, the delta method is usually used in standardization, which is
suboptimal for standardization unless the sample size is large (Falk,
2018). For example, the covariance with variables standardized is a
correlation, and its sampling distribution is skewed unless its
population value is zero. However, delta-method confidence interval for
the correlation is necessarily symmetric around the point estimate.

### Limitations

- It only supports observed variable interaction terms, and only support
  two-way interactions.

- It does not support multilevel models.

- It only supports models fitted to raw data.

## References

Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
Retrieved from
https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf

Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
(2022) Improving an old way to measure moderation effect in standardized
units. *Health Psychology*, *41*(7), 502-505.
[doi:10.1037/hea0001188](https://doi.org/10.1037/hea0001188)

Falk, C. F. (2018). Are robust standard errors the best approach for
interval estimation with nonnormal data in structural equation modeling?
*Structural Equation Modeling: A Multidisciplinary Journal, 25*(2)
244-266.
[doi:10.1080/10705511.2017.1367254](https://doi.org/10.1080/10705511.2017.1367254)

## See also

[`print.lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/print.lav_betaselect.md)
for its print method.

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
fit_beta
#> 
#> Selected Standardization:
#>                     
#>  Standard Error: Nil
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Regressions:
#>          BetaSelect
#>  med ~             
#>   iv          1.845
#>   mod         0.325
#>   iv:mod      0.797
#>  dv ~              
#>   med         0.049
#>   iv          0.333
#> 
#> Covariances:
#>          BetaSelect
#>  iv ~~             
#>   mod         1.894
#>   iv:mod      0.630
#>  mod ~~            
#>   iv:mod      5.733
#> 
#> Variances:
#>          BetaSelect
#>  .med        61.851
#>  .dv          0.574
#>   iv          1.000
#>   mod        23.129
#>   iv:mod     27.015
#> 
#> Footnote:
#> - Variable(s) standardized: dv, iv
#> - Call 'print()' and set 'standardized_only' to 'FALSE' to print both
#>   original estimates and betas-select.
#> - Product terms (iv:mod) have variables standardized before computing
#>   them. The product term(s) is/are not standardized.
print(fit_beta, standardized_only = FALSE)
#> 
#> Selected Standardization:
#>                     
#>  Standard Error: Nil
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Regressions:
#>          Estimate  S.E.      Z P(>|z|)  CI.Lo  CI.Up BSelect
#>  med ~                                                      
#>   iv        0.661 0.217  3.047   0.002  0.236  1.086   1.845
#>   mod       0.325 0.128  2.535   0.011  0.074  0.577   0.325
#>   iv:mod    0.286 0.039  7.248   0.000  0.208  0.363   0.797
#>  dv ~                                                       
#>   med       0.093 0.011  8.298   0.000  0.071  0.115   0.049
#>   iv        0.229 0.039  5.917   0.000  0.153  0.304   0.333
#> 
#> Covariances:
#>          Estimate  S.E.      Z P(>|z|)  CI.Lo  CI.Up BSelect
#>  iv ~~                                                      
#>   mod       5.287                                      1.894
#>   iv:mod    4.908                                      0.630
#>  mod ~~                                                     
#>   iv:mod   16.006                                      5.733
#> 
#> Variances:
#>          Estimate  S.E.      Z P(>|z|)  CI.Lo  CI.Up BSelect
#>  .med      61.851 6.185 10.000   0.000 49.728 73.974  61.851
#>  .dv        2.104 0.210 10.000   0.000  1.692  2.517   0.574
#>   iv        7.795                                      1.000
#>   mod      23.129                                     23.129
#>   iv:mod  210.572                                     27.015
#> 
#> Footnote:
#> - Variable(s) standardized: dv, iv
#> - Betas-select are shown in column 'BSelect'.
#> - Column(s) prefixed by 'BS.*' are for betas-select.
#> - Call 'print()' and set 'standardized_only' to 'TRUE' to print only
#>   betas-select.
#> - Product terms (iv:mod) have variables standardized before computing
#>   them. The product term(s) is/are not standardized.

# In real studies:
# - should set bootstrap to at least 5000
# - should set parallel to "snow" or "multicore"
fit_beta_boot <- lav_betaselect(fit,
                                to_standardize = c("iv", "dv"),
                                std_se = "bootstrap",
                                std_ci = TRUE,
                                bootstrap = 100,
                                iseed = 1234)
fit_beta_boot
#> 
#> Selected Standardization:
#>                                              
#>  Standard Error:      Nonparametric bootstrap
#>  Bootstrap samples:   100                    
#>  Confidence Interval: Percentile             
#>  Level of Confidence: 95.0%                  
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Regressions:
#>          BetaSelect    SE      Z p-value Sig  CI.Lo  CI.Hi CI.Sig
#>  med ~                                                           
#>   iv          1.845 0.592  3.115   0.000 ***  0.813  3.047   Sig.
#>   mod         0.325 0.149  2.185   0.020   *  0.034  0.621   Sig.
#>   iv:mod      0.797 0.114  6.987   0.000 ***  0.567  1.018   Sig.
#>  dv ~                                                            
#>   med         0.049 0.005 10.592   0.000 ***  0.038  0.057   Sig.
#>   iv          0.333 0.046  7.179   0.000 ***  0.226  0.412   Sig.
#> 
#> Covariances:
#>          BetaSelect    SE      Z p-value Sig  CI.Lo  CI.Hi CI.Sig
#>  iv ~~                                                           
#>   mod         1.894                       --                     
#>   iv:mod      0.630                       --                     
#>  mod ~~                                                          
#>   iv:mod      5.733                       --                     
#> 
#> Variances:
#>          BetaSelect    SE      Z p-value Sig  CI.Lo  CI.Hi CI.Sig
#>  .med        61.851 5.836 10.598   0.000 *** 48.732 72.713   Sig.
#>  .dv          0.574 0.054 10.695   0.000 ***  0.441  0.684   Sig.
#>   iv          1.000                       --                     
#>   mod        23.129                       --                     
#>   iv:mod     27.015                       --                     
#> 
#> Footnote:
#> - Variable(s) standardized: dv, iv
#> - Sig codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> - Standard errors, p-values, and confidence intervals are not computed
#>   for betas-select which are fixed in the standardized solution.
#> - P-values for betas-select are asymmetric bootstrap p-value computed
#>   by the method of Asparouhov and Muthén (2021).
#> - Call 'print()' and set 'standardized_only' to 'FALSE' to print both
#>   original estimates and betas-select.
#> - Product terms (iv:mod) have variables standardized before computing
#>   them. The product term(s) is/are not standardized.
print(fit_beta_boot, standardized_only = FALSE)
#> 
#> Selected Standardization:
#>                                              
#>  Standard Error:      Nonparametric bootstrap
#>  Bootstrap samples:   100                    
#>  Confidence Interval: Percentile             
#>  Level of Confidence: 95.0%                  
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Regressions:
#>          Estimate  S.E.      Z P(>|z|)  CI.Lo  CI.Up BSelect BS.SE   BS.Z  BS.p
#>  med ~                                                                         
#>   iv        0.661 0.217  3.047   0.002  0.236  1.086   1.845 0.592  3.115 0.000
#>   mod       0.325 0.128  2.535   0.011  0.074  0.577   0.325 0.149  2.185 0.020
#>   iv:mod    0.286 0.039  7.248   0.000  0.208  0.363   0.797 0.114  6.987 0.000
#>  dv ~                                                                          
#>   med       0.093 0.011  8.298   0.000  0.071  0.115   0.049 0.005 10.592 0.000
#>   iv        0.229 0.039  5.917   0.000  0.153  0.304   0.333 0.046  7.179 0.000
#>  BS.Sig BS.CI.Lo BS.CI.Hi BS.CI.Sig
#>                                    
#>     ***    0.813    3.047      Sig.
#>       *    0.034    0.621      Sig.
#>     ***    0.567    1.018      Sig.
#>                                    
#>     ***    0.038    0.057      Sig.
#>     ***    0.226    0.412      Sig.
#> 
#> Covariances:
#>          Estimate  S.E.      Z P(>|z|)  CI.Lo  CI.Up BSelect BS.SE   BS.Z  BS.p
#>  iv ~~                                                                         
#>   mod       5.287                                      1.894                   
#>   iv:mod    4.908                                      0.630                   
#>  mod ~~                                                                        
#>   iv:mod   16.006                                      5.733                   
#>  BS.Sig BS.CI.Lo BS.CI.Hi BS.CI.Sig
#>                                    
#>      --                            
#>      --                            
#>                                    
#>      --                            
#> 
#> Variances:
#>          Estimate  S.E.      Z P(>|z|)  CI.Lo  CI.Up BSelect BS.SE   BS.Z  BS.p
#>  .med      61.851 6.185 10.000   0.000 49.728 73.974  61.851 5.836 10.598 0.000
#>  .dv        2.104 0.210 10.000   0.000  1.692  2.517   0.574 0.054 10.695 0.000
#>   iv        7.795                                      1.000                   
#>   mod      23.129                                     23.129                   
#>   iv:mod  210.572                                     27.015                   
#>  BS.Sig BS.CI.Lo BS.CI.Hi BS.CI.Sig
#>     ***   48.732   72.713      Sig.
#>     ***    0.441    0.684      Sig.
#>      --                            
#>      --                            
#>      --                            
#> 
#> Footnote:
#> - Variable(s) standardized: dv, iv
#> - Sig codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> - Standard errors, p-values, and confidence intervals are not computed
#>   for betas-select which are fixed in the standardized solution.
#> - P-values for betas-select are asymmetric bootstrap p-value computed
#>   by the method of Asparouhov and Muthén (2021).
#> - Betas-select are shown in column 'BSelect'.
#> - Column(s) prefixed by 'BS.*' are for betas-select.
#> - Call 'print()' and set 'standardized_only' to 'TRUE' to print only
#>   betas-select.
#> - Product terms (iv:mod) have variables standardized before computing
#>   them. The product term(s) is/are not standardized.

# Print full results
print(fit_beta_boot,
      standardized_only = FALSE)
#> 
#> Selected Standardization:
#>                                              
#>  Standard Error:      Nonparametric bootstrap
#>  Bootstrap samples:   100                    
#>  Confidence Interval: Percentile             
#>  Level of Confidence: 95.0%                  
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Regressions:
#>          Estimate  S.E.      Z P(>|z|)  CI.Lo  CI.Up BSelect BS.SE   BS.Z  BS.p
#>  med ~                                                                         
#>   iv        0.661 0.217  3.047   0.002  0.236  1.086   1.845 0.592  3.115 0.000
#>   mod       0.325 0.128  2.535   0.011  0.074  0.577   0.325 0.149  2.185 0.020
#>   iv:mod    0.286 0.039  7.248   0.000  0.208  0.363   0.797 0.114  6.987 0.000
#>  dv ~                                                                          
#>   med       0.093 0.011  8.298   0.000  0.071  0.115   0.049 0.005 10.592 0.000
#>   iv        0.229 0.039  5.917   0.000  0.153  0.304   0.333 0.046  7.179 0.000
#>  BS.Sig BS.CI.Lo BS.CI.Hi BS.CI.Sig
#>                                    
#>     ***    0.813    3.047      Sig.
#>       *    0.034    0.621      Sig.
#>     ***    0.567    1.018      Sig.
#>                                    
#>     ***    0.038    0.057      Sig.
#>     ***    0.226    0.412      Sig.
#> 
#> Covariances:
#>          Estimate  S.E.      Z P(>|z|)  CI.Lo  CI.Up BSelect BS.SE   BS.Z  BS.p
#>  iv ~~                                                                         
#>   mod       5.287                                      1.894                   
#>   iv:mod    4.908                                      0.630                   
#>  mod ~~                                                                        
#>   iv:mod   16.006                                      5.733                   
#>  BS.Sig BS.CI.Lo BS.CI.Hi BS.CI.Sig
#>                                    
#>      --                            
#>      --                            
#>                                    
#>      --                            
#> 
#> Variances:
#>          Estimate  S.E.      Z P(>|z|)  CI.Lo  CI.Up BSelect BS.SE   BS.Z  BS.p
#>  .med      61.851 6.185 10.000   0.000 49.728 73.974  61.851 5.836 10.598 0.000
#>  .dv        2.104 0.210 10.000   0.000  1.692  2.517   0.574 0.054 10.695 0.000
#>   iv        7.795                                      1.000                   
#>   mod      23.129                                     23.129                   
#>   iv:mod  210.572                                     27.015                   
#>  BS.Sig BS.CI.Lo BS.CI.Hi BS.CI.Sig
#>     ***   48.732   72.713      Sig.
#>     ***    0.441    0.684      Sig.
#>      --                            
#>      --                            
#>      --                            
#> 
#> Footnote:
#> - Variable(s) standardized: dv, iv
#> - Sig codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> - Standard errors, p-values, and confidence intervals are not computed
#>   for betas-select which are fixed in the standardized solution.
#> - P-values for betas-select are asymmetric bootstrap p-value computed
#>   by the method of Asparouhov and Muthén (2021).
#> - Betas-select are shown in column 'BSelect'.
#> - Column(s) prefixed by 'BS.*' are for betas-select.
#> - Call 'print()' and set 'standardized_only' to 'TRUE' to print only
#>   betas-select.
#> - Product terms (iv:mod) have variables standardized before computing
#>   them. The product term(s) is/are not standardized.

```
