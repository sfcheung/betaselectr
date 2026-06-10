# Betas-Select in a Regression Model

Can fit a linear regression models with selected variables standardized;
handle product terms correctly and skip categorical predictors in
standardization.

## Usage

``` r
lm_betaselect(
  ...,
  to_standardize = NULL,
  not_to_standardize = NULL,
  skip_response = FALSE,
  do_boot = TRUE,
  bootstrap = 100L,
  iseed = NULL,
  parallel = FALSE,
  ncpus = parallel::detectCores(logical = FALSE) - 1,
  progress = TRUE,
  load_balancing = FALSE,
  model_call = c("lm", "glm")
)

glm_betaselect(
  ...,
  to_standardize = NULL,
  not_to_standardize = NULL,
  skip_response = FALSE,
  do_boot = TRUE,
  bootstrap = 100L,
  iseed = NULL,
  parallel = FALSE,
  ncpus = parallel::detectCores(logical = FALSE) - 1,
  progress = TRUE,
  load_balancing = FALSE
)

# S3 method for class 'lm_betaselect'
print(
  x,
  digits = max(3L, getOption("digits") - 3L),
  type = c("beta", "standardized", "raw", "unstandardized"),
  ...
)

# S3 method for class 'glm_betaselect'
print(
  x,
  digits = max(3L, getOption("digits") - 3L),
  type = c("beta", "standardized", "raw", "unstandardized"),
  ...
)

raw_output(x)
```

## Arguments

- ...:

  For `lm_betaselect()`. these arguments will be passed directly to
  [`lm()`](https://rdrr.io/r/stats/lm.html). For `glm_betaselect()`,
  these arguments will be passed to
  [`glm()`](https://rdrr.io/r/stats/glm.html). For the `print`-method of
  `lm_betaselect` or `glm_betaselect` objects, this will be passed to
  other methods.

- to_standardize:

  A string vector, which should be the names of the variables to be
  standardized. Default is `NULL`, indicating all variables are to be
  standardized.

- not_to_standardize:

  A string vector, which should be the names of the variables that
  should *not* be standardized. This argument is useful when most
  variables, except for a few, are to be standardized. This argument
  cannot be ued with `to_standardize` at the same time. Default is
  `NULL`, and only `to_standardize` is used.

- skip_response:

  Logical. If `TRUE`, will not standardize the response (outcome)
  variable even if it appears in `to_standardize` or `to_standardize` is
  not specified. Used for models such as logistic regression models in
  which there are some restrictions on the response variables (e.g.,
  only 0 or 1 for logistic regression).

- do_boot:

  Whether bootstrapping will be conducted. Default is `TRUE`.

- bootstrap:

  If `do_boot` is `TRUE`, this argument is the number of bootstrap
  samples to draw. Default is 100. Should be set to 5000 or even 10000
  for stable results.

- iseed:

  If `do_boot` is `TRUE` and this argument is not `NULL`, it will be
  used by [`set.seed()`](https://rdrr.io/r/base/Random.html) to set the
  seed for the random number generator. Default is `NULL`.

- parallel:

  If `do_boot` is `TRUE` and this argument is `TRUE`, parallel
  processing will be used to do bootstrapping. Default is `FALSE`
  because bootstrapping for models fitted by
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) is rarely slow.
  Actually, if both `parallel` and `progress` are set to `TRUE`, the
  speed may even be slower than serial processing.

- ncpus:

  If `do_boot` is `TRUE` and `parallel` is also `TRUE`, this argument is
  the number of processes to be used in parallel processing. Default is
  `parallel::detectCores(logical = FALSE) - 1`

- progress:

  Logical. If `TRUE`, progress bars will be displayed for long process.
  Default is `TRUE`.

- load_balancing:

  Logical. If `parallel` is `TRUE`, this determines whether load
  balancing will be used. Default is `FALSE` because the gain in speed
  is usually minor.

- model_call:

  The model function to be called. If `"lm"`, the default, the model
  will be fitted by [`stats::lm()`](https://rdrr.io/r/stats/lm.html). If
  `"glm"`, the model will be fitted by
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html). Users should call
  the corresponding function directly rather than setting this argument
  manually.

- x:

  An `lm_betaselect` or `glm_betaselect` object.

- digits:

  The number of significant digits to be printed for the coefficients.

- type:

  The coefficients to be printed. For `"beta"` or `"standardized"`, the
  coefficients after selected variables standardized will be printed.
  For `"raw"` or `"unstandardized"`, the coefficients before
  standardization was done will be printed.

## Value

The function `lm_betaselect()` returns an object of the class
`lm_betaselect`, The function `glm_betaselect()` returns an object of
the class `glm_betaselect`. They are similar in structure to the output
of [`stats::lm()`](https://rdrr.io/r/stats/lm.html) and
[`stats::glm()`](https://rdrr.io/r/stats/glm.html), with additional
information stored.

The function `raw_output()` returns an object of the class `lm` or
`glm`, which are the results of fitting the model to the data by
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) without
standardization.

## Details

The functions `lm_betaselect()` and `glm_betaselect()` let users select
which variables to be standardized when computing the standardized
solution. They have the following features:

- They automatically skip categorical predictors (i.e., factor or string
  variables).

- They do not standardize a product term, which is incorrect. Instead,
  they compute the product term with its component variables
  standardized, if requested.

- They standardize the selected variables *before* fitting a model.
  Therefore, If a model has the term `log(x)` and `x` is one of the
  selected variables, the model used the logarithm of the *standardized*
  `x` in the model, instead of standardized `log(x)` which is difficult
  to interpret.

- They can be used to generate nonparametric bootstrap confidence
  intervals for the standardized solution. Bootstrap confidence interval
  is better than the default confidence interval ignoring the
  standardization because it takes into account the sampling variance of
  the standard deviations. Preliminary support for bootstrap confidence
  has been found for forming confidence intervals for coefficients
  involving standardized variables in linear regression (Jones & Waller,
  2013).

### Problems With Common Approaches

In some regression programs, users have limited control on which
variables to standardize when requesting the so-called "betas". The
solution may be uninterpretable or misleading in these conditions:

- Dummy variables are standardized and their coefficients cannot be
  interpreted as the difference between two groups on the outcome
  variables.

- Product terms (interaction terms) are standardized and they cannot be
  interpreted as the changes in the effects of focal variables when the
  moderators change (Cheung, Cheung, Lau, Hui, & Vong, 2022).

- Variables with meaningful units can be more difficult to interpret
  when they are standardized (e.g., age).

### How The Function Work

They standardize the original variables *before* they are used in the
model. Therefore, strictly speaking, they do not standardize the
predictors in model, but standardize the *input variable* (Gelman et
al., 2021).

The requested model is then fitted to the dataset with selected
variables standardized. For the ease of follow-up analysis, both the
results with selected variables standardized and the results without
standardization are stored. If required, the results without
standardization can be retrieved by `raw_output()`.

### Methods

The output of `lm_betaselect()` is an `lm_betaselect`-class object, and
the output of `glm_betaselect()` is a `glm_betaselect`-class object.
They have the following methods:

- A `coef`-method for extracting the coefficients of the model. (See
  [`coef.lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/coef.lm_betaselect.md)
  and
  [`coef.glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/coef.lm_betaselect.md)
  for details.)

- A `vcov`-method for extracting the variance-covariance matrix of the
  estimates of the coefficients. If bootstrapping is requested, it can
  return the matrix based on the bootstrapping estimates. (See
  [`vcov.lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/vcov.lm_betaselect.md)
  and
  [`vcov.glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/vcov.lm_betaselect.md)
  for details.)

- A `confint`-method for forming the confidence intervals of the
  estimates of the coefficients. If bootstrapping is requested, it can
  return the bootstrap confidence intervals. (See
  [`confint.lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/confint.lm_betaselect.md)
  and
  [`confint.glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/confint.lm_betaselect.md)
  for details.)

- A `summary`-method for printing the summary of the results, with
  additional information such as the number of bootstrap samples and
  which variables have been standardized. (See
  [`summary.lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/summary.lm_betaselect.md)
  and
  [`summary.glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/summary.glm_betaselect.md)
  for details.)

- An `anova`-method for printing the ANOVA table. Can also be used to
  compare two or more outputs of `lm_betaselect()` or `glm_betaselect()`
  (See
  [`anova.glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/anova.lm_betaselect.md)
  and
  [`anova.glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/anova.lm_betaselect.md)
  for details.)

- A `predict`-method for computing predicted values. It can be used to
  compute the predicted values given a set of new unstandardized data.
  The data will be standardized before computing the predicted values in
  the models with standardization. (See
  [`predict.lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/predict.lm_betaselect.md)
  and
  [`predict.glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/predict.glm_betaselect.md)
  for details.)

- The default `update`-method for updating a call also works for an
  `lm_betaselect` object or a `glm_betaselect()` object. It can update
  the model in the same way it updates a model fitted by
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html), and also update
  the arguments of `lm_betaselect()` or `glm_betaselect()` such as the
  variables to be standardized. (See
  [`stats::update()`](https://rdrr.io/r/stats/update.html) for details.)

Most other methods for the output of
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) and
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) should also work on
an `lm_betaselect`-class object or a `glm_betaselect`-class object,
respectively. Some of them will give the same results regardless of the
variables standardized. Examples are
[`rstandard()`](https://rdrr.io/r/stats/influence.measures.html) and
[`cooks.distance()`](https://rdrr.io/r/stats/influence.measures.html).
For some others, they should be used with cautions if they make use of
the variance-covariance matrix of the estimates.

To use the methods for `lm` objects or `glm` objects on the results
without standardization, simply use `raw_output()`. For example, to get
the fitted values without standardization, call `fitted(raw_output(x))`,
where `x` is the output of `lm_betaselect()` or `glm_betaselect()`.

The function `raw_output()` simply extracts the regression output by
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) on the variables
without standardization.

## References

Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
(2022) Improving an old way to measure moderation effect in standardized
units. *Health Psychology*, *41*(7), 502-505.
[doi:10.1037/hea0001188](https://doi.org/10.1037/hea0001188)

Craig, C. C. (1936). On the frequency function of xy. *The Annals of
Mathematical Statistics, 7*(1), 1–15.
[doi:10.1214/aoms/1177732541](https://doi.org/10.1214/aoms/1177732541)

Gelman, A., Hill, J., & Vehtari, A. (2021). *Regression and other
stories*. Cambridge University Press.
[doi:10.1017/9781139161879](https://doi.org/10.1017/9781139161879)

Jones, J. A., & Waller, N. G. (2013). Computing confidence intervals for
standardized regression coefficients. *Psychological Methods, 18*(4),
435–453. [doi:10.1037/a0033269](https://doi.org/10.1037/a0033269)

Sun, R. wei, Chang, F., Yang, W., Cheung, S. F., & Cheung, S.-H. (2026).
`betaselectr`: Selective (and proper) standardization in structural
equation models. *Multivariate Behavioral Research*. Advance online
publication.
[doi:10.1080/00273171.2026.2672692](https://doi.org/10.1080/00273171.2026.2672692)

## See also

`print.lm_betaselect()` and `print.glm_betaselect()` for the
`print`-methods.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

data(data_test_mod_cat)

# Standardize only iv

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
                           data = data_test_mod_cat,
                           to_standardize = "iv")
lm_beta_x
#> 
#> Call to lm_betaselect():
#> betaselectr::lm_betaselect(formula = dv ~ iv * mod + cov1 + cat1, 
#>     data = data_test_mod_cat, to_standardize = "iv")
#> 
#> Variable(s) standardized: iv
#> 
#> Model *after* standardization:
#> 
#> Call:
#> stats::lm(formula = dv ~ iv * mod + cov1 + cat1, data = betaselectr::std_data(data = data_test_mod_cat, 
#>     to_standardize = "iv"))
#> 
#> Coefficients:
#> (Intercept)           iv          mod         cov1      cat1gp2      cat1gp3  
#>     790.550      -94.302       57.578       10.024     -112.588      -53.106  
#>      iv:mod  
#>       8.661  
#> 
summary(lm_beta_x)
#> Call to lm_betaselect():
#> betaselectr::lm_betaselect(formula = dv ~ iv * mod + cov1 + cat1, 
#>     data = data_test_mod_cat, to_standardize = "iv")
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
#> (Intercept)   790.550  -616.077  2200.706    628.105   1.259     0.16    
#> iv            -94.302 -1608.007  1514.014    721.336  -0.131     0.82    
#> mod            57.578    43.098    70.722      6.133   9.388   <0.001 ***
#> cov1           10.024   -10.567    33.103     10.126   0.990     0.34    
#> cat1gp2      -112.588  -276.419    57.141     76.889  -1.464     0.22    
#> cat1gp3       -53.106  -217.405    90.518     77.879  -0.682     0.54    
#> iv:mod          8.661    -7.435    24.082      7.200   1.203     0.26    
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

# Manually standardize iv and call lm()

data_test_mod_cat$iv_z <- scale(data_test_mod_cat[, "iv"])[, 1]

lm_beta_x_manual <- lm(dv ~ iv_z*mod + cov1 + cat1,
                       data = data_test_mod_cat)

coef(lm_beta_x)
#> (Intercept)          iv         mod        cov1     cat1gp2     cat1gp3 
#>  790.549933  -94.301982   57.578053   10.024281 -112.587966  -53.106405 
#>      iv:mod 
#>    8.661027 
coef(lm_beta_x_manual)
#> (Intercept)        iv_z         mod        cov1     cat1gp2     cat1gp3 
#>  790.549933  -94.301982   57.578053   10.024281 -112.587966  -53.106405 
#>    iv_z:mod 
#>    8.661027 

# Standardize all numeric variables

lm_beta_all <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
                             data = data_test_mod_cat)
# Note that cat1 is not standardized
summary(lm_beta_all)
#> Call to lm_betaselect():
#> betaselectr::lm_betaselect(formula = dv ~ iv * mod + cov1 + cat1, 
#>     data = data_test_mod_cat)
#> 
#> Variable(s) standardized: dv, iv, mod, cov1, iv_z 
#> 
#> Call:
#> stats::lm(formula = dv ~ iv * mod + cov1 + cat1, data = betaselectr::std_data(data = data_test_mod_cat, 
#>     to_standardize = c("dv", "iv", "mod", "cov1", "iv_z")))
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.85143 -0.43232  0.00024  0.42408  2.00559 
#> 
#> Coefficients:
#>             Estimate CI.Lower CI.Upper Std. Error z value Pr(Boot)    
#> (Intercept)    0.051   -0.047    0.146      0.042   1.217     0.22    
#> iv             0.720    0.671    0.758      0.022  32.675   <0.001 ***
#> mod            0.269    0.228    0.325      0.025  10.584   <0.001 ***
#> cov1           0.028   -0.016    0.082      0.025   1.105     0.28    
#> cat1gp2       -0.105   -0.259    0.056      0.075  -1.393     0.12    
#> cat1gp3       -0.049   -0.191    0.088      0.065  -0.759     0.38    
#> iv:mod         0.040   -0.022    0.099      0.030   1.366     0.16    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.6346 on 493 degrees of freedom
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


data(data_test_mod_cat)

data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
                              yes = 1,
                              no = 0)
# Standardize only iv
logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
                                  family = binomial,
                                  data = data_test_mod_cat,
                                  to_standardize = "iv")
summary(logistic_beta_x)
#> Call to glm_betaselect():
#> betaselectr::lm_betaselect(formula = p ~ iv * mod + cov1 + cat1, 
#>     family = binomial, data = data_test_mod_cat, to_standardize = "iv", 
#>     model_call = "glm")
#> 
#> Variable(s) standardized: iv 
#> 
#> Call:
#> stats::glm(formula = p ~ iv * mod + cov1 + cat1, family = binomial, 
#>     data = betaselectr::std_data(data = data_test_mod_cat, to_standardize = "iv"))
#> 
#> Coefficients:
#>             Estimate CI.Lower CI.Upper Std. Error z value Pr(Boot)    
#> (Intercept)  -16.205  -25.558  -10.935      3.337  -4.856   <0.001 ***
#> iv            -1.148   -9.603    7.538      4.062  -0.283     0.80    
#> mod            0.165    0.115    0.264      0.034   4.918   <0.001 ***
#> cov1          -0.004   -0.079    0.105      0.048  -0.091     0.92    
#> cat1gp2       -0.211   -0.870    0.502      0.344  -0.611     0.48    
#> cat1gp3       -0.189   -0.829    0.420      0.307  -0.616     0.60    
#> iv:mod         0.031   -0.056    0.116      0.041   0.757     0.42    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 692.86  on 499  degrees of freedom
#> Residual deviance: 440.10  on 493  degrees of freedom
#> AIC: 454.1
#> 
#> Number of Fisher Scoring iterations: 5
#> 
#> Transformed Parameter Estimates:
#>             Exp(B) CI.Lower CI.Upper
#> (Intercept)  0.000    0.000    0.000
#> iv           0.317    0.000 1946.771
#> mod          1.180    1.122    1.302
#> cov1         0.996    0.924    1.111
#> cat1gp2      0.810    0.419    1.653
#> cat1gp3      0.828    0.437    1.521
#> iv:mod       1.031    0.945    1.123
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

logistic_beta_x
#> 
#> Call to lm_betaselect():
#> betaselectr::lm_betaselect(formula = p ~ iv * mod + cov1 + cat1, 
#>     family = binomial, data = data_test_mod_cat, to_standardize = "iv", 
#>     model_call = "glm")
#> 
#> Variable(s) standardized: iv
#> 
#> Model *after* standardization:
#> 
#> Call:  stats::glm(formula = p ~ iv * mod + cov1 + cat1, family = binomial, 
#>     data = betaselectr::std_data(data = data_test_mod_cat, to_standardize = "iv"))
#> 
#> Coefficients:
#> (Intercept)           iv          mod         cov1      cat1gp2      cat1gp3  
#>  -16.205362    -1.148483     0.165202    -0.004369    -0.210518    -0.189053  
#>      iv:mod  
#>    0.030878  
#> 
#> Degrees of Freedom: 499 Total (i.e. Null);  493 Residual
#> Null Deviance:       692.9 
#> Residual Deviance: 440.1     AIC: 454.1
summary(logistic_beta_x)
#> Call to glm_betaselect():
#> betaselectr::lm_betaselect(formula = p ~ iv * mod + cov1 + cat1, 
#>     family = binomial, data = data_test_mod_cat, to_standardize = "iv", 
#>     model_call = "glm")
#> 
#> Variable(s) standardized: iv 
#> 
#> Call:
#> stats::glm(formula = p ~ iv * mod + cov1 + cat1, family = binomial, 
#>     data = betaselectr::std_data(data = data_test_mod_cat, to_standardize = "iv"))
#> 
#> Coefficients:
#>             Estimate CI.Lower CI.Upper Std. Error z value Pr(Boot)    
#> (Intercept)  -16.205  -25.558  -10.935      3.337  -4.856   <0.001 ***
#> iv            -1.148   -9.603    7.538      4.062  -0.283     0.80    
#> mod            0.165    0.115    0.264      0.034   4.918   <0.001 ***
#> cov1          -0.004   -0.079    0.105      0.048  -0.091     0.92    
#> cat1gp2       -0.211   -0.870    0.502      0.344  -0.611     0.48    
#> cat1gp3       -0.189   -0.829    0.420      0.307  -0.616     0.60    
#> iv:mod         0.031   -0.056    0.116      0.041   0.757     0.42    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 692.86  on 499  degrees of freedom
#> Residual deviance: 440.10  on 493  degrees of freedom
#> AIC: 454.1
#> 
#> Number of Fisher Scoring iterations: 5
#> 
#> Transformed Parameter Estimates:
#>             Exp(B) CI.Lower CI.Upper
#> (Intercept)  0.000    0.000    0.000
#> iv           0.317    0.000 1946.771
#> mod          1.180    1.122    1.302
#> cov1         0.996    0.924    1.111
#> cat1gp2      0.810    0.419    1.653
#> cat1gp3      0.828    0.437    1.521
#> iv:mod       1.031    0.945    1.123
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

# Manually standardize iv and call glm()

data_test_mod_cat$iv_z <- scale(data_test_mod_cat[, "iv"])[, 1]

logistic_beta_x_manual <- glm(p ~ iv_z*mod + cov1 + cat1,
                              family = binomial,
                              data = data_test_mod_cat)

coef(logistic_beta_x)
#>   (Intercept)            iv           mod          cov1       cat1gp2 
#> -16.205361985  -1.148482578   0.165201690  -0.004369295  -0.210517691 
#>       cat1gp3        iv:mod 
#>  -0.189052500   0.030878129 
coef(logistic_beta_x_manual)
#>   (Intercept)          iv_z           mod          cov1       cat1gp2 
#> -16.205361985  -1.148482578   0.165201690  -0.004369295  -0.210517691 
#>       cat1gp3      iv_z:mod 
#>  -0.189052500   0.030878129 

# Standardize all numeric predictors

logistic_beta_allx <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
                                     family = binomial,
                                     data = data_test_mod_cat,
                                     to_standardize = c("iv", "mod", "cov1"))
# Note that cat1 is not standardized
summary(logistic_beta_allx)
#> Call to glm_betaselect():
#> betaselectr::lm_betaselect(formula = p ~ iv * mod + cov1 + cat1, 
#>     family = binomial, data = data_test_mod_cat, to_standardize = c("iv", 
#>         "mod", "cov1"), model_call = "glm")
#> 
#> Variable(s) standardized: iv, mod, cov1 
#> 
#> Call:
#> stats::glm(formula = p ~ iv * mod + cov1 + cat1, family = binomial, 
#>     data = betaselectr::std_data(data = data_test_mod_cat, to_standardize = c("iv", 
#>     "mod", "cov1")))
#> 
#> Coefficients:
#>             Estimate CI.Lower CI.Upper Std. Error z value Pr(Boot)    
#> (Intercept)    0.280   -0.290    0.862      0.256   1.093     0.18    
#> iv             1.941    1.626    2.430      0.202   9.603   <0.001 ***
#> mod            0.827    0.499    1.223      0.158   5.229   <0.001 ***
#> cov1          -0.013   -0.232    0.229      0.116  -0.113     0.98    
#> cat1gp2       -0.211   -0.963    0.454      0.311  -0.677     0.32    
#> cat1gp3       -0.189   -0.866    0.496      0.329  -0.575     0.40    
#> iv:mod         0.155   -0.224    0.701      0.210   0.736     0.32    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 692.86  on 499  degrees of freedom
#> Residual deviance: 440.10  on 493  degrees of freedom
#> AIC: 454.1
#> 
#> Number of Fisher Scoring iterations: 5
#> 
#> Transformed Parameter Estimates:
#>             Exp(B) CI.Lower CI.Upper
#> (Intercept)  1.323    0.748    2.373
#> iv           6.965    5.083   11.364
#> mod          2.287    1.648    3.399
#> cov1         0.987    0.793    1.257
#> cat1gp2      0.810    0.382    1.576
#> cat1gp3      0.828    0.421    1.643
#> iv:mod       1.167    0.800    2.019
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


summary(raw_output(lm_beta_x))
#> 
#> Call:
#> stats::lm(formula = dv ~ iv * mod + cov1 + cat1, data = data_test_mod_cat)
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
