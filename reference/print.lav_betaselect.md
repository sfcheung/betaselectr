# Print a 'lav_betaselect' Object

Print method for a 'lav_betaselect' object, which is the output of
[`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md).

## Usage

``` r
# S3 method for class 'lav_betaselect'
print(
  x,
  ...,
  nd = 3,
  output = c("lavaan.printer", "table"),
  standardized_only = TRUE,
  show_Bs.by = FALSE,
  by_group = TRUE,
  na_str = " ",
  sig_stars = TRUE,
  ci_sig = TRUE
)
```

## Arguments

- x:

  A `lav_betaselect`-class object, such as the output of
  [`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md).

- ...:

  Optional arguments to be passed to
  [`print()`](https://rdrr.io/r/base/print.html) methods.

- nd:

  The number of digits after the decimal place. Default is 3.

- output:

  String. How the results are printed. Default is `"lavaan.printer"`,
  and the results will be printed in a format similar to the printout of
  the output of the `summary`-method of a 'lavaan'-class object. If set
  to `"table"`, the results are printed in a table format similar to
  that of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  with `output` set to `"data.frame"`.

- standardized_only:

  Logical. If `TRUE`, the default, only the results for the standardized
  solution will be printed. If `FALSE`, then the standardized solution
  is printed alongside the unstandardized solution, as in the printout
  of the output of [`summary()`](https://rdrr.io/r/base/summary.html) of
  a 'lavaan'-class object.

- show_Bs.by:

  Logical. If `TRUE` and `output` is `"lavaan.printer"`, then the column
  `"Bs.by"` is shown, indicating, for each parameter, the variables
  standardized. This column is not shown if `output` is not
  `"lavaan.printer"`.

- by_group:

  If `TRUE`, the default, and the model has more than one group,
  sections will be grouped by groups first, as in the print out of
  [`summary()`](https://rdrr.io/r/base/summary.html) in `lavaan`. If
  `FALSE`, then the sections will be grouped by sections first.

- na_str:

  The string to be used for cells with `NA`. Default is `" "`, a
  whitespace.

- sig_stars:

  If `TRUE`, the default, symbols such as asterisks (`*`, `**`, `***`)
  will be used to denote whether a beta-select is significant.

- ci_sig:

  If `TRUE`, the default, a beta-select will be denoted as significant
  or not significant based on its confidence interval.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

The default format of the printout, `"lavaan.printer"`, is similar to
that of the [`summary()`](https://rdrr.io/r/base/summary.html) of a
`lavaan` object. Users can also select whether only the standardized
solution is printed or whether the standardized solution is appended to
the right of the printout.

If `output` is set to
`"table"' the format is that of [lavaan::parameterEstimates()] with `output
= "data.frame"\`, which is compact but not easy to read.

## See also

[`lav_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lav_betaselect.md).
This function is adapted from
`semhelpinghands::print.std_solution_boot()`.

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
print(fit_beta)
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
print(fit_beta, show_Bs.by = TRUE)
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
#>          BetaSelect Selected
#>  med ~                      
#>   iv          1.845       iv
#>   mod         0.325         
#>   iv:mod      0.797       iv
#>  dv ~                       
#>   med         0.049       dv
#>   iv          0.333    iv,dv
#> 
#> Covariances:
#>          BetaSelect Selected
#>  iv ~~                      
#>   mod         1.894       iv
#>   iv:mod      0.630       iv
#>  mod ~~                     
#>   iv:mod      5.733       iv
#> 
#> Variances:
#>          BetaSelect Selected
#>  .med        61.851         
#>  .dv          0.574       dv
#>   iv          1.000       iv
#>   mod        23.129         
#>   iv:mod     27.015       iv
#> 
#> Footnote:
#> - Variable(s) standardized: dv, iv
#> - Call 'print()' and set 'standardized_only' to 'FALSE' to print both
#>   original estimates and betas-select.
#> - The column 'Selected' lists variable(s) standardized when computing
#>   the standardized coefficient of a parameter. ('NA' for user-defined
#>   parameters because they are computed from other standardized
#>   parameters.)
#> - Product terms (iv:mod) have variables standardized before computing
#>   them. The product term(s) is/are not standardized.
print(fit_beta, output = "table")
#>       lhs op    rhs     est    se      z pvalue ci.lower ci.upper  std.p
#> 1     med  ~     iv   0.661 0.217  3.047  0.002    0.236    1.086  1.845
#> 2     med  ~    mod   0.325 0.128  2.535  0.011    0.074    0.577  0.325
#> 3     med  ~ iv:mod   0.286 0.039  7.248  0.000    0.208    0.363  0.797
#> 4      dv  ~    med   0.093 0.011  8.298  0.000    0.071    0.115  0.049
#> 5      dv  ~     iv   0.229 0.039  5.917  0.000    0.153    0.304  0.333
#> 6     med ~~    med  61.851 6.185 10.000  0.000   49.728   73.974 61.851
#> 7      dv ~~     dv   2.104 0.210 10.000  0.000    1.692    2.517  0.574
#> 8      iv ~~     iv   7.795 0.000     NA     NA    7.795    7.795  1.000
#> 9      iv ~~    mod   5.287 0.000     NA     NA    5.287    5.287  1.894
#> 10     iv ~~ iv:mod   4.908 0.000     NA     NA    4.908    4.908  0.630
#> 11    mod ~~    mod  23.129 0.000     NA     NA   23.129   23.129 23.129
#> 12    mod ~~ iv:mod  16.006 0.000     NA     NA   16.006   16.006  5.733
#> 13 iv:mod ~~ iv:mod 210.572 0.000     NA     NA  210.572  210.572 27.015
#>    std.p.by
#> 1        iv
#> 2          
#> 3        iv
#> 4        dv
#> 5     iv,dv
#> 6          
#> 7        dv
#> 8        iv
#> 9        iv
#> 10       iv
#> 11         
#> 12       iv
#> 13       iv
```
