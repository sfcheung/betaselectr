# Call in an 'lm_betaselect' or 'glm_betaselect' Object

The `getCall`-method for an `lm_betaselect`-class or
`glm_betaselectd`-class objects.

## Usage

``` r
# S3 method for class 'lm_betaselect'
getCall(
  x,
  what = c("lm_betaselect", "beta", "standardized", "raw", "unstandardized"),
  ...
)

# S3 method for class 'glm_betaselect'
getCall(
  x,
  what = c("glm_betaselect", "beta", "standardized", "raw", "unstandardized"),
  ...
)
```

## Arguments

- x:

  An `lm_betaselect`-class or `glm_betaselect`-class object from which
  the call is to be extracted.

- what:

  Which call to extract. For `"lm_betaselect"` or `"glm_betaselect"` the
  call to
  [`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  or
  [`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  is extracted. For `"beta"` or `"standardized"`, the call used to fit
  the model *after* selected variables standardized is extracted. For
  `"raw"` or `"unstandardized"`, the call used to fit hte model *before*
  standardization is extracted.

- ...:

  Additional arguments. Ignored.

## Value

It returns the call requested.

## Details

This works in the same way the default `getCall`-method does for the
outputs of [`stats::lm()`](https://rdrr.io/r/stats/lm.html) and
[`stats::glm()`](https://rdrr.io/r/stats/glm.html).

## See also

[`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md),
[`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md),
and [`stats::getCall()`](https://rdrr.io/r/stats/update.html)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

data(data_test_mod_cat)

lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1,
                           data = data_test_mod_cat,
                           to_standardize = "iv")
getCall(lm_beta_x)
#> betaselectr::lm_betaselect(formula = dv ~ iv * mod + cov1, data = data_test_mod_cat, 
#>     to_standardize = "iv")
getCall(lm_beta_x, what = "beta")
#> stats::lm(formula = dv ~ iv * mod + cov1, data = betaselectr::std_data(data = data_test_mod_cat, 
#>     to_standardize = "iv"))
getCall(lm_beta_x, what = "raw")
#> stats::lm(formula = dv ~ iv * mod + cov1, data = data_test_mod_cat)
```
