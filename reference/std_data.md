# Standardize Selected Variables

Standardize selected variables in a data frame or similar object.

## Usage

``` r
std_data(data, to_standardize)
```

## Arguments

- data:

  A data frame or similar object.

- to_standardize:

  A character vector of the column names of variables to be
  standardized.

## Value

A data frame similar to `data`, with selected variables standardized.

## Details

This is a helper functions to be used by
[`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
and
[`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md).
It assumes that the variables selected has been checked whether they are
numeric.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
data(data_test_mod_cat)
dat <- data_test_mod_cat
dat <- std_data(dat, to_standardize = c("iv", "dv"))
colMeans(dat[, c("dv", "iv")])
#>           dv           iv 
#> 2.078719e-16 4.026692e-16 
apply(dat[, c("dv", "iv")], 2, sd)
#> dv iv 
#>  1  1 
```
