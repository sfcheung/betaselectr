# betaselectr: Do Selective Standardization in Structural Equation Models and Regression Models

(Version 0.2.1, updated on 2026-06-10, [release
history](https://sfcheung.github.io/betaselectr/news/index.html))

It computes *beta*s-select, coefficients (*beta*s) after standardization
in structural equation models and regression models with only *select*ed
variables standardized. It supports models with moderation, with product
terms formed appropriately (formed *after* standardization). It can also
form confidence intervals that take into account the standardization
appropriately.

An introduction to the package can be found in the following article:

Sun, R. wei, Chang, F., Yang, W., Cheung, S. F., & Cheung, S.-H. (2026).
`betaselectr`: Selective (and proper) standardization in structural
equation models. *Multivariate Behavioral Research*. Advance online
publication. <https://doi.org/10.1080/00273171.2026.2672692>

For more information on this package, please visit its GitHub page:

<https://sfcheung.github.io/betaselectr/>

# Installation

The stable CRAN version can be installed by
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html):

``` r

install.packages("betaselectr")
```

The latest developmental version of this package can be installed by
[`pak::pkg_install`](https://pak.r-lib.org/reference/pkg_install.html)
(please install `pak` first):

``` r

pak::pkg_install("sfcheung/betaselectr")
```

# Issues

If you have any suggestions and found any bugs, please feel free to open
a GitHub issue. Thanks.

<https://github.com/sfcheung/betaselectr/issues>
