
<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN status](https://www.r-pkg.org/badges/version/betaselectr?color=blue)](https://CRAN.R-project.org/package=betaselectr)
[![CRAN: Release Date](https://www.r-pkg.org/badges/last-release/betaselectr?color=blue)](https://cran.r-project.org/package=betaselectr)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/betaselectr?color=blue)](https://r-pkg.org/pkg/betaselectr)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/betaselectr.svg)](https://github.com/sfcheung/betaselectr)
[![Last Commit at Main](https://img.shields.io/github/last-commit/sfcheung/betaselectr.svg)](https://github.com/sfcheung/betaselectr/commits/main)
[![R-CMD-check](https://github.com/sfcheung/betaselectr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/betaselectr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# betaselectr: Do Selective Standardization in Structural Equation Models and Regression Models <img src="man/figures/logo.png" align="right" height="150" />

(Version 0.2.1, updated on 2026-06-10, [release history](https://sfcheung.github.io/betaselectr/news/index.html))

It computes *beta*s-select, coefficients
(*beta*s) after standardization
in structural equation models and
regression models with only
*select*ed variables standardized. It
supports models with moderation, with
product terms formed appropriately
(formed *after* standardization). It can also form
confidence intervals that take into
account the standardization
appropriately.

An
introduction to the package can be found
in the following article:

Sun, R. wei, Chang, F., Yang, W.,
Cheung, S. F., & Cheung, S.-H. (2026).
`betaselectr`: Selective (and proper)
standardization in structural equation
models. *Multivariate Behavioral Research*.
Advance online publication.
https://doi.org/10.1080/00273171.2026.2672692

For more information on this package,
please visit its GitHub page:

https://sfcheung.github.io/betaselectr/

# Installation

The stable CRAN version can be installed by `install.packages()`:

```r
install.packages("betaselectr")
```

The latest developmental version of this
package can be installed by `pak::pkg_install`
(please install `pak` first):

```r
pak::pkg_install("sfcheung/betaselectr")
```

# Issues

If you have any suggestions and found
any bugs, please feel free to open a
GitHub issue. Thanks.

https://github.com/sfcheung/betaselectr/issues