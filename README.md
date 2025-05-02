
<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/betaselectr.svg)](https://github.com/sfcheung/betaselectr)
[![Last Commit at Main](https://img.shields.io/github/last-commit/sfcheung/betaselectr.svg)](https://github.com/sfcheung/betaselectr/commits/main)
[![R-CMD-check](https://github.com/sfcheung/betaselectr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/betaselectr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# betaselectr: Do Selective Standardization in Structural Equation Models and Regression Models

(Version 0.1.2, updated on 2025-05-02, [release history](https://sfcheung.github.io/betaselectr/news/index.html))

It computes *beta*s-select, coefficients
(*beta*s) after standardization
in structural equation models and
regression models with only
*select*ed variables standardized. It
supports models with moderation, with
product terms formed appropriately
(formed *after* standardization). It can also form
confidence intervals that takes into
account the standardization
appropriately.

For more information on this package,
please visit its GitHub page:

https://sfcheung.github.io/betaselectr/

# Installation

The latest developmental version of this
package can be installed by `remotes::install_github`:

```r
remotes::install_github("sfcheung/betaselectr")
```

# Issues

If you have any suggestions and found
any bugs, please feel feel to open a
GitHub issue. Thanks.

https://github.com/sfcheung/betaselectr/issues