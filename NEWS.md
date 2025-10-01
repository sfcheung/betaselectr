# betaselectr 0.1.2.2

## New Features

- `lav_betaselect()` can also compute
  the "standardized" intercepts, though
  in the same way `lavaan` does, by
  dividing an intercept by the
  standard deviation of the outcome
  variable (the `y` variable). This
  can be enabled by setting
  `std_intercept` to `TRUE` (`FALSE`
  by default). (0.1.2.1)

## Improvement

- `lav_betaselect()` will no longer
  check whether variables involved in
  a product term have been mean-centered
  by default (but can still be enabled
  if necessary).
  The coefficient of the so-called
  "main effect" term of these variables
  will now be computed correctly even
  without mean-centering. (0.1.2.1, 0.1.2.2)

## Miscellaneous

- `lav_betaselect()` will not compute
  the coefficients of covariances and
  variances that involve a product term,
  if the variables involved are not
  mean-centered. Without mean-centering,
  it is impossible to compute them if
  the joint distribution of the variables
  is not multivariate normal. (0.1.2.1)

# betaselectr 0.1.2

## Miscellaneous

- Added a few more tests for one-IV
  models. (0.1.2)

## Bug Fixes

- `lav_betaselect()`: The standardized
  coefficients of the component
  variables of a product term are
  incorrect if mean-centering is not
  done. For now, `lav_betaselect()` will
  check whether they are mena-centered.
  If not, it will raise an error and
  suggest users to mean-center the
  involved variables first. (0.1.1)

# betaselectr 0.1.0

- First version to CRAN.
