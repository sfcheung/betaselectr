# betaselectr 0.1.1

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