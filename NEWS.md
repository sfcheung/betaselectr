# betaselectr 0.0.1.18

- Added `lm_betaselect()` and related
  methods and helper functions.
  (0.0.1.1)

- Added parallel processing support to
  the internal function
  `find_all_products()`. (0.0.1.2)

- For `lav_betaselect()`, added an
  option to skip finding product
  terms. (0.0.1.2)

- Added some examples for the main
  functions. (0.0.1.3)

- Added `glm_betaselect()` and
  related methods to
  support models fitted by
  `stats::glm()`. (0.0.1.4)

- Set the column "Bs.by" to hidden
  by default when the output of
  `lav_betaselect()` is printed with
  `output` set to `"text"`. Can be
  displayed with `"show_Bs.by"` set
  to `TRUE`. (0.0.1.5)

- Updated to use `lavaan.printer`
  to print the output. The default
  format of the print out is changed
  to the `lavaan`-style format. (0.0.1.6)

- For the `print`-method of `lav_betaselect()`,
  symbols can be added to denote
  significant results by p-values
  or by confidence intervals. Controlled
  by the argument `sig_stars` and
  `ci_sig` in the `print`-method. (0.0.1.6)

- The `print`-method of `lav_betaselect()`
  now print only the standardized solution
  by default (0.0.1.6)

- Added a draft vignette for
  `lav_betaselect()`. (0.0.1.7)

- Set the default value of `load_balancing`
  to `FALSE` in `lm_betaselect()`. Also
  fix the case of `progress = FALSE`,
  `parallel = TRUE`, and `load_balancing = FALSE`.
  Load balancing is no longer used in this
  case. (0.0.1.8)

- Added `coef.lav_betaselect()`. (0.0.1.9)

- Added `confint.lav_betaselect()`. (0.0.1.10)

- Updated `summary.lm_betaselect()`
  to allow including the estimates
  before standardization. (0.0.1.11)

- Updated `print.summary.lm_betaselect()`
  to print confidence intervals by
  default, if available. (0.0.1.11)

- Fixed a bug in printing the bootstrap
  *p*-values for `summary.lm_betaselect()`.
  (0.0.1.13)

- Updated `summary.glm_betaselect()`
  to allow including the estimates
  before standardization. (0.0.1.13)

- Updated `print.summary.glm_betaselect()`
  to print confidence intervals by
  default, if available. (0.0.1.13)

- Added `skip_response` to
  `lm_betaselect()` and `glm_betaselect()`
  as a convenient way to skip standardizing
  the response variables. (0.0.1.15)

- Added `transform_b` to `summary.glm_betaselect()`
  for transforming
  the coefficients and their confidence
  limits. Intended to compute exponentiated
  coefficients but can be used for other
  purposes. (0.0.1.16)

- Removed some warnings in the methods
  for `lm_betaselect()` and `glm_betaselect()`.
  Not necessary because the notes in
  the print method can already alert the
  users. (0.0.1.16)

- Updated the vignettes and the `pkgdown`
  site. (0.0.1.17)

- Proofread the documentation and
  update meta-data. (0.0.1.18)