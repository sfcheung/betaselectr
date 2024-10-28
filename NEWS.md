# betaselectr 0.0.1.10

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
