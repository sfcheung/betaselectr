# betaselectr 0.0.1.5

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