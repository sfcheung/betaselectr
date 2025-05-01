# lav_betaselect()

## Preparation

- Find all product terms:

  - `find_product_terms()`

- Identify terms to standardize:

  - `to_standardize()`

- Generate the list of functions to
  compute the standardized estimates:

  - Get the initial list of functions by `gen_std()`

  - Convert it to one function by `gen_std_vector()`:

    - Output: `std_fct_v` (if `vector_form`)

## Computation

- Compute the standardized estimates by
  calling `std_fct_v` (if `vector_form`).

- User-defined parameters are recomputed
  from the standardized estimates.