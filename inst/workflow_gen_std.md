# gen_std()

Loop over the rows to standardize
and call `gen_std_i()` with
`internal_only = FALSE`.

# gen_std_i()

- Call `gen_std_i_internal()`
  to find the terms to standardize.
  Necessary for product terms. E.g.,

  - `list(to_standardize_i = c("med", "iv"), prod_names = "iv_mod")`

- Call `gen_std_i_internal()` to
  generate the function to compute
  the standardized estimates.

  - The output, `std_i_internal`, is
    a function doing the computation.

# gen_std_i_internal()`

