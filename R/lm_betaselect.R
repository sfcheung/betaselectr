# WIP

#' @title Standardize Coefficients in a Regression Model
#'
#' @description Can standardize selected
#' variables in a regression model without
#' refitting the models, can handle
#' product term correctly and skip
#' categorical predictors in
#' standardization.
#'
#' @details This function lets users
#' select which variables to be standardized
#' when computing the standardized
#' solution. It has the following
#' features:
#'
#' - It automatically skips categorical
#' predictors (i.e., factor or string
#' variables).
#'
#' - It does not standardize product
#' term, which is incorrect. Instead,
#' it compute the product term with
#' its component variables standardized.
#'
#' - It can be used to generate bootstrap
#' confidence intervals for the
#' standardized solution. Bootstrap
#' confidence interval is better than
#' doing standardization *before* fitting
#' a model because it correctly takes
#' into account the sampling variance
#' of the standard deviations. It is
#' also better than delta method
#' confidence interval because it takes
#' into account the usually asymmetric
#' distribution of parameters after
#' standardization.
#'
#' - For comparison, it can also report
#' delta method standard errors and
#' confidence intervals.
#'
#' ## Problems With Common Approaches
#'
#' In some regression programs, users
#' have limited control on which
#' variables to standardize when
#' requesting the so-called "betas".
#' The solution may be uninterpretable
#' or misleading in these conditions:
#'
#' - Dummy variables are standardized
#' and can not be interpreted as the
#' difference between two groups on the
#' outcome variables.
#'
#' - Product terms (interaction terms)
#' are standardized and they cannot be
#' interpreted as the changes in the
#' effects of focal variables when the
#' moderators change (Cheung, Cheung,
#' Lau, Hui, & Vong, 2022).
#'
#' - Variables with meaningful units can
#' be more difficult to interpret when
#' they are standardized (e.g., age).
#'
#' @return
#' A data frame storing the parameter
#' estimates, with `vcov`, `coef` and
#' `confint` methods. (TODO)
#'
#' @param object The output of
#' a regression model fitted by [lm()].
#'
#' @param to_standardize A string vector,
#' which should be the names of the
#' variables to be standardized.
#' Default is `NULL`, indicating all
#' variables are to be standardized
#' (but see `skip_categorical`).
#'
#' @param not_to_standardize A string
#' vector, which should be the names
#' of the variables that should not be
#' standardized. This argument is useful
#' when most variables, except for a few,
#' are to be standardized. This argument
#' cannot be ued with `to_standardize`
#' at the same time. Default is `NULL`,
#' and only `to_standardize` is used.
#'
#' @param skip_categorical_x Logical.
#' If `TRUE`, the default, all
#' categorical predictors, defined as
#' variables recognized as factors
#' by [lm()], will
#' be skipped in standardization. This
#' overrides the argument
#' `to_standardize`. That is, a
#' categorical predictor will not be
#' standardized even if listed in
#' `to_standardize`, unless uses set
#' this argument to `FALSE`.
#'
#' @param std_se String. If set to `"none"`,
#' the default, standard errors will not
#' be computed for the standardized
#' solution. If set to `"delta"`,
#' delta method will be used to compute
#' the standard errors. If set to
#' `"bootstrap"`, then what it does
#' depends whether `boot_out` is set.
#' If `boot_out` is to an output of
#' [manymome::do_boot()], its content
#' will be used. If `boot_out` is
#' `NULL` *and* bootstrap
#' estimates are available in `object`
#' (e.g., bootstrapping is requested
#' when fitting the model in `lavaan`),
#' then the stored bootstrap estimates
#' will be sued. If not available,
#' the bootstrapping will be conducted
#' using [lavaan::bootstrapLavaan()],
#' using arguments `bootstrap`,
#' `parallel`, `ncpus`, `cl`, and
#' `iseed`.`
#'
#' @param std_z Logical. If `TRUE` and
#' `std_se` is not set to `"none"`,
#' standard error will be computed
#' using the method specified in
#' `std_se`. Default is `TRUE`.
#'
#' @param std_pvalue Logical. If `TRUE`,
#' `std_se` is not set to `"none"`,
#' and `std_z` is `TRUE`, *p*-values
#' will be computed using the method
#' specified in `std_se`. For
#' bootstrapping, the method proposed by
#' Asparouhov and Muthén (2021) is used.
#' Default is `TRUE`.
#'
#' @param std_ci Logical. If `TRUE` and
#' `std_se` is not set to `"none"`,
#' confidence intervals will be
#' computed using the method specified in
#' `std_se`. Default is `FALSE.`
#'
#' @param level The level of confidence
#' of the confidence intervals. Default
#' is .95. It will be used in the
#' confidence intervals of both
#' the unstandardized and
#' standardized solution.
#'
#' @param ... Optional arguments to be
#' passed to the [lavaan::parameterEstimates()],
#' which will be use to generate the
#' output.
#'
#' @param delta_method The method used
#' to compute delta method standard
#' errors. For internal use and should
#' not be changed.
#'
#' @param vector_form The internal
#' method used to compute standardized
#' solution. For internal use and should
#' not be changed.
#'
#' @param progress Logical. If `TRUE`,
#' progress bars will be displayed
#' for long process.
#'
#' @param boot_out If `std_se` is
#' `"bootstrap"` and this argument
#' is set to an output of
#' [manymome::do_boot()], its output
#' will be used in computing statistics
#' such as standard errors and
#' confidence intervals. This allows
#' users to use methods other than
#' bootstrapping when fitting the
#' model, while they can still request
#' bootstrapping for the standardized
#' solution.
#'
#' @param bootstrap If `std_se` is
#' `"bootstrap"` but bootstrapping is
#' not requested when fitting the model
#' and `boot_out` is not set,
#' [lavaan::bootstrapLavaan()] will be
#' called to do bootstrapping. This
#' argument is the number of bootstrap
#' samples to draw. Default is 100.
#' Should be set to 5000 or even 10000
#' for stable results.
#'
#' @param parallel If `std_se` is
#' `"bootstrap"` but bootstrapping is
#' not requested when fitting the model
#' and `boot_out` is not set,
#' [lavaan::bootstrapLavaan()] will be
#' called to do bootstrapping. This
#' argument is to be passed to
#' [lavaan::bootstrapLavaan()]. Default
#' is `"no"`.
#'
#' @param ncpus If `std_se` is
#' `"bootstrap"` but bootstrapping is
#' not requested when fitting the model
#' and `boot_out` is not set,
#' [lavaan::bootstrapLavaan()] will be
#' called to do bootstrapping. This
#' argument is to be passed to
#' [lavaan::bootstrapLavaan()]. Default
#' is `parallel::detectCores(logical = FALSE) - 1`.
#' Ignored if `parallel` is `"no"`.
#'
#' @param cl If `std_se` is
#' `"bootstrap"` but bootstrapping is
#' not requested when fitting the model
#' and `boot_out` is not set,
#' [lavaan::bootstrapLavaan()] will be
#' called to do bootstrapping. This
#' argument is to be passed to
#' [lavaan::bootstrapLavaan()]. Default
#' is `NULL`.
#' Ignored if `parallel` is `"no"`.
#'
#' @param iseed If `std_se` is
#' `"bootstrap"` but bootstrapping is
#' not requested when fitting the model
#' and `boot_out` is not set,
#' [lavaan::bootstrapLavaan()] will be
#' called to do bootstrapping. This
#' argument is to be passed to
#' [lavaan::bootstrapLavaan()] to set
#' the seed for the random resampling.
#' Default
#' is `NULL`. Should be set to an integer
#' for reproducible results.
#' Ignored if `parallel` is `"no"`.
#'
#' @param store_boot_est Logical. If
#' `std_se` is `"bootstrap"` and this
#' argument is `TRUE`, the default,
#' the bootstrap estimates of the
#' standardized solution will be stored
#' in the attribute `"boot_est"`. These
#' estimates can be used for
#' diagnosis of the bootstrapping. If
#' `FALSE`, then the bootstrap estimates
#' will not be stored.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
#' Retrieved from https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
#'
#' Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
#' (2022) Improving an old way to measure moderation effect in standardized
#' units. *Health Psychology*, *41*(7), 502-505.
#' \doi{10.1037/hea0001188}
#'
#' @seealso [print.lav_betaselect()] for its print method.
#'
#' @examples
#' \donttest{
#' # TO ADD
#' }
#'
#' @noRd
#'

lm_betaselect <- function(...,
                          to_standardize = NULL,
                          not_to_standardize = NULL,
                          skip_categorical_x = TRUE,
                          do_boot = TRUE,
                          bootstrap = 100L,
                          iseed = NULL,
                          parallel = FALSE,
                          ncpus = parallel::detectCores(logical = FALSE) - 1,
                          progress = TRUE,
                          load_balancing = TRUE) {

    # Workflow
    # - Do regression on the unstandardized input variables.
    #   - The results are stored because some methods need them.
    #   - Compute and store the OLS or ML vcov.
    # - Do regression on standardized input variables.
    #   - Compute and store the OLS or ML vcov.
    # - Do bootstrapping if requested.
    #   - Compute and store the bootstrap estimate.
    # - Other SEs and CIs can be computed on request.

    # Do regression on the unstandardized input variables.
    my_call <- match.call()
    lm_args <- as.list(my_call)[-1]
    my_formals <- names(formals())
    lm_args[my_formals] <- NULL
    lm_call <- as.call(c(str2lang("stats::lm"), lm_args))
    lm_ustd <- eval(lm_call,
                    envir = parent.frame())
    vcov_ustd <- stats::vcov(lm_ustd)
    lm_ustd_call <- stats::getCall(lm_ustd)
    lm_ustd_args <- as.list(lm_ustd_call)[-1]

    # Do regression on standardized input variables.

    # Get the variables to be standardized
    if (is.null(to_standardize)) {
        to_standardize <- ".all."
      }
    input_data <- tryCatch(eval(lm_ustd_call$data,
                                envir = parent.frame()))
    if (inherits(input_data, "error")) {
        input_data <- stats::model.frame(lm_ustd)
      }
    to_standardize <- fix_to_standardize_lm_data(
                        object = lm_ustd,
                        input_data = input_data,
                        to_standardize = to_standardize,
                        not_to_standardize = not_to_standardize,
                        skip_categorical_x = skip_categorical_x
                      )
    # Do standardization
    input_data_z <- input_data
    for (xx in to_standardize) {
        input_data_z[, xx] <- scale(input_data_z[, xx])[, 1]
      }
    lm_std_call <- lm_ustd_call
    data_call <- lm_std_call$data
    if (is.null(data_call)) {
        lm_std_call$data <- std_data(input_data,
                                    to_standardize = to_standardize)
      } else {
        tmp <- call("std_data",
                    data = data_call,
                    to_standardize = to_standardize)
        lm_std_call$data <- tmp
      }
    lm_std <- eval(lm_std_call,
                   envir = parent.frame())
    vcov_std <- stats::vcov(lm_std)

    # Prepare output

    out <- lm_std
    out$lm_betaselect$ustd <- lm_ustd
    out$lm_betaselect$vcov_std <- vcov_std
    out$lm_betaselect$vcov_ustd <- vcov_ustd
    out$lm_betaselect$call <- match.call()

    # Do bootstrapping if requested.

    if (do_boot) {
        boot_out <- lm_boot(lm_args = lm_ustd_args,
                            input_data = input_data,
                            to_standardize = to_standardize,
                            parallel = parallel,
                            iseed = iseed,
                            bootstrap = bootstrap,
                            ncpus = ncpus,
                            progress = progress,
                            load_balancing = load_balancing)
      } else {
        boot_out <- NULL
      }
    out$lm_betaselect$boot_out <- boot_out

    class(out) <- c("lm_betaselect", class(out))

    out
  }

#' @noRd
# Should be exported when ready

std_data <- function(data,
                     to_standardize) {
    for (xx in to_standardize) {
        data[, xx] <- scale(data[, xx])[, 1]
      }
    data
  }

#' @noRd

lm_boot <- function(lm_args,
                    input_data,
                    to_standardize,
                    parallel,
                    iseed,
                    bootstrap,
                    ncpus,
                    progress,
                    load_balancing) {

    if (!is.null(iseed)) set.seed(iseed)

    n <- nrow(input_data)
    boot_idx <- replicate(bootstrap,
                          sample(n, n, replace = TRUE),
                          simplify = FALSE)

    lm_args_i <- lapply(lm_args,
                        eval,
                        envir = parent.frame())

    tmpfct <- function(i) {
                  force(std_data)
                  data_i <- input_data[i, ]
                  input_data_z <- std_data(data_i,
                                            to_standardize = to_standardize)
                  lm_args_i$data <- data_i
                  ustd_i <- do.call(stats::lm,
                                    lm_args_i)
                  lm_args_i$data <- input_data_z
                  std_i <- do.call(stats::lm,
                                    lm_args_i)
                  list(coef_ustd = stats::coef(ustd_i),
                      vcov_ustd = stats::vcov(ustd_i),
                      coef_std = stats::coef(std_i),
                      vcov_std = stats::vcov(std_i))
                }

    if (parallel) {
        if (load_balancing && progress) {
            pbopt_old <- pbapply::pboptions(use_lb = TRUE)
            on.exit(pbapply::pboptions(pbopt_old), add = TRUE)
          }
        my_cl <- parallel::makeCluster(ncpus)
        # TODO:
        # - Change to pkg::std_data() later
        parallel::clusterExport(cl = my_cl, "std_data")
        on.exit(parallel::stopCluster(my_cl), add = TRUE)
        if (progress) {
            boot_out <- pbapply::pblapply(
                X = boot_idx,
                FUN = tmpfct,
                cl = my_cl
              )
          } else {
            boot_out <- parallel::parLapplyLB(
                cl = my_cl,
                X = boot_idx,
                fun = tmpfct,
                chunk.size = 1
              )
          }
      } else {
        if (progress) {
            boot_out <- pbapply::pblapply(
                X = boot_idx,
                FUN = tmpfct
              )
          } else {
            boot_out <- lapply(
                X = boot_idx,
                FUN = tmpfct
              )
          }
      }
    return(boot_out)
  }