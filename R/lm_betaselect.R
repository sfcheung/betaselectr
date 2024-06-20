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
                          std_se = c("none", "delta", "bootstrap"),
                          std_z = TRUE,
                          std_pvalue = TRUE,
                          std_ci = TRUE,
                          level = .95,
                          progress = TRUE,
                          boot_out = NULL,
                          bootstrap = 100L,
                          store_boot_est = TRUE,
                          parallel = c("no", "snow", "multicore"),
                          ncpus = parallel::detectCores(logical = FALSE) - 1,
                          cl = NULL,
                          iseed = NULL,
                          delta_method = c("lavaan", "numDeriv"),
                          vector_form = TRUE) {

    # TODO:
    # - Revise for lm-class object.
    # Workflow:
    # - Compute the variance-covariance matrix of variables
    # - Identify variables to be standardized
    # - Detect inline terms and warn users of them
    # - For each term, identify how its beta_select is to be computed
    # - Compute beta_select for each term
    # - Compute SE and/or CI, for retrieval.
    #   - Do the computation in other functions.

    # - Do regression on the unstandardized input variables.
    #   - The results are stored because some methods need them.
    #   - Compute and store the OLS or ML vcov.
    # - Do regression on standardized input variables.
    #   - Compute and store the OLS or ML vcov.
    # - Do bootstrapping if requested.
    #   - Compute and store the bootstrap estimate.
    # - Other SEs and CIs can be computed on request.

browser()
    if (!isTRUE(requireNamespace("pbapply", quietly = TRUE)) ||
        !interactive()) {
        progress <- FALSE
      }
    # Check whether the object is supported
    # lm_betaselect_check_fit(object)

    # output <- match.arg(output)
    parallel <- match.arg(parallel)
    delta_method <- match.arg(delta_method)
    std_se <- tolower(match.arg(std_se))
    has_se <- !identical("none", std_se)
    # ngroups <- lavaan::lavTech(object, what = "ngroups")

    # Get the variables to be standardized
    # prods <- find_all_products(object)
    if (is.null(to_standardize)) {
        to_standardize <- ".all."
      }
    to_standardize <- fix_to_standardize_lm(object = object,
                                            to_standardize = to_standardize,
                                            not_to_standardize = not_to_standardize,
                                            skip_categorical_x = skip_categorical_x,
                                            prods = prods)

    # Do standardization
    dat <- stats::model.frame(object)
    datz <- dat
    for (xx in to_standardize) {
        datz[, xx] <- scale(dat[, xx])[, 1]
      }
    objectz <- stats::update(object,
                             data = datz)

    # Standard errors

    # z statistic

    # p-values

    # Confidence intervals

    class(objectz) <- c("lm_betaselect", class(object))

    objectz$standardized_terms <- to_standardize
    objectz$lm_betaselect <- match.call()
    objectz$lm_call <- stats::getCall(object)
    objectz
  }

