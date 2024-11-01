#' @title Betas-Select in a 'lavaan'-Model
#'
#' @description Can standardize selected
#' variables in a `lavaan` model without
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
#' - It automatically skips predictors
#' which has only two unique values,
#' assuming that they are dummy variables.
#'
#' - It does not standardize product
#' term, which is incorrect. Instead,
#' it computes the product term with
#' its component variables standardized
#' first.
#'
#' - It can be used to generate bootstrap
#' confidence intervals for the
#' standardized solution (Falk, 2018). Bootstrap
#' confidence interval is better than
#' doing standardization *before* fitting
#' a model because it correctly takes
#' into account the sampling variance
#' of the standard deviations. It is
#' also better than delta-method
#' confidence interval because it takes
#' into account the usually asymmetric
#' distribution of parameters after
#' standardization, such as standardized
#' loadings and correlations.
#'
#' - For comparison, it can also report
#' delta-method standard errors and
#' confidence intervals if requested.
#'
#' ## Problems With Common Approaches
#'
#' In most SEM programs, users
#' have limited control on which
#' variables to standardize when
#' requesting the standardized solution.
#' The solution may be uninterpretable
#' or misleading in these conditions:
#'
#' - Dummy variables are standardized
#' and their coefficients cannot be interpreted as the
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
#' Moreover, the delta method is usually
#' used in standardization, which is suboptimal for
#' standardization unless the sample
#' size is large (Falk, 2018). For example, the
#' covariance with variables standardized
#' is a correlation, and its sampling
#' distribution is skewed unless its
#' population value is zero. However,
#' delta-method confidence interval
#' for the correlation is necessarily
#' symmetric around the point estimate.
#'
#' ## Limitations
#'
#' - It only supports observed variable
#' interaction terms, and only support
#' two-way interactions.
#'
#' - It does not support multilevel
#' models.
#'
#' - It only supports models fitted to
#' raw data.
#'
#' - Intercepts not supported.
#'
#' @return
#' A `lav_betaselect`-class object,
#' which is a data frame storing the parameter
#' estimates, similar in form to the
#' output of [lavaan::parameterEstimates()].
#'
#' @param object The output of
#' `lavaan` model fit functions, such
#' as [lavaan::sem()] and [lavaan::cfa()].
#'
#' @param to_standardize A string vector,
#' which should be the names of the
#' variables to be standardized.
#' Default is `".all."`, indicating all
#' variables are to be standardized
#' (but see `skip_categorical_x`).
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
#' variables with only two possible
#' values in the data analyzed, will
#' be skipped in standardization. This
#' overrides the argument
#' `to_standardize`. That is, a
#' categorical predictor will not be
#' standardized even if listed in
#' `to_standardize`, unless users set
#' this argument to `FALSE`.
#'
#' @param output The format of the
#' output. Not used because the format
#' of the printout is now controlled
#' by the `print`-method of the output
#' of this function. Kept for backward
#' compatibility.
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
#' to compute delta-method standard
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
#' @param find_product_terms String.
#' If it is certain that a model does
#' not have product terms, setting this
#' to `FALSE` will skip the search, which
#' is time consuming for a models with
#' many paths and/or many variables.
#' Default is `TRUE`, and the function
#' will automatically identify product
#' terms, if any.
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
#' Falk, C. F. (2018). Are robust standard errors the best approach
#' for interval estimation with nonnormal data in structural equation
#' modeling?
#' *Structural Equation Modeling: A Multidisciplinary Journal, 25*(2)
#'  244-266. \doi{10.1080/10705511.2017.1367254}

#'
#' @seealso [print.lav_betaselect()] for its print method.
#'
#' @examples
#'
#' library(lavaan)
#' mod <-
#' "
#' med ~ iv + mod + iv:mod
#' dv ~ med + iv
#' "
#' fit <- sem(mod,
#'            data_test_medmod,
#'            fixed.x = TRUE)
#' summary(fit)
#' fit_beta <- lav_betaselect(fit,
#'                            to_standardize = c("iv", "dv"))
#' fit_beta
#' print(fit_beta, standardized_only = FALSE)
#'
#' # In real studies:
#' # - should set bootstrap to at least 5000
#' # - should set parallel to "snow" or "multicore"
#' fit_beta_boot <- lav_betaselect(fit,
#'                                 to_standardize = c("iv", "dv"),
#'                                 std_se = "bootstrap",
#'                                 std_ci = TRUE,
#'                                 bootstrap = 100,
#'                                 iseed = 1234)
#' fit_beta_boot
#' print(fit_beta_boot, standardized_only = FALSE)
#'
#' # Print full results
#' print(fit_beta_boot,
#'       standardized_only = FALSE)
#'
#'
#' @export
#'

lav_betaselect <- function(object,
                           to_standardize = ".all.",
                           not_to_standardize = NULL,
                           skip_categorical_x = TRUE,
                           output = c("data.frame", "text"),
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
                           find_product_terms = TRUE,
                           ...,
                           delta_method = c("lavaan", "numDeriv"),
                           vector_form = TRUE) {
    if (!isTRUE(requireNamespace("pbapply", quietly = TRUE)) ||
        !interactive()) {
        progress <- FALSE
      }

    # Check whether the object is supported
    lav_betaselect_check_fit(object)

    output <- match.arg(output)
    parallel <- match.arg(parallel)
    delta_method <- match.arg(delta_method)
    std_se <- tolower(match.arg(std_se))
    has_se <- !identical("none", std_se)
    ngroups <- lavaan::lavTech(object, what = "ngroups")

    # Get the variables to be standardized
    if (find_product_terms) {
        prods <- find_all_products(object,
                                  parallel = (parallel != "no"),
                                  ncpus = ncpus,
                                  cl = cl,
                                  progress = progress)
      } else {
        prods <- list()
      }
    to_standardize <- fix_to_standardize(object = object,
                                         to_standardize = to_standardize,
                                         not_to_standardize = not_to_standardize,
                                         skip_categorical_x = skip_categorical_x,
                                         prods = prods)

    # Prepare the tables for the results
    ptable <- lavaan::parameterTable(object)
    est <- lavaan::parameterEstimates(object,
                                      ...,
                                      level = level,
                                      output = output)
    est[, "std.lv"] <- NULL
    est[, "std.nox"] <- NULL
    std <- lavaan::standardizedSolution(object,
                                        se = TRUE,
                                        zstat = TRUE,
                                        pvalue = TRUE,
                                        ci = TRUE,
                                        partable = ptable)
    if (is.null(std$group)) {
        std$group <- ptable$group
      }
    # Generate the function for each parameter with
    # a standardized solution.
    i <- which(!(std$op %in% c("~1", "==", ":=")))
    # If vector_form is TRUE:
    #   std_fct is a list of gen_std_i_internal() output
    std_fct <- gen_std(object = object,
                       i = i,
                       to_standardize = to_standardize,
                       prods = prods,
                       internal_only = vector_form)
    if (vector_form) {
        std_fct_v <- gen_std_vector(fit = object,
                                    i_vector = i,
                                    std_fct_vector = std_fct)
      } else {
        std_fct_v <- NULL
      }

    # Compute the standardized solution
    fit_est <- methods::getMethod("coef",
                  signature = "lavaan",
                  where = asNamespace("lavaan"))(object)
    fit_vcov <- lavaan::lavInspect(object, what = "vcov")
    if (vector_form) {
        est_std_full <- std_fct_v(fit_est)
      } else {
        est_std_full <- lapply(std_fct, function(xx) xx(fit_est))
      }
    est_std <- unlist(est_std_full)
    if (vector_form) {
        est_std_by <- attr(est_std_full,
                           which = "std_by",
                           exact = TRUE)
      } else {
        est_std_by <- lapply(est_std_full,
                            FUN = attr,
                            which = "std_by",
                            exact = TRUE)
      }
    est_std_by <- sapply(est_std_by,
                         function(x) {paste0(x, collapse = ",")})
    std[i, "std.p"] <- est_std
    std[i, "std.p.by"] <- est_std_by

    # User-parameters
    def.function <- object@Model@def.function
    has_def <- ":=" %in% ptable$op

    if (has_def) {
        std_def <- def_std(std = std,
                           ptable = ptable,
                           def.function = def.function)
        i_def <- match(names(std_def), std$label)
        std[i_def, "std.p"] <- std_def
      } else {
        std_def <- numeric(0)
        i_def <- numeric(0)
      }
    i0 <- c(i, i_def)

    # Standard errors
    if (has_se) {
        if ("bootstrap" %in% std_se) {
            boot_est <- std_boot(object = object,
                                 std_fct = std_fct,
                                 std_fct_v = std_fct_v,
                                 boot_out = boot_out,
                                 progress = progress,
                                 bootstrap = bootstrap,
                                 parallel = parallel,
                                 ncpus = ncpus,
                                 cl = cl,
                                 iseed = iseed)
            std_labels <- lavaan::lav_partable_labels(std)
            colnames(boot_est) <- std_labels[i]
            est_std_se <- std_se_boot_all(boot_est)
            if (has_def) {
                boot_est_user <- std_boot_user(std = std,
                                               ptable = ptable,
                                               i = i,
                                               def.function = def.function,
                                               boot_est = boot_est)
                est_std_user_se <- std_se_boot_all(boot_est_user)
              } else {
                boot_est_user <- NULL
                est_std_user_se <- numeric(0)
              }
          }
        if ("delta" %in% std_se) {
            est_std_se <- std_se_delta_all(std_fct = std_fct,
                                          std_fct_v = std_fct_v,
                                          fit_est = fit_est,
                                          fit_vcov = fit_vcov,
                                          method = delta_method,
                                          progress = progress)
            std_vcov <- attr(est_std_se,
                             which = "std_vcov",
                             exact = TRUE)
            if (has_def) {
                est_std_user_se <- std_se_delta_user(std = std,
                                                     ptable = ptable,
                                                     i = i,
                                                     def.function = def.function,
                                                     std_fct = std_fct,
                                                     std_fct_v = std_fct_v,
                                                     fit_vcov = fit_vcov,
                                                     std_vcov = std_vcov,
                                                     method = delta_method,
                                                     progress = progress)
              } else {
                est_std_user_se <- numeric(0)
              }
          }
        std[i, "std.p.se"] <- est_std_se
        if (has_def) {
            std[i_def, "std.p.se"] <- est_std_user_se
          }
      }

    # Fix 0 SE

    if (has_se) {
        i <- (std$std.p.se < sqrt(.Machine$double.eps))
        std[which(i), "std.p.se"] <- NA
      }


    # z statistic
    if (has_se && std_z) {
        # Same for delta and bootstrap
        est_std_z <- std[i0, "std.p"] / std[i0, "std.p.se"]
        est_std_z[std[i0, "std.p.se"] < sqrt(.Machine$double.eps)] <- NA
        std[i0, "std.p.z"] <- est_std_z
      }

    # p-values
    if (has_se && std_pvalue && std_z) {
        if ("bootstrap" %in% std_se) {
            est_pvalue <- std_pvalue_boot_all(cbind(boot_est, boot_est_user))
          }
        if ("delta" %in% std_se) {
            est_pvalue <- std_pvalue_delta_all(est_std_z)
          }
        std[i0, "std.p.pvalue"] <- est_pvalue
      }

    # Confidence intervals
    if (has_se && std_ci) {
        if ("bootstrap" %in% std_se) {
            ci <- std_ci_boot_all(x_est = c(est_std, std_def),
                                  x_est_boot = cbind(boot_est, boot_est_user),
                                  level = level)
          }
        if ("delta" %in% std_se) {
            ci <- std_ci_delta_all(x_est = c(est_std, std_def),
                                   x_se = c(est_std_se, est_std_user_se),
                                   level = level)
          }
        i1 <- (ci[, "ci.upper"] - ci[, "ci.lower"] < sqrt(.Machine$double.eps))
        ci[i1, "ci.lower"] <- NA
        ci[i1, "ci.upper"] <- NA
        std[i0, "std.p.ci.lower"] <- ci[, "ci.lower"]
        std[i0, "std.p.ci.upper"] <- ci[, "ci.upper"]
      }

    # Store results in the parameter estimates table
    if (is.null(est$group)) {
        est$group <- 1
        est[est$op %in% c("==", ":="), "group"] <- 0
      }
    std_names <- colnames(std)
    std_to_keep <- std_names[startsWith(std_names, "std.p")]
    est$est_id <- seq_len(nrow(est))
    out <- merge(est,
                 std[, c("lhs", "op", "rhs", "group", std_to_keep)],
                 all.x = TRUE,
                 all.y = FALSE,
                 sort = FALSE)
    out <- out[order(out$est_id), ]
    est[out$est_id, std_to_keep] <- out[, std_to_keep]
    est$est_id <- NULL
    if (ngroups == 1) {
        est$group <- NULL
      }
    class(est) <- c("lav_betaselect", class(est))
    attr(est, "call") <- match.call()
    if(store_boot_est && ("bootstrap" %in% std_se)) {
        attr(est, "boot_est") <- boot_est
      }

    # Adapted from semhelpinghands::standardizedSolution_boot_ci()
    fit_summary <- lavaan::summary(object)
    attr(est, "pe_attrib") <- attributes(fit_summary$pe)
    attr(est, "partable") <- lavaan::parameterTable(object)
    attr(est, "est") <- lavaan::parameterEstimates(object)
    attr(est, "level") <- level
    attr(est, "std_se") <- std_se
    attr(est, "R") <- ifelse("bootstrap" %in% std_se,
                             nrow(boot_est),
                             NA)
    attr(est, "prods") <- prods
    attr(est, "categorical") <- find_categorical(object)
    est
  }

