#' @title Standardize Coefficients in a
#' Regression Model
#'
#' @description Can fit a linear regression
#' models with selected variables standardized;
#' handle product terms correctly and
#' skip categorical predictors in
#' standardization.
#'
#' @details The function [lm_betaselect()]
#' lets users
#' select which variables to be
#' standardized when computing the
#' standardized solution. It has the
#' following features:
#'
#' - It automatically skips categorical
#' predictors (i.e., factor or string
#' variables).
#'
#' - It does not standardize product
#' term, which is incorrect. Instead, it
#' compute the product term with its
#' component variables standardized.
#'
#' - It standardizes the selected
#' variables *before* fitting a model.
#' Therefore, If a model has the term
#' `log(x)` and `x` is one of the
#' selected variables, the model used
#' the logarithm of the *standardized*
#' `x` in the model, instead of
#' standardized `log(x)` which is
#' difficult to interpret.
#'
#' - It can be used to generate
#' nonparametric
#' bootstrap confidence intervals for
#' the standardized solution. Bootstrap
#' confidence interval is better than
#' the least square confidence interval
#' ignoring the standardization
#' because it
#' takes into account the sampling
#' variance of the standard deviations.
#' It is one of the recommended methods
#' for forming confidence intervals for
#' coefficients involving standardized
#' variables (Jones & Waller, 2013).
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
#' and cannot be interpreted as the
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
#' ## How It Works
#'
#' It standardizes the original variables
#' *before* they are used in the
#' regression model. Therefore, strictly
#' speaking, it does not standardize
#' the predictors in a regression model,
#' but standardize the *input variable*
#' (Gelman et al., 2021).
#'
#' The requested model is then fitted to
#' the dataset with selected variables
#' standardized. For the ease of
#' follow-up analysis, both the results
#' with selected variables standardized
#' and the results without
#' standardization are stored. If
#' required, the results without
#' standardization can be retrieved
#' by [raw_output()].
#'
#' ## Methods
#'
#' The output of [lm_betaselect()] is
#' an `lm_betaselect`-class object.
#' It has the following methods:add
#'
#' - A `coef`-method for extracting
#' the coefficients of the model.
#' (See [coef.lm_betaselect()] for details.)
#'
#' - A `vcov`-method for extracting the
#' variance-covariance matrix of the
#' estimates of the coefficients.
#' If bootstrapping is requested, it
#' can return the matrix based on the
#' bootstrapping estimates.
#' (See [vcov.lm_betaselect()] for details.)
#'
#' - A `confint`-method for forming the
#' confidence intervals of the
#' estimates of the coefficients.
#' If bootstrapping is requested, it
#' can return the bootstrap confidence
#' intervals.
#' (See [confint.lm_betaselect()] for details.)
#'
#' - A `summary`-method for printing the
#' summary of the results, with additional
#' information such as the number of
#' bootstrap samples and which variables
#' have been standardized.
#' (See [summary.lm_betaselect()] for details.)
#'
#' - An `anova`-method for printing the
#' ANOVA table. Can also be used to
#' compare two or more outputs of
#' [lm_betaselect()].
#' (See [anova.lm_betaselect()] for details.)
#'
#' - A `predict`-method for computing
#' predicted values. It can be used to
#' compute the predicted values given
#' a set of new unstandardized data.
#' The data will be standardized before
#' computing the predicted values in
#' the models with standardization.
#' (See [predict.lm_betaselect()] for details.)
#'
#' - The default `update`-method for updating
#' a call also works for an
#' `lm_betaselect` object. It can
#' update the model in the same
#' way it updates a model fitted by
#' [stats::lm()], and also update
#' arguments of [lm_betaselect()],
#' such as the variables to be
#' standardized.
#' (See [stats::update()] for details.)
#'
#' Most other methods for the output
#' of [stats::lm()] should also work
#' on an `lm_betaselect`-class object.
#' Some of them will give the same
#' results regardless of the variables
#' standardized. For example,
#' [rstandard()] and [cooks.distance()].
#' For some others, they should be used
#' with cautions if they make use of
#' the variance-covariance matrix
#' of the estimates because they may use
#' the least square version.
#'
#' To use the methods for `lm` objects
#' on the results without standardization,
#' simply use [raw_output()]. For example,
#' to get the fitted values without
#' standardization, call
#' `fitted(raw_output(x))`, where `x`
#' is the output of [lm_betaselect()].
#'
#' @return
#' The function [lm_betaselect()]
#' returns an object of the class `lm_betaselect`,
#' which is similar to the output
#' of [lm()], with additional information
#' stored.
#'
#' @param ... For [lm_betaselect()],
#' these arguments will be
#' passed directly to [lm()]. For
#' the `print`-method of `lm_betaselect`
#' objects, this will be passed to
#' other methods.
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
#' of the variables that should *not* be
#' standardized. This argument is useful
#' when most variables, except for a few,
#' are to be standardized. This argument
#' cannot be ued with `to_standardize`
#' at the same time. Default is `NULL`,
#' and only `to_standardize` is used.
#'
#' @param do_boot Whether bootstrapping
#' will be conducted. Default is `TRUE`.
#'
#' @param bootstrap If `do_boot` is
#' `TRUE`, this argument is the number
#' of bootstrap samples to draw. Default
#' is 100. Should be set to 5000 or even
#' 10000 for stable results.
#'
#' @param iseed If `do_boot` is `TRUE`
#' and this argument is not `NULL`,
#' it will be used by [set.seed()] to
#' set the seed for the random number
#' generator. Default is `NULL`.
#'
#' @param parallel If `do_boot` is
#' `TRUE` and this argument is `TRUE`,
#' parallel processing will be used to
#' do bootstrapping. Default is `FALSE`
#' because bootstrapping in regression
#' by [lm()] is rarely slow.
#'
#' @param ncpus If `do_boot` is `TRUE`
#' and `parallel` is also `TRUE`, this
#' argument is the number of processes
#' to be used in parallel processing.
#' Default
#' is `parallel::detectCores(logical = FALSE) - 1`
#'
#' @param progress Logical. If `TRUE`,
#' progress bars will be displayed
#' for long process.
#'
#' @param load_balancing Logical. If
#' `parallel` is `TRUE`, this determine
#' whether load balancing will be used.
#' Default is `TRUE`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
#' (2022) Improving an old way to measure moderation effect in standardized
#' units. *Health Psychology*, *41*(7), 502-505.
#' \doi{10.1037/hea0001188}
#'
#' Craig, C. C. (1936). On the frequency function of xy.
#' *The Annals of Mathematical Statistics, 7*(1),
#' 1--15. \doi{10.1214/aoms/1177732541}
#'
#' Gelman, A., Hill, J., & Vehtari, A. (2021).
#' *Regression and other stories*.
#' Cambridge University Press.
#' \doi{10.1017/9781139161879}
#'
#' Jones, J. A., & Waller, N. G. (2013). Computing confidence
#' intervals for standardized regression coefficients.
#' *Psychological Methods, 18*(4), 435--453.
#' \doi{10.1037/a0033269}

#'
#' @seealso [print.lav_betaselect()] for its print method.
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' # Standardize only iv
#'
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv")
#' lm_beta_x
#' summary(lm_beta_x)
#'
#' # Manually standardize iv and call lm()
#'
#' data_test_mod_cat$iv_z <- scale(data_test_mod_cat[, "iv"])[, 1]
#'
#' lm_beta_x_manual <- lm(dv ~ iv_z*mod + cov1 + cat1,
#'                        data = data_test_mod_cat)
#'
#' coef(lm_beta_x)
#' coef(lm_beta_x_manual)
#'
#' # Standardize all numeric variables
#'
#' lm_beta_all <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                              data = data_test_mod_cat)
#' # Note that cat1 is not standardized
#' summary(lm_beta_all)
#'
#' @rdname lm_betaselect
#' @export

lm_betaselect <- function(...,
                          to_standardize = NULL,
                          not_to_standardize = NULL,
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

    # Construct a call to lm_betaselect with argument names
    my_call2 <- as.list(match.call(expand.dots = FALSE))
    lm_betaselect_args <- c(lm_ustd_args, my_call2[-c(1,2)])
    lm_betaselect_call <- as.call(c(str2lang("betaselectr::lm_betaselect"),
                                    lm_betaselect_args))

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
                        skip_categorical_x = TRUE
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
        # Need this trick to use pgk::name
        tmp[[1]] <- str2lang("betaselectr::std_data")
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
    out$lm_betaselect$call <- lm_betaselect_call
    out$lm_betaselect$to_standardize <- to_standardize

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

#' @title Standardize Selected Variables
#'
#' @description Standardize selected
#' variables in a data frame or similar
#' object.
#'
#' @details This is a helper functions
#' to be used by [lm_betaselect()]. It
#' assumes that the variables selected
#' has been checked whether they are
#' numeric.
#'
#' @return
#' A data frame similar to `data`,
#' with selected variables standardized.
#'
#' @param data A data frame or similar
#' object.
#'
#' @param to_standardize A character
#' vector of the column names of
#' variables to be standardized.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' data(data_test_mod_cat)
#' dat <- data_test_mod_cat
#' dat <- std_data(dat, to_standardize = c("iv", "dv"))
#' colMeans(dat[, c("dv", "iv")])
#' apply(dat[, c("dv", "iv")], 2, sd)
#'
#' @export

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
                  data_i <- input_data[i, ]
                  input_data_z <- betaselectr::std_data(data_i,
                                            to_standardize = to_standardize)
                  lm_args_i$data <- data_i
                  ustd_i <- do.call(stats::lm,
                                    lm_args_i)
                  lm_args_i$data <- input_data_z
                  std_i <- do.call(stats::lm,
                                    lm_args_i)
                  list(coef_ustd = stats::coef(ustd_i),
                       vcov_ustd = stats::vcov(ustd_i),
                       sigma_mm_ustd = stats::cov(stats::model.matrix(ustd_i)),
                       coef_std = stats::coef(std_i),
                       vcov_std = stats::vcov(std_i),
                       sigma_mm_std = stats::cov(stats::model.matrix(std_i)))
                }

    if (parallel) {
        if (load_balancing && progress) {
            pbopt_old <- pbapply::pboptions(use_lb = TRUE)
            on.exit(pbapply::pboptions(pbopt_old), add = TRUE)
          }
        my_cl <- parallel::makeCluster(ncpus)
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
    attr(boot_out,
         "boot_idx") <- boot_idx
    return(boot_out)
  }

#' @param x A `lm_betaselect`-class object.
#'
#' @param digits The number of significant
#' digits to be printed for the
#' coefficients.
#'
#' @param type The coefficients to be
#' printed. For `"beta"` or
#' `"standardized"`, the coefficients
#' after selected variables standardized
#' will be printed. For `"raw"` or
#' `"unstandardized"`, the coefficients
#' before standardization was done will
#' be printed.
#'
#' @rdname lm_betaselect
#' @export

print.lm_betaselect <- function(x,
                                digits = max(3L, getOption("digits") - 3L),
                                type = c("beta", "standardized", "raw", "unstandardized"),
                                ...) {
    type <- match.arg(type)
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    to_standardize <- x$lm_betaselect$to_standardize

    cat("\nCall to lm_betaselect():\n")
    print(x$lm_betaselect$call)
    if (type == "beta") {
        if (length(to_standardize) > 0) {
            tmp <- paste0(to_standardize, collapse = ", ")
          } else {
            tmp <- "[Nil]"
          }
        cat("\nVariable(s) standardized:",
            tmp)
        cat("\n")
        cat("\nModel *after* standardization:\n")
        NextMethod()
      } else {
        cat("\nModel *before* standardization:\n")
        NextMethod(x = x$lm_betaselect$ustd,
                   digits = digits,
                   ...)
      }
    invisible(x)
  }

#' @param x A `lm_betaselect`-class object.
#'
#' @details
#' The function [raw_output()] simply extracts
#' the regression output by [stats::lm()]
#' on the variables without standardization.
#'
#' @return
#' The function [raw_output()] returns
#' an object of the class `lm`, which are
#' the results of fitting the model
#' to the data by [stats::lm()] without
#' standardization.
#'
#' @examples
#'
#' summary(raw_output(lm_beta_x))
#'
#' @rdname lm_betaselect
#' @export

raw_output <- function(x) {
    x$lm_betaselect$ustd
  }