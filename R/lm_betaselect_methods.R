#' @title Coefficients of
#' Beta-Select in Linear Models
#'
#' @description Return the estimates of
#' coefficients in an
#' `lm_betaselect`-class or
#' `glm_betaselect`-class object.
#
#' @details By default, it extracts the
#' regression coefficients *after* the
#' selected variables have been
#' standardized. If requested, it can
#' also return the regression
#' coefficients *without*
#' standardization.
#'
#' @return
#' A scalar vector: The estimate of
#' regression coefficients.
#'
#' @param object The output of
#' [lm_betaselect()] or
#' [glm_betaselect()], or an
#' `lm_betaselect`-class or
#' `glm_betaselect`-class object.
#'
#' @param complete If `TRUE`, it returns
#' the full vector of coefficients,
#' including those of terms dropped in
#' an over-determined system. See
#' [stats::coef()] for further
#' information. Default is `FALSE`.
#'
#' @param type String. If `"unstandardized"`
#' or `"raw"`, the coefficients *before*
#' standardization are returned. If
#' `"beta"` or `"standardized"`, then
#' the coefficients *after* selected
#' variables standardized are returned.
#' Default is `"beta"`.
#'
#' @param ...  Other arguments. Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_betaselect()] and
#' [glm_betaselect()]
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv")
#' coef(lm_beta_x)
#' coef(lm_beta_x, type = "raw")
#'
#' @export
coef.lm_betaselect <- function(object,
                               complete = FALSE,
                               type = c("beta",
                                        "standardized",
                                        "raw",
                                        "unstandardized"),
                               ...) {
    type <- match.arg(type)
    if (type %in% c("beta", "standardized")) {
        NextMethod()
      } else {
        ustd_out <- object$lm_betaselect$ustd
        return(stats::coef(ustd_out))
      }
  }

#' @examples
#'
#' data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
#' data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
#'                               yes = 1,
#'                               no = 0)
#' logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
#'                                   data = data_test_mod_cat,
#'                                   family = binomial,
#'                                   to_standardize = "iv")
#' coef(logistic_beta_x)
#' coef(logistic_beta_x, type = "raw")
#'
#' @rdname coef.lm_betaselect
#' @export

coef.glm_betaselect <- coef.lm_betaselect

#' @title The 'vcov' Method for
#' 'lm_betaselect' and `glm_betaselect`
#' Objects
#'
#' @description Compute the
#' variance-covariance matrix of
#' estimates in the output of
#' [lm_betaselect()] or
#' [glm_betaselect()].
#'
#' @details The type of
#' variance-covariance matrix depends
#' on the object. If bootstrapping
#' was requested, by default it returns
#' the bootstrap variance-covariance
#' matrix. Otherwise, it returns the
#' default variance-covariance
#' matrix and raises a warning.
#'
#' Support for other type of
#' variance-covariance matrix will be
#' added.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @seealso [lm_betaselect()] and
#' [glm_betaselect()]
#'
#' @return
#' A matrix of the variances and
#' covariances of the parameter
#' estimates.
#'
#' @param object The output of
#' [lm_betaselect()]
#' or an `lm_betaselect`-class object,
#' or the output of [glm_betaselect()]
#' or a `glm_betaselect`-class object.
#'
#' @param method The method used to
#' compute the variance-covariance
#' matrix. If bootstrapping was
#' requested when calling
#' [lm_betaselect()] or
#' [glm_betaselect()] and this argument
#' is set to `"bootstrap"` or `"boot"`,
#' the bootstrap variance-covariance
#' matrix is returned. If bootstrapping
#' was not requested or if this argument
#' is set to `"ls"` or `"default"`,
#' then the usual `lm` or `glm`
#' variance-covariance matrix is
#' returned, with a warning raised
#' unless `type` is `"raw"` or
#' `"unstandardized".`
#' Default is `"boot"`.
#'
#' @param type String. If
#' `"unstandardized"` or `"raw"`, the
#' variance-covariance matrix of the
#' coefficients *before* standardization
#' are returned. If `"beta"` or
#' `"standardized"`, then the
#' variance-covariance matrix of the
#' coefficients *after* selected
#' variables standardized are returned.
#' Default is `"beta"`.
#'
#' @param warn Logical. WHether a warning
#' will be raised is OLS (or WLS)
#' variance-covariance matrix is
#' requested for the model with some
#' variables standardized (i.e., `type`
#' is `"beta"` or `"standardized"`).
#' Default is `TRUE`.
#'
#' @param ...  Other arguments to be
#' passed to [stats::vcov()].
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' # bootstrap should be set to 2000 or 5000 in real studies
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv",
#'                            do_boot = TRUE,
#'                            bootstrap = 100,
#'                            iseed = 1234)
#' vcov(lm_beta_x)
#' # A warning is expected for the following call
#' vcov(lm_beta_x, method = "ls")
#' vcov(lm_beta_x, type = "raw")
#'
#'
#' @export
# Adapted from vcov.std_selected()

vcov.lm_betaselect <- function(object,
                               method = c("boot", "bootstrap", "ls", "default"),
                               type = c("beta",
                                        "standardized",
                                        "raw",
                                        "unstandardized"),
                               warn = TRUE,
                               ...) {
    method <- match.arg(method)
    type <- match.arg(type)
    method <- switch(method,
                     boot = "boot",
                     bootstrap = "boot",
                     ls = "ls",
                     default = "ls")
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    if (identical(method, "boot") && is.null(object$lm_betaselect$boot_out)) {
        warning("Bootstrap estimates not available; ",
                "'method' changed to 'ls'.")
        method <- "ls"
      }
    if (type == "beta") {
        if (method == "boot") {
            boot_out <- object$lm_betaselect$boot_out
            boot_est <- sapply(boot_out, function(x) {
                            x$coef_std
                          })
            out <- stats::cov(t(boot_est))
            return(out)
          } else {
            if (warn) {
                warning("With standardization, the variance-covariance matrix ",
                        "from 'lm()' or 'glm()' should not be used.")
              }
            NextMethod()
          }
      } else {
        if (method == "boot") {
            boot_out <- object$lm_betaselect$boot_out
            boot_est <- sapply(boot_out, function(x) {
                            x$coef_ustd
                          })
            out <- stats::cov(t(boot_est))
            return(out)
          } else {
            out <- stats::vcov(object$lm_betaselect$ustd)
            return(out)
          }
      }
  }

#' @examples
#'
#' data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
#' data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
#'                               yes = 1,
#'                               no = 0)
#' # bootstrap should be set to 2000 or 5000 in real studies
#' logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
#'                                   data = data_test_mod_cat,
#'                                   family = binomial,
#'                                   to_standardize = "iv",
#'                                   do_boot = TRUE,
#'                                   bootstrap = 100,
#'                                   iseed = 1234)
#' vcov(logistic_beta_x)
#' # A warning is expected for the following call
#' vcov(logistic_beta_x, method = "default")
#' vcov(logistic_beta_x, type = "raw")
#'
#' @rdname vcov.lm_betaselect
#' @export

vcov.glm_betaselect <- vcov.lm_betaselect

#' @title Confidence Interval for
#' 'lm_betaselect' or 'glm_betaselect'
#' Objects
#'
#' @description Return the confidence
#' interval of the regression
#' coefficients in the output of
#' [lm_betaselect()] or
#' [glm_betaselect()].
#'
#' @details
#' The type of
#' confidence intervals depends
#' on the object. If bootstrapping
#' was requested, by default it returns
#' the percentile bootstrap confidence
#' intervals. Otherwise, it returns the
#' default confidence intervals
#' and raises a warning for the
#' standardized solution.
#'
#' Support for other type of
#' confidence intervals will be
#' added.
#'
#' @return
#' A *p* by 2 matrix of the confidence
#' intervals, *p* being the number
#' of coefficients.
#'
#' @param object The output of
#' [lm_betaselect()] or
#' [glm_betaselect()].
#'
#' @param parm The terms for which
#' the confidence intervals are returned.
#' If missing, the confidence intervals
#' of all terms will be returned.
#'
#' @param level The level of confidence,
#' default is .95, returning the 95%
#' confidence interval.
#'
#' @param method The method used to
#' compute the confidence intervals/
#' If bootstrapping was
#' requested when calling
#' [lm_betaselect()] and this argument
#' is set to `"bootstrap"` or `"boot"`,
#' the bootstrap confidence intervals
#' are returned. If bootstrapping
#' was not requested or if this argument
#' is set to `"ls"`, then the usual `lm`
#' confidence intervals are
#' returned, with a warning raised
#' unless `type` is `"raw"` or
#' `"unstandardized".`
#' Default is `"boot"`.
#'
#' @param type String. If
#' `"unstandardized"` or `"raw"`, the
#' confidence intervals of the
#' coefficients *before* standardization
#' are returned. If `"beta"` or
#' `"standardized"`, then the
#' confidence intervals of the
#' coefficients *after* selected
#' variables standardized are returned.
#' Default is `"beta"`.
#'
#' @param warn Logical. Whether a warning
#' will be raised is OLS (or WLS)
#' confidence intervals are
#' requested for the model with some
#' variables standardized (i.e., `type`
#' is `"beta"` or `"standardized"`).
#' Default is `TRUE`.
#'
#' @param boot_type The type of
#' bootstrap confidence intervals.
#' Currently, it supports `"perc"`,
#' percentile bootstrap confidence
#' intervals, and `"bc"`, bias-corrected
#' bootstrap confidence interval.
#'
#' @param ...  Optional arguments.
#' Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_betaselect()]
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' # bootstrap should be set to 2000 or 5000 in real studies
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv",
#'                            do_boot = TRUE,
#'                            bootstrap = 100,
#'                            iseed = 1234)
#' confint(lm_beta_x)
#' confint(lm_beta_x, method = "ls")
#' confint(lm_beta_x, type = "raw")
#'
#' @export

confint.lm_betaselect <- function(object,
                                  parm,
                                  level = .95,
                                  method = c("boot", "bootstrap", "ls"),
                                  type = c("beta",
                                           "standardized",
                                           "raw",
                                           "unstandardized"),
                                  warn = TRUE,
                                  boot_type = c("perc", "bc"),
                                  ...) {
    method <- match.arg(method)
    type <- match.arg(type)
    boot_type <- match.arg(boot_type)
    if (missing(parm)) {
        parm <- stats::variable.names(object)
      }
    method <- switch(method,
                     boot = "boot",
                     bootstrap = "boot",
                     ls = "ls")
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    if (identical(method, "boot") && is.null(object$lm_betaselect$boot_out)) {
        warning("Bootstrap estimates not available; ",
                "'method' changed to 'ls'.")
        method <- "ls"
      }
    if (type == "beta") {
        if (method == "boot") {1
            boot_out <- object$lm_betaselect$boot_out
            boot_idx <- attr(boot_out, "boot_idx")
            boot_est <- lapply(parm, function(y) {
                            out <- sapply(boot_out, function(x) {
                                      x$coef_std[y]
                                    })
                            out
                          })
            est <- stats::coef(object,
                               type = type)[parm]
            out <- mapply(boot_ci_internal,
                          t0 = est,
                          t = boot_est,
                          level = level,
                          boot_type = boot_type,
                          add_names = TRUE,
                          SIMPLIFY = FALSE)
            out <- do.call(rbind, out)
            return(out)
          } else {
            if (warn) {
                warning("With standardization, the variance-covariance matrix ",
                        "using OLS or WLS should not be used.")
              }
            class(object) <- "lm"
            out <- stats::confint(object,
                                  parm = parm,
                                  level = level,
                                  ...)
            return(out)
          }
      } else {
        if (method == "boot") {
            boot_out <- object$lm_betaselect$boot_out
            boot_idx <- attr(boot_out, "boot_idx")
            boot_est <- lapply(parm, function(y) {
                            out <- sapply(boot_out, function(x) {
                                      x$coef_ustd[y]
                                    })
                            out
                          })
            est <- stats::coef(object,
                               type = type)[parm]
            out <- mapply(boot_ci_internal,
                          t0 = est,
                          t = boot_est,
                          level = level,
                          boot_type = boot_type,
                          add_names = TRUE,
                          SIMPLIFY = FALSE)
            out <- do.call(rbind, out)
            return(out)
          } else {
            out <- stats::confint(object$lm_betaselect$ustd,
                                  parm = parm,
                                  level = level)
            return(out)
          }
      }
  }

#' @param trace Logical. Whether profiling
#' will be traced. See
#' [stats::confint.glm()] for details.
#' ignored if `method` is `"boot"` or
#' `"bootstrap"`.
#'
#' @param test The test used for
#' profiling. See [stats::confint.glm]
#' for details.
#' ignored if `method` is `"boot"` or
#' `"bootstrap"`.
#'
#' @examples
#'
#' data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
#' data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
#'                               yes = 1,
#'                               no = 0)
#' # bootstrap should be set to 2000 or 5000 in real studies
#' logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
#'                                   data = data_test_mod_cat,
#'                                   family = binomial,
#'                                   to_standardize = "iv",
#'                                   do_boot = TRUE,
#'                                   bootstrap = 100,
#'                                   iseed = 1234)
#'
#' confint(logistic_beta_x, method = "default")
#' confint(logistic_beta_x, type = "raw")
#'
#' @rdname confint.lm_betaselect
#' @export

# Code duplication is intentional
confint.glm_betaselect <- function(object,
                                   parm,
                                   level = .95,
                                   trace = FALSE,
                                   test = c("LRT", "Rao"),
                                   method = c("boot", "bootstrap", "default", "ls"),
                                   type = c("beta",
                                            "standardized",
                                            "raw",
                                            "unstandardized"),
                                   warn = TRUE,
                                   boot_type = c("perc", "bc"),
                                   ...) {
    test <- match.arg(test)
    method <- match.arg(method)
    type <- match.arg(type)
    boot_type <- match.arg(boot_type)
    if (missing(parm)) {
        parm <- stats::variable.names(object)
      }
    method <- switch(method,
                     boot = "boot",
                     bootstrap = "boot",
                     ls = "ls",
                     default = "ls")
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    if (identical(method, "boot") && is.null(object$lm_betaselect$boot_out)) {
        warning("Bootstrap estimates not available; ",
                "'method' changed to 'ls' or 'default'.")
        method <- "ls"
      }
    if (type == "beta") {
        if (method == "boot") {1
            boot_out <- object$lm_betaselect$boot_out
            boot_idx <- attr(boot_out, "boot_idx")
            boot_est <- lapply(parm, function(y) {
                            out <- sapply(boot_out, function(x) {
                                      x$coef_std[y]
                                    })
                            out
                          })
            est <- stats::coef(object,
                               type = type)[parm]
            out <- mapply(boot_ci_internal,
                          t0 = est,
                          t = boot_est,
                          level = level,
                          boot_type = boot_type,
                          add_names = TRUE,
                          SIMPLIFY = FALSE)
            out <- do.call(rbind, out)
            return(out)
          } else {
            if (warn) {
                warning("With standardization, the confidence interval",
                        "from 'lm()' or 'glm()' should not be used.")
              }
            class(object) <- "glm"
            out <- stats::confint(object,
                                  parm = parm,
                                  level = level,
                                  trace = trace,
                                  test = test,
                                  ...)
            return(out)
          }
      } else {
        if (method == "boot") {
            boot_out <- object$lm_betaselect$boot_out
            boot_idx <- attr(boot_out, "boot_idx")
            boot_est <- lapply(parm, function(y) {
                            out <- sapply(boot_out, function(x) {
                                      x$coef_ustd[y]
                                    })
                            out
                          })
            est <- stats::coef(object,
                               type = type)[parm]
            out <- mapply(boot_ci_internal,
                          t0 = est,
                          t = boot_est,
                          level = level,
                          boot_type = boot_type,
                          add_names = TRUE,
                          SIMPLIFY = FALSE)
            out <- do.call(rbind, out)
            return(out)
          } else {
            out <- stats::confint(object$lm_betaselect$ustd,
                                  parm = parm,
                                  level = level,
                                  trace = trace,
                                  test = test)
            return(out)
          }
      }
  }

#' @title ANOVA Tables For
#' 'lm_betaselect' and 'glm_betaselect'
#' Objects
#'
#' @description Return the analysis
#' of variance tables for
#' the outputs of
#' [lm_betaselect()] and
#' [glm_betaselect()].
#'
#' @details
#' By default, it calls [stats::anova()]
#' on the results with selected variables
#' standardized. By setting `type` to
#' `"raw"` or `"unstandardized"`, it
#' calls [stats::anova()] on the results
#' *before* standardization.
#'
#' @return
#' It returns an object of class
#' `anova`, which is identical to
#' the output of [stats::anova()] in
#' structure.
#'
#' @param object The output of
#' [lm_betaselect()] or
#' [glm_betaselect()].
#'
#' @param ...  Additional outputs
#' of [lm_betaselect()] or
#' [glm_betaselect()].
#'
#' @param type String. If
#' `"unstandardized"` or `"raw"`, the
#' output *before* standardization
#' are used If `"beta"` or
#' `"standardized"`, then the
#' output *after* selected
#' variables standardized are returned.
#' Default is `"beta"`.
#'
#' @param dispersion To be passed to
#' [stats::anova.glm()]. The dispersion
#' parameter. Default ia `NULL` and it
#' is extracted from the model.
#'
#' @param test String. The test to be
#' conducted. Please refer to
#' [stats::anova.glm()] for details.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_betaselect()]
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv",
#'                            do_boot = FALSE)
#' anova(lm_beta_x)
#' anova(lm_beta_x, type = "raw")
#'
#' @export

anova.lm_betaselect <- function(object,
                                ...,
                                type = c("beta",
                                          "standardized",
                                          "raw",
                                          "unstandardized")) {
    type <- match.arg(type)
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    if (type == "beta") {
        NextMethod()
      } else {
        objects <- c(list(object), list(...))
        ustds <- lapply(objects, function(x) x$lm_betaselect$ustd)
        out <- do.call(stats::anova,
                       ustds)
        return(out)
      }
  }

#' @examples
#'
#' data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
#' data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
#'                               yes = 1,
#'                               no = 0)
#' logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
#'                                   data = data_test_mod_cat,
#'                                   family = binomial,
#'                                   to_standardize = "iv")
#' anova(logistic_beta_x)
#' anova(logistic_beta_x, type = "raw")
#'
#' @rdname anova.lm_betaselect
#' @export

anova.glm_betaselect <- function(object,
                                 ...,
                                 type = c("beta",
                                          "standardized",
                                          "raw",
                                          "unstandardized"),
                                 dispersion = NULL,
                                 test = NULL) {
    type <- match.arg(type)
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    if (type == "beta") {
        type <- NULL
        NextMethod()
      } else {
        objects <- c(list(object), list(...))
        ustds <- lapply(objects, function(x) x$lm_betaselect$ustd)
        out <- do.call(stats::anova,
                       ustds)
        return(out)
      }
  }

#' @title Summary of an
#' 'lm_betaselect'-Class Object
#'
#' @description The `summary` method
#' for `lm_betaselect`-class objects.
#'
#' @details
#' By default, it returns a
#' `summary.lm_betaselect`-class object
#' for the results with selected variables
#' standardized. By setting `type` to
#' `"raw"` or `"unstandardized"`, it
#' return the summary for the results
#' *before* standardization.
#'
#' @return
#' It returns an object of class
#' `summary.lm_betaselect`, which is
#' similar to the output of
#' [stats::summary.lm()], with additional
#' information on the standardization
#' and bootstrapping, if requested.
#'
#' @param object The output of
#' [lm_betaselect()].
#'
#' @param correlation If `TRUE`, the
#' correlation matrix of the estimates
#' will be returned. The same argument
#' in [stats::summary.lm()]. Default
#' is `FALSE`.
#'
#' @param symbolic.cor If `TRUE`,
#' correlations are printed in symbolic
#' form as in [stats::summary.lm()].
#' Default is `FALSE`.
#'
#' @param se_method The method used to
#' compute the standard errors and
#' confidence intervals (if requested).
#' If bootstrapping was
#' requested when calling
#' [lm_betaselect()] and this argument
#' is set to `"bootstrap"` or `"boot"`,
#' the bootstrap standard errors are
#' returned. If bootstrapping
#' was not requested or if this argument
#' is set to `"t"`, `"lm"`, or `"ls"`,
#' then the usual `lm`
#' standard errors are
#' returned, with a warning raised
#' unless `type` is `"raw"` or
#' `"unstandardized".`
#' Default is `"boot"`.
#'
#' @param ci Logical. Whether
#' confidence intervals are computed.
#' Default is `FALSE.`
#'
#' @param level The level of confidence,
#' default is .95, returning the 95%
#' confidence interval.
#'
#' @param boot_type The type of
#' bootstrap confidence intervals,
#' if requested.
#' Currently, it supports `"perc"`,
#' percentile bootstrap confidence
#' intervals, and `"bc"`, bias-corrected
#' bootstrap confidence interval.
#'
#' @param boot_pvalue_type The type
#' of *p*-values if `se_method` is
#' `"boot"` or `"bootstrap"`. If `"norm"`,
#' then the *z* score is used to compute
#' the *p*-value using a
#' standard normal distribution.
#' If `"asymmetric"`, the default, then
#' the method presented in
#' Asparouhov and Muthén (2021) is used
#' to compute the *p*-value based on the
#' bootstrap distribution.
#'
#' @param type String. If
#' `"unstandardized"` or `"raw"`, the
#' output *before* standardization
#' are used If `"beta"` or
#' `"standardized"`, then the
#' output *after* selected
#' variables standardized are returned.
#' Default is `"beta"`.
#'
#' @param ...  Additional arguments
#' passed to other methods.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
#' Retrieved from https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
#'
#' @seealso [lm_betaselect()]
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' # bootstrap should be set to 2000 or 5000 in real studies
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv",
#'                            do_boot = TRUE,
#'                            bootstrap = 100,
#'                            iseed = 1234)
#'
#' summary(lm_beta_x)
#' summary(lm_beta_x, ci = TRUE)
#' summary(lm_beta_x, boot_pvalue_type = "norm")
#' summary(lm_beta_x, type = "raw")
#'
#' @rdname summary.lm_betaselect
#'
#' @export

summary.lm_betaselect <- function(object,
                                  correlation = FALSE,
                                  symbolic.cor = FALSE,
                                  se_method = c("boot", "bootstrap",
                                                "t", "lm", "ls"),
                                  ci = FALSE,
                                  level = .95,
                                  boot_type = c("perc", "bc"),
                                  boot_pvalue_type = c("asymmetric", "norm"),
                                  type = c("beta",
                                           "standardized",
                                           "raw",
                                           "unstandardized"),
                                  ...) {
    se_method <- match.arg(se_method)
    type <- match.arg(type)
    se_method <- switch(se_method,
                        boot = "boot",
                        bootstrap = "boot",
                        t = "ls",
                        lm = "ls",
                        ls = "ls")
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    boot_type <- match.arg(boot_type)
    boot_pvalue_type <- match.arg(boot_pvalue_type)
    if (identical(se_method, "boot") && is.null(object$lm_betaselect$boot_out)) {
        warning("Bootstrap estimates not available; ",
                "'se_method' changed to 'ls'.")
        se_method <- "ls"
      }
    if (type == "beta") {
        out <- NextMethod()
      } else {
        # type = "raw"
        out <- stats::summary.lm(object = object$lm_betaselect$ustd,
                                  correlation = correlation,
                                  symbolic.cor = symbolic.cor,
                                  ...)
      }
    out$lm_betaselect$summary_call <- match.call()
    out$lm_betaselect$call <- object$lm_betaselect$call
    out$lm_betaselect$to_standardize <- object$lm_betaselect$to_standardize
    out$lm_betaselect$se_method <- se_method
    out$lm_betaselect$ci <- ci
    out$lm_betaselect$level <- level
    out$lm_betaselect$boot_type <- boot_type
    out$lm_betaselect$type <- type
    out$lm_betaselect$boot_pvalue_type <- boot_pvalue_type
    class(out) <- c("summary.lm_betaselect", class(out))
    if (se_method == "boot") {
        boot_out <- object$lm_betaselect$boot_out
        out$lm_betaselect$bootstrap <- length(boot_out)
        boot_est <- sapply(boot_out, function(x) {
                        x$coef_std
                      })
        boot_est_se <- apply(boot_est, 1, stats::sd, simplify = TRUE)
        out$coefficients[, "Std. Error"] <- boot_est_se
        z_values <- out$coefficients[, "Estimate"] / boot_est_se
        out$coefficients[, "t value"] <- z_values
        i <- which(colnames(out$coefficients) == "t value")
        colnames(out$coefficients)[i] <- "z value"
        if (boot_pvalue_type == "asymmetric") {
            boot_est_list <- split(boot_est, rownames(boot_est))
            boot_pvalues <- sapply(boot_est_list,
                                   est2p,
                                   h0 = 0)
          } else {
            # boot_pvalue_type == "norm"
            boot_pvalues <- stats::pnorm(abs(z_values),
                                         lower.tail = FALSE) * 2
          }
        out$coefficients[, "Pr(>|t|)"] <- boot_pvalues
        i <- which(colnames(out$coefficients) == "Pr(>|t|)")
        colnames(out$coefficients)[i] <- switch(boot_pvalue_type,
                    asymmetric = "Pr(Boot)",
                    norm = "Pr(>|z|)")
      } else {
        # se_method == "ls"
        # No need to change
      }
    if (ci) {
        out_ci <- confint.lm_betaselect(object,
                                        level = level,
                                        method = se_method,
                                        type = type,
                                        warn = FALSE,
                                        boot_type = boot_type)
        colnames(out_ci) <- c("CI.Lower", "CI.Upper")
        i <- which(colnames(out$coefficients) == "Estimate")
        out_coef <- out$coefficients
        out_coef <- cbind(out_coef[, seq_len(i), drop = FALSE],
                          out_ci,
                          out_coef[, -seq_len(i), drop = FALSE])
        out$coefficients <- out_coef
      }
    out
  }

#' @details
#' The `print` method of
#' `summary.lm_betaselect`-class objects
#' is adapted from
#' [stdmod::print.summary.std_selected()].
#'
#' @return
#' The `print`-method of
#' `summary.lm_betaselect` is called
#' for its side effect. The object `x`
#' is returned invisibly.
#'
#' @param x The output of
#' [summary.lm_betaselect()].
#'
#' @param est_digits The number of
#' digits after the decimal to be
#' displayed for the coefficient
#' estimates, their standard errors, and
#' confidence intervals (if present).
#' Note that the values will be rounded
#' to this number of digits before
#' printing. If all digits at this
#' position are zero for all values, the
#' values may be displayed with fewer
#' digits. Note that the coefficient
#' table is printed by
#' [stats::printCoefmat()]. If some
#' numbers are vary large, the number of
#' digits after the decimal may be
#' smaller than `est_digits` due to a
#' limit on the column width. This value
#' also determines the number of digits
#' for displayed R-squared.
#'
#' @param signif.stars Whether "stars"
#' (asterisks) are printed to denote
#' the level of significance achieved
#' for each coefficient. Default is
#' `TRUE`.
#'
#' @param tz_digits The number of digits
#' after the decimal to be displayed for
#' the *t* or similar statistic (in the
#' column `"t value"` or `"z value"`).
#' This value also determines the number
#' of digits for the *F* statistic for
#' the R-squared.
#'
#' @param pvalue_less_than If a
#' *p*-value is less than this value, it
#' will be displayed with `"<(this
#' value)".` For example, if
#' `pvalue_less_than` is .001, the
#' default, *p*-values less than .001
#' will be displayed as `<.001`. This
#' value also determines the printout of
#' the *p*-value of the *F* statistic.
#' (This argument does what `eps.Pvalue`
#' does in [stats::printCoefmat()].)
#'
#'
#' @rdname summary.lm_betaselect
#'
#' @export

print.summary.lm_betaselect <- function(x,
                                        est_digits = 3,
                                        symbolic.cor = x$symbolic.cor,
                                        signif.stars = getOption("show.signif.stars"),
                                        tz_digits = 3,
                                        pvalue_less_than = .001,
                                        ...) {
    cat("Call to lm_betaselect():\n")
    print(x$lm_betaselect$call)
    to_standardize <- x$lm_betaselect$to_standardize
    type <- x$lm_betaselect$type
    level <- x$lm_betaselect$level
    level_str <- paste0(formatC(level * 100, digits = 1,
                                format = "f"),
                        "%")
    if (length(to_standardize) > 0) {
        tmp <- paste(to_standardize, collapse = ", ")
        tmp <- strwrap(tmp)
      } else {
        tmp <- "[Nil]"
      }
    cat("\nVariable(s) standardized:",
        tmp, "\n")

    x_rsq <- x$r.squared
    x_rsq_adj <- x$adj.r.squared
    x_fstatistic <- x$fstatistic
    x$coefficients[, "Estimate"] <- round(x$coefficients[, "Estimate"], est_digits)
    x$coefficients[, "Std. Error"] <- round(x$coefficients[, "Std. Error"], est_digits)
    if (x$lm_betaselect$ci) {
        x$coefficients[, "CI.Lower"] <- round(x$coefficients[, "CI.Lower"], est_digits)
        x$coefficients[, "CI.Upper"] <- round(x$coefficients[, "CI.Upper"], est_digits)
      }
    i <- match(c("t value", "z value"), colnames(x$coefficients))
    i <- i[!is.na(i)]
    x$coefficients[, i] <- round(x$coefficients[, i], tz_digits)
    x$fstatistic <- NULL
    NextMethod(eps.Pvalue = pvalue_less_than,
              dig.tst = tz_digits)

    cat(format_rsq(rsq = x_rsq,
                   rsq_adj = x_rsq_adj,
                   digits = est_digits), sep = "\n")
    print_fstatistic(x_fstatistic,
                      f_digits = tz_digits,
                      p_digits = ceiling(-log10(pvalue_less_than)))
    cat("\n")

    tmp <- character(0)
    tmp <- c(tmp, "Note:")
    tmp <- c(tmp,
             strwrap(switch(type,
                beta = "- Results *after* standardization are reported.",
                raw = "- Results *before* standardization are reported."),
                exdent = 2))
    if (x$lm_betaselect$se_method == "boot") {
        tmp <- c(tmp,
                 strwrap("- Nonparametric bootstrapping conducted.",
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap(paste0("- The number of bootstrap samples is ",
                                x$lm_betaselect$bootstrap, "."),
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap("- Standard errors are bootstrap standard errors.",
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap("- Z values are computed by 'Estimate / Std. Error'.",
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap(switch(x$lm_betaselect$boot_pvalue_type,
                           asymmetric = "- The bootstrap p-values are asymmetric p-values by Asparouhov and Muth\u00e9n (2021).",
                           norm = "- The bootstrap p-values are based on standard normal distribution using z values."),
                         exdent = 2))
        if (x$lm_betaselect$ci) {
            boot_type_str <- switch(x$lm_betaselect$boot_type,
                               perc = "Percentile",
                               bc = "Bias-corrected")
            tmp <- c(tmp,
                     strwrap(paste0("- ",
                                   boot_type_str,
                                   " bootstrap ",
                                   level_str,
                                   " confidence interval reported."),
                             exdent = 2))
          }
      } else {
        # se_method == "ls"
        tmp <- c(tmp,
                 strwrap("- Standard errors are least-squares standard errors.",
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap("- T values are computed by 'Estimate / Std. Error'.",
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap("- P-values are usual t-test p-values.",
                         exdent = 2))
        if ((length(to_standardize) > 0) &&
            type == "beta") {
            tmp <- c(tmp,
                     strwrap(paste0("- Least squares standard errors, t values, p-values, and confidence intervals (if reported) ",
                                    "should not be used for coefficients involved in standardization."),
                            exdent = 2))
          }
        if (x$lm_betaselect$ci) {
            tmp <- c(tmp,
                     strwrap(paste0("- ",
                                    "Least squares ",
                                    level_str,
                                    " confidence interval reported."),
                             exdent = 2))
          }
      }
    cat(tmp, sep = "\n")
    invisible(x)
  }


#' @noRd
# Copied from stdmod

format_pvalue <- function(p,
                          eps = 1e-3) {
    p_digits <- ceiling(-log10(eps))
    if (p < eps) {
        return(paste0("< ",
               formatC(eps,
                       digits = p_digits,
                       format = "f")))
      } else {
        return(formatC(p,
                       digits = p_digits,
                       format = "f"))
      }
  }


#' @noRd
# Copied from stdmod

format_rsq <- function(rsq, rsq_adj,
                       digits = 4) {
    x1 <- c("R-squared",
            "Adjusted R-squared")
    x2 <- formatC(c(rsq, rsq_adj),
                  digits = digits,
                  format = "f")
    x1max <- max(nchar(x1))
    i <- which(nchar(x1) != x1max)
    x1[i] <- paste0(x1[i],
                    paste0(rep(" ", x1max - nchar(x1[1])),
                           collapse = ""))
    paste0(x1, "       : ", x2)
  }

#' @noRd
# Copied from stdmod

print_fstatistic <- function(fstatistic,
                             f_digits = 4,
                             p_digits = 3) {
     f <- fstatistic["value"]
     df1 <- fstatistic["numdf"]
     df2 <- fstatistic["dendf"]
     f_txt <- paste0("F(",
                     df1, ", ", df2, ") = ",
                     round(f, f_digits))
     p <- stats::pf(f, df1, df2, lower.tail = FALSE)
     p_txt <- format_pvalue(p,
                            eps = 10^(-p_digits))
     if (!grepl("^<", p_txt)) {
        p_txt <- paste0("= ", p_txt)
       }
     cat("ANOVA test of R-squared : ",
         f_txt, ", p ", p_txt, "\n", sep = "")
  }

#' @title Summary of an
#' 'glm_betaselect'-Class Object
#'
#' @description The `summary` method
#' for `glm_betaselect`-class objects.
#'
#' @details
#' By default, it returns a
#' `summary.glm_betaselect`-class object
#' for the results with selected variables
#' standardized. By setting `type` to
#' `"raw"` or `"unstandardized"`, it
#' returns the summary for the results
#' *before* standardization.
#'
#' @return
#' It returns an object of class
#' `summary.glm_betaselect`, which is
#' similar to the output of
#' [stats::summary.glm()], with additional
#' information on the standardization
#' and bootstrapping, if requested.
#'
#' @param object The output of
#' [glm_betaselect()].
#'
#' @param dispersion The dispersion
#' parameter. If `NULL`, then it is
#' extracted from the object. If
#' a scalar, it will be used as
#' the dispersion parameter. See
#' [stats::summary.glm()] for details.
#'
#' @param correlation If `TRUE`, the
#' correlation matrix of the estimates
#' will be returned. The same argument
#' in [stats::summary.glm()]. Default
#' is `FALSE`.
#'
#' @param symbolic.cor If `TRUE`,
#' correlations are printed in symbolic
#' form as in [stats::summary.glm()].
#' Default is `FALSE`.
#'
#' @param trace Logical. Whether
#' profiling will be traced when forming
#' the confidence interval if
#' `se_method` is `"default"`, `"z"`, or
#' `"glm"`. Ignored if `ci` is `FALSE`.
#' See [stats::confint.glm()] for
#' details.
#'
#' @param test The test used for
#' `se_method` is `"default"`, `"z"`, or
#' `"glm"`. Ignored if `ci` is `FALSE`.
#' See [stats::confint.glm()] for
#' details.
#'
#' @param se_method The method used to
#' compute the standard errors and
#' confidence intervals (if requested).
#' If bootstrapping was
#' requested when calling
#' [glm_betaselect()] and this argument
#' is set to `"bootstrap"` or `"boot"`,
#' the bootstrap standard errors are
#' returned. If bootstrapping
#' was not requested or if this argument
#' is set to `"z"`, `"glm"`, or `"default"`,
#' then the usual `glm`
#' standard errors are
#' returned, with a warning raised
#' unless `type` is `"raw"` or
#' `"unstandardized".`
#' Default is `"boot"`.
#'
#' @param ci Logical. Whether
#' confidence intervals are computed.
#' Default is `FALSE.`
#'
#' @param level The level of confidence,
#' default is .95, returning the 95%
#' confidence interval.
#'
#' @param boot_type The type of
#' bootstrap confidence intervals,
#' if requested.
#' Currently, it supports `"perc"`,
#' percentile bootstrap confidence
#' intervals, and `"bc"`, bias-corrected
#' bootstrap confidence interval.
#'
#' @param boot_pvalue_type The type
#' of *p*-values if `se_method` is
#' `"boot"` or `"bootstrap"`. If `"norm"`,
#' then the *z* score is used to compute
#' the *p*-value using a
#' standard normal distribution.
#' If `"asymmetric"`, the default, then
#' the method presented in
#' Asparouhov and Muthén (2021) is used
#' to compute the *p*-value based on the
#' bootstrap distribution.
#'
#' @param type String. If
#' `"unstandardized"` or `"raw"`, the
#' output *before* standardization
#' are used If `"beta"` or
#' `"standardized"`, then the
#' output *after* selected
#' variables standardized are returned.
#' Default is `"beta"`.
#'
#' @param ...  Additional arguments
#' passed to other methods.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
#' Retrieved from https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
#'
#' @seealso [glm_betaselect()]
#'
#' @examples
#'
#' data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
#' data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
#'                               yes = 1,
#'                               no = 0)
#' # bootstrap should be set to 2000 or 5000 in real studies
#' logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
#'                                   data = data_test_mod_cat,
#'                                   family = binomial,
#'                                   to_standardize = "iv",
#'                                   do_boot = TRUE,
#'                                   bootstrap = 100,
#'                                   iseed = 1234)
#' summary(logistic_beta_x)
#'
#' @rdname summary.glm_betaselect
#'
#' @export
# Code duplication intentional
summary.glm_betaselect <- function(object,
                                   dispersion = NULL,
                                   correlation = FALSE,
                                   symbolic.cor = FALSE,
                                   trace = FALSE,
                                   test = c("LRT", "Rao"),
                                   se_method = c("boot", "bootstrap",
                                                 "z", "glm", "default"),
                                   ci = FALSE,
                                   level = .95,
                                   boot_type = c("perc", "bc"),
                                   boot_pvalue_type = c("asymmetric", "norm"),
                                   type = c("beta",
                                            "standardized",
                                            "raw",
                                            "unstandardized"),
                                   ...) {
    se_method <- match.arg(se_method)
    type <- match.arg(type)
    se_method <- switch(se_method,
                        boot = "boot",
                        bootstrap = "boot",
                        z = "default",
                        glm = "default",
                        default = "default")
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    boot_type <- match.arg(boot_type)
    boot_pvalue_type <- match.arg(boot_pvalue_type)
    if (identical(se_method, "boot") && is.null(object$lm_betaselect$boot_out)) {
        warning("Bootstrap estimates not available; ",
                "'se_method' changed to 'default'.")
        se_method <- "default"
      }
    if (type == "beta") {
        out <- NextMethod()
      } else {
        # type = "raw"
        out <- stats::summary.glm(object = object$lm_betaselect$ustd,
                                  dispersion = dispersion,
                                  correlation = correlation,
                                  symbolic.cor = symbolic.cor,
                                  ...)
      }
    out$lm_betaselect$summary_call <- match.call()
    out$lm_betaselect$call <- object$lm_betaselect$call
    out$lm_betaselect$to_standardize <- object$lm_betaselect$to_standardize
    out$lm_betaselect$se_method <- se_method
    out$lm_betaselect$ci <- ci
    out$lm_betaselect$level <- level
    out$lm_betaselect$boot_type <- boot_type
    out$lm_betaselect$type <- type
    out$lm_betaselect$boot_pvalue_type <- boot_pvalue_type
    class(out) <- c("summary.glm_betaselect", class(out))
    if (se_method == "boot") {
        boot_out <- object$lm_betaselect$boot_out
        out$lm_betaselect$bootstrap <- length(boot_out)
        boot_est <- sapply(boot_out, function(x) {
                        x$coef_std
                      })
        boot_est_se <- apply(boot_est, 1, stats::sd, simplify = TRUE)
        out$coefficients[, "Std. Error"] <- boot_est_se
        z_values <- out$coefficients[, "Estimate"] / boot_est_se
        out$coefficients[, "z value"] <- z_values
        i <- which(colnames(out$coefficients) == "z value")
        colnames(out$coefficients)[i] <- "z value"
        if (boot_pvalue_type == "asymmetric") {
            boot_est_list <- split(boot_est, rownames(boot_est))
            boot_pvalues <- sapply(boot_est_list,
                                   est2p,
                                   h0 = 0)
          } else {
            # boot_pvalue_type == "norm"
            boot_pvalues <- stats::pnorm(abs(z_values),
                                         lower.tail = FALSE) * 2
          }
        out$coefficients[, "Pr(>|z|)"] <- boot_pvalues
        i <- which(colnames(out$coefficients) == "Pr(>|z|)")
        colnames(out$coefficients)[i] <- switch(boot_pvalue_type,
                    asymmetric = "Pr(Boot)",
                    norm = "Pr(>|z|)")
      } else {
        # se_method == "default"
        # No need to change
      }
    if (ci) {
        out_ci <- confint.glm_betaselect(object,
                                         level = level,
                                         trace = trace,
                                         test = test,
                                         method = se_method,
                                         type = type,
                                         warn = FALSE,
                                         boot_type = boot_type)
        colnames(out_ci) <- c("CI.Lower", "CI.Upper")
        i <- which(colnames(out$coefficients) == "Estimate")
        out_coef <- out$coefficients
        out_coef <- cbind(out_coef[, seq_len(i), drop = FALSE],
                          out_ci,
                          out_coef[, -seq_len(i), drop = FALSE])
        out$coefficients <- out_coef
      }
    out
  }

#' @details
#' The `print` method of
#' `summary.glm_betaselect`-class objects
#' is adapted from
#' [stdmod::print.summary.std_selected()].
#'
#' @return
#' The `print`-method of
#' `summary.glm_betaselect` is called
#' for its side effect. The object `x`
#' is returned invisibly.
#'
#' @param x The output of
#' [summary.glm_betaselect()].
#'
#' @param est_digits The number of
#' digits after the decimal to be
#' displayed for the coefficient
#' estimates, their standard errors, and
#' confidence intervals (if present).
#' Note that the values will be rounded
#' to this number of digits before
#' printing. If all digits at this
#' position are zero for all values, the
#' values may be displayed with fewer
#' digits. Note that the coefficient
#' table is printed by
#' [stats::printCoefmat()]. If some
#' numbers are vary large, the number of
#' digits after the decimal may be
#' smaller than `est_digits` due to a
#' limit on the column width.
#'
#' @param signif.stars Whether "stars"
#' (asterisks) are printed to denote
#' the level of significance achieved
#' for each coefficient. Default is
#' `TRUE`.
#'
#' @param z_digits The number of digits
#' after the decimal to be displayed for
#' the *z* or similar statistic (in the
#' column `"z value"`).
#'
#' @param show.residuals If `TRUE`,
#' a summary of the deviance residuals
#' will be printed. Default is `FALSE`.
#'
#' @param pvalue_less_than If a
#' *p*-value is less than this value, it
#' will be displayed with `"<(this
#' value)".` For example, if
#' `pvalue_less_than` is .001, the
#' default, *p*-values less than .001
#' will be displayed as `<.001`. This
#' value also determines the printout of
#' the *p*-value of the *F* statistic.
#' (This argument does what `eps.Pvalue`
#' does in [stats::printCoefmat()].)
#'
#'
#' @rdname summary.glm_betaselect
#'
#' @export
# Code duplication intentional
print.summary.glm_betaselect <- function(x,
                                         est_digits = 3,
                                         symbolic.cor = x$symbolic.cor,
                                         signif.stars = getOption("show.signif.stars"),
                                         show.residuals = FALSE,
                                         z_digits = 3,
                                         pvalue_less_than = .001,
                                         ...) {
    cat("Call to glm_betaselect():\n")
    print(x$lm_betaselect$call)
    to_standardize <- x$lm_betaselect$to_standardize
    type <- x$lm_betaselect$type
    level <- x$lm_betaselect$level
    level_str <- paste0(formatC(level * 100, digits = 1,
                                format = "f"),
                        "%")
    if (length(to_standardize) > 0) {
        tmp <- paste(to_standardize, collapse = ", ")
        tmp <- strwrap(tmp)
      } else {
        tmp <- "[Nil]"
      }
    cat("\nVariable(s) standardized:",
        tmp, "\n")

    x$coefficients[, "Estimate"] <- round(x$coefficients[, "Estimate"], est_digits)
    x$coefficients[, "Std. Error"] <- round(x$coefficients[, "Std. Error"], est_digits)
    if (x$lm_betaselect$ci) {
        x$coefficients[, "CI.Lower"] <- round(x$coefficients[, "CI.Lower"], est_digits)
        x$coefficients[, "CI.Upper"] <- round(x$coefficients[, "CI.Upper"], est_digits)
      }
    i <- match(c("t value", "z value"), colnames(x$coefficients))
    i <- i[!is.na(i)]
    x$coefficients[, i] <- round(x$coefficients[, i], z_digits)
    NextMethod(digits = est_digits,
               eps.Pvalue = pvalue_less_than,
               dig.tst = z_digits)

    cat("\n")

    tmp <- character(0)
    tmp <- c(tmp, "Note:")
    tmp <- c(tmp,
             strwrap(switch(type,
                beta = "- Results *after* standardization are reported.",
                raw = "- Results *before* standardization are reported."),
                exdent = 2))
    if (x$lm_betaselect$se_method == "boot") {
        tmp <- c(tmp,
                 strwrap("- Nonparametric bootstrapping conducted.",
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap(paste0("- The number of bootstrap samples is ",
                                x$lm_betaselect$bootstrap, "."),
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap("- Standard errors are bootstrap standard errors.",
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap("- Z values are computed by 'Estimate / Std. Error'.",
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap(switch(x$lm_betaselect$boot_pvalue_type,
                           asymmetric = "- The bootstrap p-values are asymmetric p-values by Asparouhov and Muth\u00e9n (2021).",
                           norm = "- The bootstrap p-values are based on standard normal distribution using z values."),
                         exdent = 2))
        if (x$lm_betaselect$ci) {
            boot_type_str <- switch(x$lm_betaselect$boot_type,
                               perc = "Percentile",
                               bc = "Bias-corrected")
            tmp <- c(tmp,
                     strwrap(paste0("- ",
                                   boot_type_str,
                                   " bootstrap ",
                                   level_str,
                                   " confidence interval reported."),
                             exdent = 2))
          }
      } else {
        # se_method == "ls"
        tmp <- c(tmp,
                 strwrap("- Standard errors are least-squares standard errors.",
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap("- Z values are computed by 'Estimate / Std. Error'.",
                         exdent = 2))
        tmp <- c(tmp,
                 strwrap("- P-values are usual z-test p-values.",
                         exdent = 2))
        if ((length(to_standardize) > 0) &&
            type == "beta") {
            tmp <- c(tmp,
                     strwrap(paste0("- Default standard errors, z values, p-values, and confidence intervals (if reported) ",
                                    "should not be used for coefficients involved in standardization."),
                            exdent = 2))
          }
        if (x$lm_betaselect$ci) {
            tmp <- c(tmp,
                     strwrap(paste0("- ",
                                    "Default ",
                                    level_str,
                                    " confidence interval reported."),
                             exdent = 2))
          }
      }
    cat(tmp, sep = "\n")
    invisible(x)
  }



# #' @title Extract Log-Likelihood
# #'
# #' @description Extract the
# #' log-likelihood of an `lm_betaselect`
# #' object.
# #'
# #' @details
# #' It simply passes the model with
# #' or without selected variables
# #' standardized to the method
# #' [stats::logLik.lm()]. Please refer to
# #' the help page of [stats::logLik.lm()]
# #' for details.
# #'
# #' @return
# #' It returns an object of the class
# #' `logLik`, the same object returned
# #' by [stats::logLik.lm()].
# #'
# #' @param object An `lm_betaselect`-class
# #' object.
# #'
# #' @param REML Whether the restricted
# #' log-likelihood is returned. Default
# #' is `FALSE`.
# #'
# #' @param type The model from which the
# #' log-likelihood is returned. For
# #' `"beta"` or `"standardized"`, the
# #' model is the one after selected
# #' variables standardized. For `"raw"`
# #' or `"unstandardized"`, the model is
# #' the one before standardization was
# #' done.
# #'
# #' @param ...  Optional arguments.
# #' To be passed to [stats::logLik.lm()].
# #'
# #' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
# #'
# #' @seealso [lm_betaselect()] and [stats::logLik.lm()]
# #'
# #' @examples
# #'
# #' data(data_test_mod_cat)
# #'
# #' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
# #'                            data = data_test_mod_cat,
# #'                            to_standardize = "iv")
# #' logLik(lm_beta_x)
# #' logLik(lm_beta_x, type = "raw")
# #'
# #' @export

# logLik.lm_betaselect <- function(object,
#                                  REML = FALSE,
#                                  type = c("beta", "standardized",
#                                           "raw", "unstandardized"),
#                                  ...) {
#     type <- match.arg(type)
#     type <- switch(type,
#                    beta = "beta",
#                    standardized = "beta",
#                    raw = "raw",
#                    unstandardized = "raw")
#     if (type == "beta") {
#         NextMethod(object = object,
#                    REML = REML,
#                    ...)
#       } else {
#         # type == "raw"
#         stats::logLik(object = object$lm_betaselect$ustd,
#                       REML = REML,
#                       ...)
#       }
#   }

# #' @title Extract AIC
# #'
# #' @description Extract the
# #' Akaike Information Criterion (AIC)
# #' from an `lm_betaselect` object.
# #'
# #' @details
# #' It simply passes the model with
# #' or without selected variables
# #' standardized to [stats::extractAIC()].
# #' Please refer to
# #' the help page of [stats::extractAIC()]
# #' for details.
# #'
# #' @return
# #' It returns a numeric vector of
# #' two elements, which is simply the
# #' output of [stats::extractAIC()]
# #' on the requested model.
# #'
# #' @param fit An `lm_betaselect`-class
# #' object.
# #'
# #' @param scale To be passed
# #' to [stats::extractAIC()]. See its
# #' help page for details.
# #'
# #' @param k The weight of the
# #' equivalent degrees of freedom to be
# #' used in the computation of AIC.
# #' See [stats::extractAIC()] for details.
# #'
# #' @param type The model from which the
# #' AIC is returned. For
# #' `"beta"` or `"standardized"`, the
# #' model is the one after selected
# #' variables standardized. For `"raw"`
# #' or `"unstandardized"`, the model is
# #' the one before standardization was
# #' done.
# #'
# #' @param ...  Optional arguments.
# #' To be passed to [stats::extractAIC()].
# #'
# #' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
# #'
# #' @seealso [lm_betaselect()] and [stats::extractAIC()]
# #'
# #' @examples
# #'
# #' data(data_test_mod_cat)
# #'
# #' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
# #'                            data = data_test_mod_cat,
# #'                            to_standardize = "iv")
# #' extractAIC(lm_beta_x)
# #' extractAIC(lm_beta_x, type = "raw")
# #'
# #' @export

# extractAIC.lm_betaselect <- function(fit,
#                                      scale,
#                                      k = 2,
#                                      type = c("beta", "standardized",
#                                               "raw", "unstandardized"),
#                                      ...) {
#     type <- match.arg(type)
#     type <- switch(type,
#                    beta = "beta",
#                    standardized = "beta",
#                    raw = "raw",
#                    unstandardized = "raw")
#     if (type == "beta") {
#         NextMethod()
#       } else {
#         # type == "raw"
#         my_args <- as.list(match.call())[-1]
#         my_args$fit <- fit$lm_betaselect$ustd
#         out <- do.call(stats::extractAIC,
#                        my_args)
#         return(out)
#       }
#   }

# #' @title Model Deviance
# #'
# #' @description Extract the
# #' deviance from an `lm_betaselect`
# #' object.
# #'
# #' @details
# #' It simply passes the model with
# #' or without selected variables
# #' standardized to [stats::deviance()].
# #' Please refer to
# #' the help page of [stats::deviance()]
# #' for details.
# #'
# #' @return
# #' It returns the value of the
# #' deviance of the requested model.
# #'
# #' @param object An `lm_betaselect`-class
# #' object.
# #'
# #' @param type The model from which the
# #' deviance is returned. For
# #' `"beta"` or `"standardized"`, the
# #' model is the one after selected
# #' variables standardized. For `"raw"`
# #' or `"unstandardized"`, the model is
# #' the one before standardization was
# #' done.
# #'
# #' @param ...  Optional arguments.
# #' To be passed to [stats::deviance()].
# #'
# #' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
# #'
# #' @seealso [lm_betaselect()] and [stats::deviance()]
# #'
# #' @examples
# #'
# #' data(data_test_mod_cat)
# #'
# #' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
# #'                            data = data_test_mod_cat,
# #'                            to_standardize = "iv")
# #' deviance(lm_beta_x)
# #' deviance(lm_beta_x, type = "raw")
# #'
# #' @export

# deviance.lm_betaselect <- function(object,
#                                    type = c("beta", "standardized",
#                                             "raw", "unstandardized"),
#                                    ...) {
#     type <- match.arg(type)
#     type <- switch(type,
#                    beta = "beta",
#                    standardized = "beta",
#                    raw = "raw",
#                    unstandardized = "raw")
#     if (type == "beta") {
#         NextMethod()
#       } else {
#         # type == "raw"
#         out <- stats::deviance(object$lm_betaselect$ustd,
#                                ...)
#         return(out)
#       }
#   }

# #' @title Model Fitted Values
# #'
# #' @description Extract the
# #' fitted values from an `lm_betaselect`
# #' object.
# #'
# #' @details
# #' It simply passes the model with
# #' or without selected variables
# #' standardized to [stats::fitted()].
# #' Please refer to
# #' the help page of [stats::fitted()]
# #' for details.
# #'
# #' @return
# #' It returns the fitted values of the
# #' requested model.
# #'
# #' @param object An `lm_betaselect`-class
# #' object.
# #'
# #' @param type The model from which the
# #' fitted values are returned. For
# #' `"beta"` or `"standardized"`, the
# #' model is the one after selected
# #' variables standardized. For `"raw"`
# #' or `"unstandardized"`, the model is
# #' the one before standardization was
# #' done.
# #'
# #' @param ...  Optional arguments.
# #' To be passed to [stats::fitted()].
# #'
# #' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
# #'
# #' @seealso [lm_betaselect()] and [stats::fitted()]
# #'
# #' @examples
# #'
# #' data(data_test_mod_cat)
# #'
# #' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
# #'                            data = data_test_mod_cat,
# #'                            to_standardize = "iv")
# #' fitted(lm_beta_x)
# #' fitted(lm_beta_x, type = "raw")
# #'
# #' @export

# fitted.lm_betaselect <- function(object,
#                                    type = c("beta", "standardized",
#                                             "raw", "unstandardized"),
#                                    ...) {
#     type <- match.arg(type)
#     type <- switch(type,
#                    beta = "beta",
#                    standardized = "beta",
#                    raw = "raw",
#                    unstandardized = "raw")
#     if (type == "beta") {
#         NextMethod()
#       } else {
#         # type == "raw"
#         out <- stats::fitted(object = object$lm_betaselect$ustd,
#                              ...)
#         return(out)
#       }
#   }

# #' @title Plot Diagnostics for an `lm_betaselect` Object
# #'
# #' @description Plot the diagnostics
# #' for the model before or after
# #' standardization.
# #'
# #' @details
# #' It simply passes the model with
# #' or without selected variables
# #' standardized to the `plot` method
# #' of `lm` objects.
# #' Please refer to
# #' the help page of [stats::plot.lm()]
# #' for details.
# #'
# #' ## IMPORTANT
# #'
# #' Some diagnostics that makes use
# #' of the sampling variances and
# #' covariances of coefficient estimates
# #' *may* not be applicable to the
# #' models with one or more variables
# #' standardized. Therefore, they should
# #' only be used for exploratory purpose.
# #'
# #' @return
# #' It returns `NULL`. Called for its
# #' side effects.
# #'
# #' @param x An `lm_betaselect`-class
# #' object.
# #'
# #' @param model_type The model from which the
# #' the diagnostics are plotted For
# #' `"beta"` or `"standardized"`, the
# #' model is the one after selected
# #' variables standardized. For `"raw"`
# #' or `"unstandardized"`, the model is
# #' the one before standardization was
# #' done.
# #'
# #' @param ...  Arguments
# #' to be passed to [stats::plot.lm()].
# #' Please refer to the help page of
# #' [stats::plot.lm()].
# #'
# #' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
# #'
# #' @seealso [lm_betaselect()] and [stats::plot.lm()]
# #'
# #' @examples
# #'
# #' data(data_test_mod_cat)
# #'
# #' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
# #'                            data = data_test_mod_cat,
# #'                            to_standardize = "iv")
# #' plot(lm_beta_x)
# #' plot(lm_beta_x, model_type = "raw")
# #'
# #' @export

# plot.lm_betaselect <- function(x,
#                                model_type = c("beta", "standardized",
#                                               "raw", "unstandardized"),
#                                ...) {
#     model_type <- match.arg(model_type)
#     model_type <- switch(model_type,
#                          beta = "beta",
#                          standardized = "beta",
#                          raw = "raw",
#                          unstandardized = "raw")
#     if (model_type == "beta") {
#         NextMethod()
#       } else {
#         # type == "raw"
#         args <- as.list(match.call())[-1]
#         args$x <- x$lm_betaselect$ustd
#         args$model_type <- NULL
#         do.call(utils::getS3method(f = "plot",
#                                    class = "lm"),
#                 args)
#       }
#   }

#' @title Predict Method for an 'lm_betaselect' Object
#'
#' @description Compute the predicted
#' values in a model fitted by
#' `lm_betaselect()`.
#'
#' @details
#' It simply passes the model *before*
#' or *after* selected variables
#' are standardized to the
#' `predict`-method of an `lm` object.
#'
#' ## IMPORTANT
#'
#' Some statistics, such as prediction
#' or confidence interval, which make use
#' of the sampling variances and
#' covariances of coefficient estimates
#' *may* not be applicable to the
#' models with one or more variables
#' standardized. Therefore, they should
#' only be used for exploratory purpose.
#'
#' @return
#' It returns the output of [stats::predict.lm()].
#'
#' @param object An `lm_betaselect`-class
#' object.
#'
#' @param model_type The model from which the
#' the predicted values are computed.
#' For
#' `"beta"` or `"standardized"`, the
#' model is the one after selected
#' variables standardized. For `"raw"`
#' or `"unstandardized"`, the model is
#' the one before standardization was
#' done.
#'
#' @param newdata If set to a data
#' frame, the predicted values are
#' computed using this data frame.
#' The data must be unstandardized.
#' That is, the variables are of the
#' same units as in the data frame
#' used in [lm_betaselect()]. If
#' `model_type` is `"beta"` or
#' `"standardized"`, it will be
#' standardized using the setting
#' of `to_standardize` when `object`
#' is created in [lm_betaselect()].
#'
#' @param ...  Arguments
#' to be passed to [stats::predict.lm()].
#' Please refer to the help page of
#' [stats::predict.lm()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_betaselect()] and [stats::predict.lm()]
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv")
#' predict(lm_beta_x)
#' predict(lm_beta_x, model_type = "raw")
#'
#' @export

predict.lm_betaselect <- function(object,
                                  model_type = c("beta", "standardized",
                                                "raw", "unstandardized"),
                                  newdata,
                                  ...) {
    model_type <- match.arg(model_type)
    model_type <- switch(model_type,
                         beta = "beta",
                         standardized = "beta",
                         raw = "raw",
                         unstandardized = "raw")
    to_standardize <- object$lm_betaselect$to_standardize
    if (model_type == "beta") {
        if (!missing(newdata)) {
            newdata_std <- std_data(newdata,
                                to_standardize = to_standardize)
            NextMethod(newdata = newdata_std)
          } else {
            NextMethod()
          }
      } else {
        object <- object$lm_betaselect$ustd
        NextMethod()
      }
  }

#' @title Predict Method for a 'glm_betaselect' Object
#'
#' @description Compute the predicted
#' values in a model fitted by
#' [glm_betaselect()].
#'
#' @details
#' It simply passes the model *before*
#' or *after* selected variables
#' are standardized to the
#' `predict`-method of a `glm` object.
#'
#' ## IMPORTANT
#'
#' Some statistics, such as prediction
#' or confidence interval, which make use
#' of the sampling variances and
#' covariances of coefficient estimates
#' *may* not be applicable to the
#' models with one or more variables
#' standardized. Therefore, they should
#' only be used for exploratory purpose.
#'
#' @return
#' It returns the output of [stats::predict.glm()].
#'
#' @param object A `glm_betaselect`-class
#' object.
#'
#' @param model_type The model from which the
#' the predicted values are computed.
#' For
#' `"beta"` or `"standardized"`, the
#' model is the one after selected
#' variables standardized. For `"raw"`
#' or `"unstandardized"`, the model is
#' the one before standardization was
#' done.
#'
#' @param newdata If set to a data
#' frame, the predicted values are
#' computed using this data frame.
#' The data must be unstandardized.
#' That is, the variables are of the
#' same units as in the data frame
#' used in [glm_betaselect()]. If
#' `model_type` is `"beta"` or
#' `"standardized"`, it will be
#' standardized using the setting
#' of `to_standardize` when `object`
#' is created in [glm_betaselect()].
#'
#' @param ...  Arguments
#' to be passed to [stats::predict.glm()].
#' Please refer to the help page of
#' [stats::predict.glm()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [glm_betaselect()] and [stats::predict.glm()]
#'
#' @examples
#'
#' data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
#' data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
#'                               yes = 1,
#'                               no = 0)
#' logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
#'                                   data = data_test_mod_cat,
#'                                   family = binomial,
#'                                   to_standardize = "iv")
#'
#' predict(logistic_beta_x)
#' predict(logistic_beta_x, model_type = "raw")
#'
#' @export

predict.glm_betaselect <- function(object,
                                   model_type = c("beta", "standardized",
                                                 "raw", "unstandardized"),
                                   newdata,
                                   ...) {
    model_type <- match.arg(model_type)
    model_type <- switch(model_type,
                         beta = "beta",
                         standardized = "beta",
                         raw = "raw",
                         unstandardized = "raw")
    to_standardize <- object$lm_betaselect$to_standardize
    if (model_type == "beta") {
        if (!missing(newdata)) {
            newdata_std <- std_data(newdata,
                                to_standardize = to_standardize)
            NextMethod(newdata = newdata_std)
          } else {
            NextMethod()
          }
      } else {
        object <- object$lm_betaselect$ustd
        NextMethod()
      }
  }

# #' @title Update and Re-fit a Call to
# #' 'glm_betaselect()'
# #'
# #' @description The `update`-method
# #' for a `glm_betaselect`-class objects.
# #'
# #' @details This works in the same way
# #' the default `update`-method does for
# #' the output of [stats::glm()].
# #'
# #' @return
# #' It returns the output of
# #' [glm_betaselect()] with the updated
# #' call, such as the updated model.
# #'
# #' @param object An `glm_betaselect`-class
# #' object.
# #'
# #' @param formula. Changes to the formula,
# #' as in the default [update()] method.
# #'
# #' @param ...  For [update.lm_betaselect()],
# #' additional arguments
# #' to the call, as in the default
# #' [update()] method. Ignored by
# #' [getCall.glm_betaselect()].
# #'
# #' @param evaluate Whether the updated
# #' call will be evaluated. Default is
# #' `TRUE`.
# #'
# #' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
# #'
# #' @seealso [glm_betaselect()] and [stats::update()]
# #'
# #' @examples
# #'
# #' data(data_test_mod_cat)
# #'
# #' data_test_mod_cat$p <- ifelse(data_test_mod_cat$dv >
# #'                               mean(data_test_mod_cat$dv),
# #'                               yes = 1,
# #'                               no = 0)
# #' logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1,
# #'                                   data = data_test_mod_cat,
# #'                                   to_standardize = "iv")
# #' summary(logistic_beta_x)
# #' logistic_beta_x2 <- update(logistic_beta_x, ~ . + cat1)
# #' summary(logistic_beta_x)
# #'
# #' @export

# update.glm_betaselect <- function(object,
#                                   formula.,
#                                   ...,
#                                   evaluate = TRUE) {
#     # Adapted from the default update
#     # By default, get the call to lm_betaselect()
#     # call <- object$lm_betaselect$call

#     # extras <- match.call(expand.dots = FALSE)$...
#     # if (!missing(formula.))
#     #     call$formula <- stats::update(stats::formula(object), formula.)
#     # if (length(extras)) {
#     #     existing <- !is.na(match(names(extras), names(call)))
#     #     for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
#     #     if (any(!existing)) {
#     #         call <- c(as.list(call), extras[!existing])
#     #         call <- as.call(call)
#     #     }
#     # }
#     # if (evaluate)
#     #     eval(call, parent.frame())
#     # else call
#   }

#' @title Call in an
#' 'lm_betaselect' or 'glm_betaselect'
#' Object
#'
#' @description The `getCall`-method
#' for an `lm_betaselect`-class or
#' `glm_betaselectd`-class objects.
#'
#' @details This works in the same way
#' the default `getCall`-method does for
#' the outputs of [stats::lm()]
#' and [stats::glm()].
#'
#' @return
#' It returns the call requested.
#'
#' @param x An `lm_betaselect`-class
#' or `glm_betaselect`-class
#' object from which the call is to
#' be extracted.
#'
#' @param what Which call to extract.
#' For `"lm_betaselect"` or
#' `"glm_betaselect"` the call
#' to [lm_betaselect()]
#' or [glm_betaselect()] is extracted.
#' For
#' `"beta"` or `"standardized"`, the
#' call used to fit the model *after*
#' selected variables standardized
#' is extracted.
#' For `"raw"`
#' or `"unstandardized"`, the call used
#' to fit hte model *before* standardization
#' is extracted.
#'
#' @param ...  Additional arguments.
#' Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_betaselect()],
#' [glm_betaselect()], and [stats::getCall()]
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv")
#' getCall(lm_beta_x)
#' getCall(lm_beta_x, what = "beta")
#' getCall(lm_beta_x, what = "raw")
#'
#' @importFrom stats getCall
#' @export

getCall.lm_betaselect <- function(x,
                                  what = c("lm_betaselect",
                                           "beta",
                                           "standardized",
                                           "raw",
                                           "unstandardized"),
                                  ...) {
    what <- match.arg(what)
    what <- switch(what,
                   lm_betaselect = "lm_betaselect",
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    out <- switch(what,
                  lm_betaselect = x$lm_betaselect$call,
                  beta = x$call,
                  raw = x$lm_betaselect$ustd$call)
    return(out)
  }

#' @rdname getCall.lm_betaselect
#' @export

getCall.glm_betaselect <- function(x,
                                  what = c("glm_betaselect",
                                           "beta",
                                           "standardized",
                                           "raw",
                                           "unstandardized"),
                                  ...) {
    what <- match.arg(what)
    what <- switch(what,
                   glm_betaselect = "lm_betaselect",
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    out <- switch(what,
                  lm_betaselect = x$lm_betaselect$call,
                  beta = x$call,
                  raw = x$lm_betaselect$ustd$call)
    return(out)
  }
