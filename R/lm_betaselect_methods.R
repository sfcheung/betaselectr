#' @title Coefficients of a
#' 'lm_betaselect'-Class Object
#'
#' @description Return the estimates of
#' coefficients in a
#' 'lm_betaselect'-class object.
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
#' [lm_betaselect()], or a
#' `lm_betaselect`-class object.
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
#' @seealso [lm_betaselect()]
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

#' @title The 'vcov' Method for a
#' 'lm_betaselect'-Class Object
#'
#' @description Compute the
#' variance-covariance matrix of
#' estimates in the output of
#' [lm_betaselect()].
#'
#' @details The type of
#' variance-covariance matrix depends
#' on the object. If bootstrapping
#' was requested, by default it returns
#' the bootstrap variance-covariance
#' matrix. Otherwise, it returns the
#' OLS (or WLS) variance-covariance
#' matrix and raises a warning.
#'
#' Support for other type of
#' variance-covariance matrix will be
#' added.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @return
#' A matrix of the variances and
#' covariances of the parameter
#' estimates.
#'
#' @param object The output of
#' [lm_betaselect()]
#' or am `lm_betaselect`-class object.
#'
#' @param method The method used to
#' compute the variance-covariance
#' matrix. If bootstrapping was
#' requested when calling
#' [lm_betaselect()] and this argument
#' is set to `"bootstrap"` or `"boot"`,
#' the bootstrap variance-covariance
#' matrix is returned. If bootstrapping
#' was not requested or if this argument
#' is set to `"ls"`, then the usual `lm`
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
#' vcov(lm_beta_x, method = "ls")
#' vcov(lm_beta_x, type = "raw")
#'
#'
#' @export
# Adapted from vcov.std_selected()

vcov.lm_betaselect <- function(object,
                               method = c("boot", "bootstrap", "ls"),
                               type = c("beta",
                                        "standardized",
                                        "raw",
                                        "unstandardized"),
                               warn = TRUE,
                               ...) {
    method <- match.arg(method)
    type <- match.arg(type)
    if (method %in% c("boot", "bootstrap")) {
        method <- "boot"
      }
    if (identical(method, "boot") && is.null(object$lm_betaselect$boot_out)) {
        stop("Bootstrap estimates not available. Maybe bootstrapping not requested?")
      }
    if (type %in% c("beta", "standardized")) {
        if (method %in% c("boot", "bootstrap")) {
            boot_out <- object$lm_betaselect$boot_out
            boot_est <- sapply(boot_out, function(x) {
                            x$coef_std
                          })
            out <- stats::cov(t(boot_est))
            return(out)
          } else {
            if (warn) {
                warning("With standardization, the variance-covariance matrix ",
                        "using OLS or WLS should not be used.")
              }
            NextMethod()
          }
      } else {
        if (method %in% c("boot", "bootstrap")) {
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

#' @title Confidence Interval for a
#' 'lm_betaselect'-Class Object
#'
#' @description Return the confidence
#' interval of the regression
#' coefficients in the output of
#' [lm_betaselect()].
#'
#' @details
#' The type of
#' confidence intervals depends
#' on the object. If bootstrapping
#' was requested, by default it returns
#' the percentile bootstrap confidence
#' intervals. Otherwise, it returns the
#' OLS (or WLS) confidence intervals
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
#' [lm_betaselect()].
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
    if (method %in% c("boot", "bootstrap")) {
        method <- "boot"
      }
    if (identical(method, "boot") && is.null(object$lm_betaselect$boot_out)) {
        stop("Bootstrap estimates not available. Maybe bootstrapping not requested?")
      }
    if (type %in% c("beta", "standardized")) {
        if (method %in% c("boot", "bootstrap")) {1
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
        if (method %in% c("boot", "bootstrap")) {
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

#' @title ANOVA Tables for a
#' 'lm_betaselect'-Class Object
#'
#' @description Return the analysis
#' of variance tables for
#' the outputs of
#' [lm_betaselect()].
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
#' [lm_betaselect()].
#'
#' @param ...  Additional outputs
#' of [lm_betaselect()].
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
    if (type %in% c("beta", "standardized")) {
        NextMethod()
      } else {
        objects <- c(list(object), list(...))
        ustds <- lapply(objects, function(x) x$lm_betaselect$ustd)
        out <- do.call(stats::anova,
                       ustds)
        return(out)
      }
  }

#' @title Summary of a
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
#' the *p*-value using a normal distribution.
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
#' @describeIn summary.lm_betaselect The summary method for a `lm_betaselect`-class object.
#'
#' @export

summary.lm_betaselect <- function(object,
                                  correlation = FALSE,
                                  symbolic.cor = FALSE,
                                  se_method = c("boot", "bootstrap", "t", "lm", "ls"),
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
    boot_type <- match.arg(boot_type)
    boot_pvalue_type <- match.arg(boot_pvalue_type)
    if (se_method %in% c("t", "lm", "ls")) {
        se_method <- "ls"
      }
    if (se_method %in% c("boot", "bootstrap")) {
        se_method <- "boot"
      }
    if (type %in% c("beta", "standardized")) {
        type <- "beta"
      }
    if (type %in% c("raw", "unstandardized")) {
        type <- "raw"
      }
    if (identical(se_method, "boot") && is.null(object$lm_betaselect$boot_out)) {
        stop("Bootstrap estimates not available. Maybe bootstrapping not requested?")
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
    out$lm_betaselect$to_standardized <- object$lm_betaselect$to_standardized
    out$lm_betaselect$se_method <- se_method
    out$lm_betaselect$ci <- ci
    out$lm_betaselect$boot_type <- boot_type
    out$lm_betaselect$type <- type
    out$lm_betaselect$boot_pvalue_type <- boot_pvalue_type
    class(out) <- c("summary.lm_betaselect", class(out))
    if (se_method == "boot") {
        boot_out <- object$lm_betaselect$boot_out
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

