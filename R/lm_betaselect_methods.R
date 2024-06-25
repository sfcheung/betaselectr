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
                        "using OLS or WLS should not be used.")
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

#' @title Extract Log-Likelihood
#'
#' @description Extract the
#' log-likelihood of a `lm_betaselect`
#' object.
#'
#' @details
#' It simply passes the model with
#' or without selected variables
#' standardized to the method
#' [stats::logLik.lm()]. Please refer to
#' the help page of [stats::logLik.lm()]
#' for details.
#'
#' @return
#' It returns an object of the class
#' `logLik`, the same object returned
#' by [stats::logLik.lm()].
#'
#' @param object A `lm_betaselect`-class
#' object.
#'
#' @param REML Whether the restricted
#' log-likelihood is returned. Default
#' is `FALSE`.
#'
#' @param type The model from which the
#' log-likelihood is returned. For
#' `"beta"` or `"standardized"`, the
#' model is the one after selected
#' variables standardized. For `"raw"`
#' or `"unstandardized"`, the model is
#' the one before standardization was
#' done.
#'
#' @param ...  Optional arguments.
#' To be passed to [stats::logLik.lm()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_betaselect()] and [stats::logLik.lm()]
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv")
#' logLik(lm_beta_x)
#' logLik(lm_beta_x, type = "raw")
#'
#' @export

logLik.lm_betaselect <- function(object,
                                 REML = FALSE,
                                 type = c("beta", "standardized",
                                          "raw", "unstandardized"),
                                 ...) {
    type <- match.arg(type)
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    if (type == "beta") {
        NextMethod(object = object,
                   REML = REML,
                   ...)
      } else {
        # type == "raw"
        stats::logLik(object = object$lm_betaselect$ustd,
                      REML = REML,
                      ...)
      }
  }

#' @title Extract AIC
#'
#' @description Extract the
#' Akaike Information Criterion (AIC)
#' from a `lm_betaselect` object.
#'
#' @details
#' It simply passes the model with
#' or without selected variables
#' standardized to [stats::extractAIC()].
#' Please refer to
#' the help page of [stats::extractAIC()]
#' for details.
#'
#' @return
#' It returns a numeric vector of
#' two elements, which is simply the
#' output of [stats::extractAIC()]
#' on the requested model.
#'
#' @param fit A `lm_betaselect`-class
#' object.
#'
#' @param scale To be passed
#' to [stats::extractAIC()]. See its
#' help page for details.
#'
#' @param k The weight of the
#' equivalent degrees of freedom to be
#' used in the computation of AIC.
#' See [stats::extractAIC()] for details.
#'
#' @param type The model from which the
#' AIC is returned. For
#' `"beta"` or `"standardized"`, the
#' model is the one after selected
#' variables standardized. For `"raw"`
#' or `"unstandardized"`, the model is
#' the one before standardization was
#' done.
#'
#' @param ...  Optional arguments.
#' To be passed to [stats::extractAIC()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_betaselect()] and [stats::extractAIC()]
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv")
#' extractAIC(lm_beta_x)
#' extractAIC(lm_beta_x, type = "raw")
#'
#' @export

extractAIC.lm_betaselect <- function(fit,
                                     scale,
                                     k = 2,
                                     type = c("beta", "standardized",
                                              "raw", "unstandardized"),
                                     ...) {
    type <- match.arg(type)
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    if (type == "beta") {
        NextMethod()
      } else {
        # type == "raw"
        my_args <- as.list(match.call())[-1]
        my_args$fit <- fit$lm_betaselect$ustd
        out <- do.call(stats::extractAIC,
                       my_args)
        return(out)
      }
  }

#' @title Model Deviance
#'
#' @description Extract the
#' deviance from a `lm_betaselect`
#' object.
#'
#' @details
#' It simply passes the model with
#' or without selected variables
#' standardized to [stats::deviance()].
#' Please refer to
#' the help page of [stats::deviance()]
#' for details.
#'
#' @return
#' It returns the value of the
#' deviance of the requested model.
#'
#' @param object A `lm_betaselect`-class
#' object.
#'
#' @param type The model from which the
#' deviance is returned. For
#' `"beta"` or `"standardized"`, the
#' model is the one after selected
#' variables standardized. For `"raw"`
#' or `"unstandardized"`, the model is
#' the one before standardization was
#' done.
#'
#' @param ...  Optional arguments.
#' To be passed to [stats::deviance()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_betaselect()] and [stats::deviance()]
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv")
#' deviance(lm_beta_x)
#' deviance(lm_beta_x, type = "raw")
#'
#' @export

deviance.lm_betaselect <- function(object,
                                   type = c("beta", "standardized",
                                            "raw", "unstandardized"),
                                   ...) {
    type <- match.arg(type)
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    if (type == "beta") {
        NextMethod()
      } else {
        # type == "raw"
        out <- stats::deviance(object$lm_betaselect$ustd,
                               ...)
        return(out)
      }
  }

#' @title Model Fitted Values
#'
#' @description Extract the
#' fitted values from a `lm_betaselect`
#' object.
#'
#' @details
#' It simply passes the model with
#' or without selected variables
#' standardized to [stats::fitted()].
#' Please refer to
#' the help page of [stats::fitted()]
#' for details.
#'
#' @return
#' It returns the fitted values of the
#' requested model.
#'
#' @param object A `lm_betaselect`-class
#' object.
#'
#' @param type The model from which the
#' fitted values are returned. For
#' `"beta"` or `"standardized"`, the
#' model is the one after selected
#' variables standardized. For `"raw"`
#' or `"unstandardized"`, the model is
#' the one before standardization was
#' done.
#'
#' @param ...  Optional arguments.
#' To be passed to [stats::fitted()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_betaselect()] and [stats::fitted()]
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv")
#' fitted(lm_beta_x)
#' fitted(lm_beta_x, type = "raw")
#'
#' @export

fitted.lm_betaselect <- function(object,
                                   type = c("beta", "standardized",
                                            "raw", "unstandardized"),
                                   ...) {
    type <- match.arg(type)
    type <- switch(type,
                   beta = "beta",
                   standardized = "beta",
                   raw = "raw",
                   unstandardized = "raw")
    if (type == "beta") {
        NextMethod()
      } else {
        # type == "raw"
        out <- stats::fitted(object = object$lm_betaselect$ustd,
                             ...)
        return(out)
      }
  }

#' @title Plot Diagnostics for an `lm_betaselect` Object
#'
#' @description Plot the diagnostics
#' for the model before or after
#' standardization.
#'
#' @details
#' It simply passes the model with
#' or without selected variables
#' standardized to the `plot` method
#' of `lm` objects.
#' Please refer to
#' the help page of [stats::plot.lm()]
#' for details.
#'
#' ## IMPORTANT
#'
#' Some diagnostics that makes use
#' of the sampling variances and
#' covariances of coefficient estimates
#' *may* not be applicable to the
#' models with one or more variables
#' standardized. Therefore, they should
#' only be used for exploratory purpose.
#'
#' @return
#' It returns `NULL`. Called for its
#' side effects.
#'
#' @param x A `lm_betaselect`-class
#' object.
#'
#' @param model_type The model from which the
#' the diagnostics are plotted For
#' `"beta"` or `"standardized"`, the
#' model is the one after selected
#' variables standardized. For `"raw"`
#' or `"unstandardized"`, the model is
#' the one before standardization was
#' done.
#'
#' @param ...  Arguments
#' to be passed to [stats::plot.lm()].
#' Please refer to the help page of
#' [stats::plot.lm()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_betaselect()] and [stats::plot.lm()]
#'
#' @examples
#'
#' data(data_test_mod_cat)
#'
#' lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
#'                            data = data_test_mod_cat,
#'                            to_standardize = "iv")
#' plot(lm_beta_x)
#' plot(lm_beta_x, model_type = "raw")
#'
#' @export

plot.lm_betaselect <- function(x,
                               model_type = c("beta", "standardized",
                                              "raw", "unstandardized"),
                               ...) {
    model_type <- match.arg(model_type)
    model_type <- switch(model_type,
                         beta = "beta",
                         standardized = "beta",
                         raw = "raw",
                         unstandardized = "raw")
    if (model_type == "beta") {
        NextMethod()
      } else {
        # type == "raw"
        args <- as.list(match.call())[-1]
        args$x <- x$lm_betaselect$ustd
        args$model_type <- NULL
        do.call(utils::getS3method(f = "plot",
                                   class = "lm"),
                args)
      }
  }