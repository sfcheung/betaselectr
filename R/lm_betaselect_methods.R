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