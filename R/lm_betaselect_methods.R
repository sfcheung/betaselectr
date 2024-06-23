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