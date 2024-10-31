#' @title Coefficients of a 'lav_betaselect'-Class Object
#'
#' @description Return the betas-select
#' in a 'lav_betaselect'-class object.
#
#' @details It just extracts and
#' returns the column `est` from
#' the object: the betas-select, with
#' selected variables standardized.
#'
#' @return
#' A numeric vector: The betas-select
#' in the object. The names of parameters
#' follow the convention in `lavaan`.
#'
#' @param object The output of
#' [lav_betaselect()].
#'
#' @param drop_na Logical. Whether betas-select
#' with `NA` are dropped. Default is
#' `FALSE`.
#'
#' @param ...  Optional arguments. Not
#' used.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lav_betaselect()]
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
#' coef(fit_beta)
#'
#' @export

coef.lav_betaselect <- function(object,
                                drop_na = FALSE,
                                ...) {
    pnames <- lavaan::lav_partable_labels(object)
    out <- object$std.p
    names(out) <- pnames
    if (drop_na) {
        out <- out[!is.na(out)]
      }
    out
  }