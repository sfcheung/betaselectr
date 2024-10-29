#' @title Confidence Interval for a
#' 'lav_betaselect'-Class Object
#'
#' @description Return the confidence
#' intervals of betas-select in the
#' output of [lav_betaselect()].
#'
#' @details Details
#' (Include subjects for verbs.)
#' (Use 3rd person forms for verbs.)
#'
#' @return
#' A two-column matrix of the confidence
#' intervals.
#'
#' @param object The output of [lav_betaselect()].
#'
#' @param parm
#' Ignored due to the complexity in the
#' naming. The confidence intervals
#' of all parameters are always
#' returned.
#'
#' @param level The level of confidence.
#' Ignored because the intervals should be
#' formed
#' when calling [lav_betaselect()].
#'
#' @param ...  Optional arguments.
#' Ignored.
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
#' confint(fit_beta)
#'
#' @export

confint.lav_betaselect <- function(object,
                                   parm,
                                   level = .95,
                                   ...) {
    pnames <- lavaan::lav_partable_labels(object)
    out <- matrix(NA, nrow = length(pnames), ncol = 2)
    rownames(out) <- pnames
    if (isFALSE("std.p.ci.lower" %in% colnames(object))) {
        return(out)
      }
    out[, 1] <- object$std.p.ci.lower
    out[, 2] <- object$std.p.ci.upper
    level <- attr(object, "level")

    # Borrowed from stats::confint()
    probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
    cnames <- paste(format(100 * probs,
                           trim = TRUE,
                           scientific = FALSE,
                           digits = 2), "%")
    colnames(out) <- cnames
    out
  }