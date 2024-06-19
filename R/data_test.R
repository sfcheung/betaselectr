#' @title Test Dataset with Moderator
#' and Mediator
#'
#' @description This dataset has one
#' mediator, one moderator, one
#' independent variable, one dependent
#' variable, and two control variables.
#'
#' @format A data frame with 200 rows
#' and five variables:
#' \describe{
#'   \item{dv}{Dependent variable, continuous}
#'   \item{iv}{Independent variable, continuous}
#'   \item{mod}{Moderator, continuous}
#'   \item{med}{Mediator, continuous}
#'   \item{cov1}{Control variable, continuous}
#'   \item{cov2}{Control variable, continuous}
#' }
#'
#' @examples
#'
#' library(lavaan)
#' mod <-
#' "
#' med ~ iv + mod + iv:mod + cov1 + cov2
#' dv ~ med + iv + cov1 + cov2
#' "
#' fit <- sem(mod,
#'            data_test_medmod)
#' summary(fit)
#'
#'
"data_test_medmod"

#' @title Test Dataset with Moderator
#' and Categorical Variables
#'
#' @description This dataset has one
#' predictor, one moderator, one
#' control variable, one dependent
#' variable, and a categorical variable.
#'
#' @format A data frame with 500 rows
#' and five variables:
#' \describe{
#'   \item{dv}{Dependent variable, continuous}
#'   \item{iv}{Independent variable, continuous}
#'   \item{mod}{Moderator, continuous}
#'   \item{cov1}{Control variable, continuous}
#'   \item{cat1}{String variable with these values: "gp1", "gp2", and "gp3"}
#' }
#'
#' @examples
#'
#' lm_out <- lm(dv ~ iv * mod + cov1 + cat1, data_test_mod_cat)
#' summary(lm_out)
#'
#'
"data_test_mod_cat"