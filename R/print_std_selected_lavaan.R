#' @title Print a 'lav_betaselect' Object
#'
#' @description Print method for an
#' 'lav_betaselect' object, which
#' is the output of
#' [lav_betaselect()].
#'
#' @details
#' The default format of the printout
#' is that of [lavaan::parameterEstimates()],
#' which is compact but not easy to
#' read. Users can request a format
#' similar to that of the printout
#' of the summary of a `lavaan` output
#' by setting `output` to `"text"`.
#'
#' For the `"text"` format, users can
#' also select whether
#' only the standardized solution is
#' printed or whether
#' the standardized solution is appended
#' to the right of the printout, as
#' in the output of [summary()]
#' for a `lavaan` output.
#'
#' @param x Object of the class
#' `std_solution_boot`.
#'
#' @param ... Optional arguments to be
#' passed to [print()] methods.
#'
#' @param nd The number of digits
#' after the decimal place. Default
#' is 3.
#'
#' @param output String. How the results
#' are printed. Default is `"table"` and
#' the results are printed in a table
#' format similar to that of
#' [lavaan::parameterEstimates()]
#' with `output` set to `"table"`.
#' If `"text"`, the results will be
#' printed in a text format similar to
#' the printout of the output of
#' [summary()] of
#' a [lavaan-class] object.
#'
#' @param standardized_only Logical.
#' If `TRUE`, only the
#' results for the standardized solution
#' will be printed. If `FALSE`,
#' the default, then
#' the standardized solution is printed
#' alongside the unstandardized solution,
#' as in the printout of the output
#' of [summary()] of a [lavaan-class]
#' object.
#'
#' @seealso [lav_betaselect()]. This
#' function is adapted from
#' [semhelpinghands::print.std_solution_boot()].
#'
#' @examples
#' \donttest{
#' # TO ADD
#' }
#'
#' @return
#'  `x` is returned invisibly. Called for its side effect.
#'
#' @author Shu Fai Cheung
#' <https://orcid.org/0000-0002-9871-9448>
#'
#' @export

print.lav_betaselect <- function(x,
                                 ...,
                                 nd = 3,
                                 output = c("table", "text"),
                                 standardized_only = FALSE) {
    output <- match.arg(output)
    x_call <- attr(x, "call")
    if (output == "table") {
        # Force the table format
        tmp <- class(x)
        tmp2 <- match("lavaan.parameterEstimates", tmp)
        if (!is.na(tmp2)) {
            tmp <- tmp[-tmp2]
          }
        class(x) <- tmp
        NextMethod()
        return(invisible(x))
      }

    tmp <- stats::na.omit(x$std.p.by)
    tmp <- unique(unlist(strsplit(tmp, ",", fixed = TRUE)))
    std.p.by.vars <- tmp

    # Add columns required for print
    ptable <- attr(x, "partable")
    est0 <- attr(x, "est")
    est1 <- x
    est1$id <- seq_len(nrow(est1))
    i0 <- colnames(x) %in% c("se", "z", "pvalue",
                             "ci.lower", "ci.upper")
    # est1 <- merge(est1,
    #               x[, !i0])
    i0 <- colnames(ptable) %in% c("est", "se",
                                  "user", "free",
                                  "ustart", "plabel",
                                  "start",
                                  "id")
    est1 <- merge(est1, ptable[, !i0])
    est1 <- est1[order(est1$id), ]
    est1$id <- NULL

    # Move std.p.by to the right
    i <- match("std.p.by", colnames(est1))
    est1 <- cbind(est1[, -i], est1[, i, drop = FALSE])

    class(est1) <- class(est0)
    pe_attrib <- attr(x, "pe_attrib")
    tmp <- !(names(pe_attrib) %in% names(attributes(est1)))
    attributes(est1) <- c(attributes(est1),
                          pe_attrib[tmp])
    if (!inherits(est1, "lavaan.parameterEstimates")) {
        class(est1) <- c("lavaan.parameterEstimates", class(est1))
      }
    header_stdp <- character(0)
    header_stdp <- c(header_stdp,
                     "Selected Standardization (Bs):",
                     "",
                     "  Only selected variables are standardized")
    # TODO:
    # - Get the width from the output
    std_se <- attr(x, "std_se")
    header_width <- 54
    if ("std.p.se" %in% colnames(est1)) {
        tmp1 <- "  Standard errors"
        tmp2 <- switch(std_se,
                       delta = "Delta Method",
                       bootstrap = "Bootstrap")
        header_stdp <- c(header_stdp,
                         paste0(tmp1,
                                paste0(rep(" ",
                                           header_width - nchar(tmp1) - nchar(tmp2)),
                                           collapse = ""),
                                tmp2))
        if ("bootstrap" %in% std_se) {
            tmp1 <- "  Bootstrap samples"
            tmp2 <- as.character(attr(x, "R"))
            header_stdp <- c(header_stdp,
                            paste0(tmp1,
                                    paste0(rep(" ",
                                                header_width - nchar(tmp1) - nchar(tmp2)),
                                                collapse = ""),
                                    tmp2))
          }
      }

    footer_stdp <- character(0)
    footer_stdp <- c(footer_stdp,
                     "Note:")
    footer_stdp <- c(footer_stdp,
                     strwrap(paste("- The column 'Bs.by' lists variable(s) standardized when computing the",
                                    "standardized coefficient of a parameter.",
                                    "('NA' for user-defined parameters because they are computed from other standardized parameters.)"),
                             exdent = 2))
    if (length(std.p.by.vars) > 0) {
        if (length(std.p.by.vars) == 1) {
            footer_stdp <- c(footer_stdp,
                            strwrap(paste("- This variable is standardized:",
                                          std.p.by.vars),
                                    exdent = 2))
          } else {
            footer_stdp <- c(footer_stdp,
                            strwrap(paste("- These variables are standardized:",
                                          paste0(std.p.by.vars, collapse = ", ")),
                                    exdent = 2))
          }
      }
    prods <- attr(x, "prods")
    if (length(prods) > 0) {
        footer_stdp <- c(footer_stdp,
                        strwrap(paste0("- Product terms (",
                                      paste0(names(prods), collapse = ", "),
                                      ") have variables standardized before computing them. ",
                                      "That is, the product terms themselves are not standardized."),
                                exdent = 2))
      }
    cat_vars <- attr(x, "categorical")
    if (length(cat_vars)) {
        footer_stdp <- c(footer_stdp,
                        strwrap(paste0("- Dummy variables (",
                                paste0(cat_vars, collapse = ", "),
                                ") which are predictors are not standardied."),
                                exdent = 2))
      }
    if ("delta" %in% std_se) {
        footer_stdp <- c(footer_stdp,
                         strwrap(paste("- Delta method standard errors, p-values, and confidence intervals are",
                                       "not recommended for parameters which have variable(s) standardized."),
                                 exdent = 2))
      }

    # if (("std.ci.lower" %in% colnames(est1)) &&
    #     ("bootstrap" %in% std_se)) {

    #   }
    comps <- c("Latent Variables:",
                "Regressions:",
                "Covariances:",
                "Variances:")
    if (!standardized_only) {
        tmp <- colnames(est1)
        # tmp[tmp == "std.p"] <- "Std.p"
        # tmp[tmp == "std.p.ci.lower"] <- "Std.p.ci.lower"
        # tmp[tmp == "std.p.ci.upper"] <- "Std.p.ci.upper"
        # tmp[tmp == "std.p.se"] <- "Std.p.SE"
        # tmp[tmp == "std.p.z"] <- "Std.p.z"
        # tmp[tmp == "std.p.pvalue"] <- "Std.p.Pvalue"
        # tmp[tmp == "std.p.by"] <- "Std.by.Vars"
        tmp <- gsub("std.p.", "Bs.", tmp, fixed = TRUE)
        tmp <- gsub("std.p", "Bs", tmp, fixed = TRUE)
        tmp <- gsub("ci.lower", "CI.Lo", tmp, fixed = TRUE)
        tmp <- gsub("ci.upper", "CI.Hi", tmp, fixed = TRUE)
        colnames(est1) <- tmp
        out <- utils::capture.output(print(est1, ..., nd = nd))
        i <- sort(match(comps, out))[1]
        out <- append(out,
                      c(header_stdp, ""),
                      after = i - 1)
        out <- c(out, footer_stdp)
        cat(out, sep = "\n")
        return(invisible(x))
      } else {
        level <- attr(x, "level")
        est2 <- est1
        est2$est <- est2$std.p
        est2$se <- est2$std.p.se
        est2$z <- est2$std.p.z
        est2$pvalue <- est2$std.p.pvalue
        est2$ci.lower <- est2$std.p.ci.lower
        est2$ci.upper <- est2$std.p.ci.upper
        est2$std.p <- NULL
        est2$std.p.se <- NULL
        est2$std.p.z <- NULL
        est2$std.p.pvalue <- NULL
        est2$std.p.ci.lower <- NULL
        est2$std.p.ci.upper <- NULL

        tmp <- colnames(est2)
        tmp <- gsub("std.p.", "Bs.", tmp, fixed = TRUE)
        # tmp <- gsub("std.p", "Bs", tmp, fixed = TRUE)
        # tmp <- gsub("ci.lower", "CI.Lo", tmp, fixed = TRUE)
        # tmp <- gsub("ci.upper", "CI.Hi", tmp, fixed = TRUE)
        colnames(est2) <- tmp

        out <- utils::capture.output(print(est2, nd = nd))
        i <- which(grepl("Parameter Estimates:", out, fixed = TRUE))
        j <- sort(match(comps, out))[1]
        out <- out[-c(i:max(i, (j - 2)))]
        i <- which(out == "")[1]
        header_stdp[i] <- "Selected Standardized Estimates Only (std.p):"
        out <- append(out,
                      header_stdp,
                      after = i)
        out <- gsub("         Estimate ",
                    "Standardized (Bs) ",
                    out)
        out <- gsub("P(>|z|)",
                    "p-value",
                    out,
                    fixed = TRUE)
        out <- c(out, footer_stdp)
        cat(out, sep = "\n")
        return(invisible(x))
      }
  }