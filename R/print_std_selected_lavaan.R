#' @title Print a 'lav_betaselect' Object
#'
#' @description Print method for a
#' 'lav_betaselect' object, which
#' is the output of
#' [lav_betaselect()].
#'
#' @details
#' The default format of the printout,
#' `"lavaan.printer"`,
#' is similar to that of the `summary()`
#' of a `lavaan` object.
#' Users can also select whether
#' only the standardized solution is
#' printed or whether
#' the standardized solution is appended
#' to the right of the printout.
#'
#' If `output` is set to `"table"'
#' the format is that of
#' [lavaan::parameterEstimates()]
#' with `output = "data.frame"`,
#' which is compact but not easy to
#' read.
#'
#' @param x A `lav_betaselect`-class
#' object, such as the output of
#' [lav_betaselect()].
#'
#' @param ... Optional arguments to be
#' passed to [print()] methods.
#'
#' @param nd The number of digits
#' after the decimal place. Default
#' is 3.
#'
#' @param output String. How the results
#' are printed. Default is `"lavaan.printer"`,
#' and the results will be
#' printed in a format similar to
#' the printout of the output of
#' the `summary`-method of a
#' 'lavaan'-class object.
#' If set to `"table"`, the results are
#' printed in a table
#' format similar to that of
#' [lavaan::parameterEstimates()]
#' with `output` set to `"data.frame"`.
#'
#' @param standardized_only Logical.
#' If `TRUE`, the default, only the
#' results for the standardized solution
#' will be printed. If `FALSE`,
#' then
#' the standardized solution is printed
#' alongside the unstandardized solution,
#' as in the printout of the output
#' of [summary()] of a 'lavaan'-class
#' object.
#'
#' @param show_Bs.by Logical. If `TRUE`
#' and `output` is `"lavaan.printer"`, then the
#' column `"Bs.by"` is shown,
#' indicating, for each parameter, the
#' variables standardized.
#' This column is not shown if `output`
#' is not `"lavaan.printer"`.
#'
#' @param by_group If `TRUE`, the
#' default, and the model has more than
#' one group, sections will be grouped
#' by groups first, as in the print
#' out of `summary()` in `lavaan`.
#' If `FALSE`, then the sections will
#' be grouped by sections first.
#'
#' @param na_str The string to be used
#' for cells with `NA`. Default is
#' `" "`, a whitespace.
#'
#' @param sig_stars If `TRUE`, the
#' default, symbols such as asterisks
#' (`*`, `**`, `***`) will be used to
#' denote whether a beta-select is
#' significant.
#'
#' @param ci_sig If `TRUE`, the default,
#' a beta-select will be denoted as
#' significant or not significant based
#' on its confidence interval.
#'
#' @seealso [lav_betaselect()]. This
#' function is adapted from
#' [semhelpinghands::print.std_solution_boot()].
#'
#' @examples
#' library(lavaan)
#' # Need to mean-center iv and mod
#' data_test_medmod$iv <- data_test_medmod$iv - mean(data_test_medmod$iv)
#' data_test_medmod$mod <- data_test_medmod$mod - mean(data_test_medmod$mod)
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
#' fit_beta
#' print(fit_beta)
#' print(fit_beta, show_Bs.by = TRUE)
#' print(fit_beta, output = "table")
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
                                 output = c("lavaan.printer", "table"),
                                 standardized_only = TRUE,
                                 show_Bs.by = FALSE,
                                 by_group = TRUE,
                                 na_str = " ",
                                 sig_stars = TRUE,
                                 ci_sig = TRUE) {
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

    # List variables standardized
    tmp <- stats::na.omit(x$std.p.by)
    tmp <- unique(unlist(strsplit(tmp, ",", fixed = TRUE)))
    std.p.by.vars <- tmp

    # Add columns required for printing in lavaan-style
    ptable <- attr(x, "partable")
    est0 <- attr(x, "est")
    est1 <- x
    est1$id <- seq_len(nrow(est1))
    class(est1) <- class(x)[-match("lav_betaselect", class(x))]
    i0 <- colnames(ptable) %in% c("est", "se",
                                  "user", "free",
                                  "ustart", "plabel",
                                  "start",
                                  "id")
    est1 <- merge(est1, ptable[, !i0])
    est1 <- est1[order(est1$id), ]
    rownames(est1) <- seq_len(nrow(est1))
    est1$id <- NULL

    # Move std.p.by to the right
    i <- match("std.p.by", colnames(est1))
    est1 <- cbind(est1[, -i], est1[, i, drop = FALSE])

    # Add "stars"?
    if (all(is.na(est1$std.p.pvalue)) || is.null(est1$std.p.pvalue)) {
        sig_stars <- FALSE
      }
    if (sig_stars && ("std.p.pvalue" %in% colnames(est1))) {
        est1 <- add_sig(est1, sig = "std.p.sig")
        sig_legend <- attr(est1, "sig_legend")
      } else {
        sig_legend <- NULL
      }
    # Check CI sig?
    if (ci_sig && ("std.p.ci.lower" %in% colnames(est1))) {
        est1 <- add_ci_sig(est1,
                           ci_lo = "std.p.ci.lower",
                           ci_up = "std.p.ci.upper",
                           sig = "std.p.ci.sig")
      }

    # Convert to a lavaan.parameterEstimates object
    class(est1) <- class(est0)
    pe_attrib <- attr(x, "pe_attrib")
    tmp <- !(names(pe_attrib) %in% names(attributes(est1)))
    attributes(est1) <- c(attributes(est1),
                          pe_attrib[tmp])
    if (!inherits(est1, "lavaan.parameterEstimates")) {
        class(est1) <- c("lavaan.parameterEstimates", class(est1))
      }

    # Add header sections
    has_std_se <- "std.p.se" %in% colnames(est1)
    has_std_pvalue <- "std.p.pvalue" %in% colnames(est1)
    std_se <- if (has_std_se) {
                  attr(x, "std_se")
                } else {
                  "nil"
                }
    std_r <- attr(x, "R")
    prods <- attr(x, "prods")
    cat_vars <- attr(x, "categorical")
    has_std_ci <- "std.p.ci.lower" %in% colnames(x)
    has_std_p <- length(std.p.by.vars) > 0
    if (has_std_ci) {
        level <- attr(x, "level")
        level_str <- paste0(formatC(level * 100,
                                    digits = 1,
                                    format = "f"), "%")
      } else {
        level <- NULL
        level_str <- character(0)
      }

    hdr_select <- function(x) {
        force(std_se)
        force(has_std_ci)
        force(level_str)
        out1 <- data.frame(Field = "Standard Error:",
                           Value = switch(std_se,
                                          delta = "Delta method",
                                          bootstrap = "Nonparametric bootstrap",
                                          "Nil"))
        if ("bootstrap" %in% std_se) {
            out2 <- data.frame(Field = "Bootstrap samples:",
                               Value = as.character(std_r))
            out1 <- rbind(out1, out2)
          }
        if (has_std_ci) {
            out2 <- data.frame(Field = c("Confidence Interval:",
                                         "Level of Confidence:"),
                               Value = c(switch(std_se,
                                              delta = "Delta method",
                                              bootstrap = "Percentile",
                                              "Nil"),
                                         level_str))
            out1 <- rbind(out1, out2)
          }
        colnames(out1) <- NULL
        attr(out1, "section_title") <- "Selected Standardization:"
        # attr(out1, "print_args") <- list(right = TRUE)
        out1
      }

    ftr_select <- function(x) {
        force(has_std_se)
        force(has_std_p)
        force(show_Bs.by)
        force(std.p.by.vars)
        force(prods)
        force(cat_vars)
        force(standardized_only)
        out0 <- character(0)
        if (has_std_p) {
            tmp <- sort(std.p.by.vars)
            std.p.by.vars.str <- paste0("  ",
                                        paste0(tmp, collapse = ", "))
            tmp <- paste("- Variable(s) standardized",
                          std.p.by.vars.str,
                          sep = ":")
            out0 <- c(out0,
                      tmp)
          } else {
            out0 <- c(out0,
                      "- No variable standardized.")
          }
        if (sig_stars && has_std_pvalue) {
            sig_legend2 <- strsplit(sig_legend, "\t")[[1]][1]
            tmp <- paste("- Sig codes:",
                         sig_legend2)
            out0 <- c(out0,
                      tmp)
          }
        if (has_std_se) {
            tmp <- paste("- Standard errors, p-values, and confidence ",
                         "intervals are not computed for betas-select ",
                         "which are fixed in the standardized solution.")
            out0 <- c(out0,
                      tmp)
          }
        if (has_std_pvalue && (std_se == "bootstrap")) {
            if (std_r < 100) {
                tmp <- paste("- Asymmetric bootstrap p-value not computed ",
                             "when the number of bootstrap samples is less ",
                             "than 100.")
                out0 <- c(out0,
                          tmp)
              } else {
                tmp <- paste("- P-values for betas-select are ",
                             "asymmetric bootstrap p-value computed ",
                             "by the method of Asparouhov and Muth\u00e9n (2021).")
                out0 <- c(out0,
                          tmp)
              }
          }
        if (!standardized_only && has_std_p) {
            tmp <- paste("- Betas-select are shown in column 'BSelect'.")
            out0 <- c(out0,
                      tmp)
          }
        if (!standardized_only && has_std_p) {
            tmp <- paste("- Column(s) prefixed by 'BS.*'",
                         "are for betas-select.")
            out0 <- c(out0,
                      tmp)
          }
        if (!standardized_only && has_std_p) {
            tmp <- paste("- Call 'print()' and set ",
                         "'standardized_only' to 'TRUE' to ",
                         "print only betas-select.")
            out0 <- c(out0,
                      tmp)
          }
        if (standardized_only && has_std_p) {
            tmp <- paste("- Call 'print()' and set ",
                         "'standardized_only' to 'FALSE' to ",
                         "print both original estimates and betas-select.")
            out0 <- c(out0,
                      tmp)
          }
        if (show_Bs.by) {
            tmp <- paste("- The column 'Selected' lists variable(s)",
                         "standardized when computing the",
                         "standardized coefficient of a parameter.",
                         "('NA' for user-defined parameters because",
                         "they are computed from other standardized",
                         "parameters.)")
            out0 <- c(out0,
                      tmp)
          }
        if (length(prods) > 0) {
            tmp <- paste0("- Product terms (",
                          paste0(names(prods), collapse = ", "),
                          ") have variables standardized before ",
                          "computing them. ",
                          "The product term(s) is/are ",
                          "not standardized.")
            out0 <- c(out0,
                      tmp)
          }
        if (length(cat_vars)) {
            tmp <- paste0("- Dummy variables (",
                          paste0(cat_vars, collapse = ", "),
                          ") which are predictors are not standardized.")
            out0 <- c(out0,
                      tmp)
          }
        if ("delta" %in% std_se) {
            tmp <- paste0("- Delta method standard errors, ",
                          "p-values, and confidence intervals are ",
                          "usually not recommended for parameters ",
                          "which have variable(s) standardized unless ",
                          "the sample size is large enough.")
            out0 <- c(out0,
                      tmp)
          }
        attr(out0, "section_title") <- "Footnote:"
        attr(out0, "print_fun") <- "cat"
        attr(out0, "strwrap_args") <- list(exdent = 2)
        out0
      }

    if (!show_Bs.by) {
        est1$std.p.by <- NULL
      }

    if (!standardized_only) {
        # Print both original and standardized estimates
        out <- lavaan.printer::parameterEstimates_table_list(est1,
                  rename_cols = c("std.p" = "BSelect",
                                  "std.p.se" = "BS.SE",
                                  "std.p.z" = "BS.Z",
                                  "std.p.pvalue" = "BS.p",
                                  "std.p.sig" = "BS.Sig",
                                  "std.p.ci.lower" = "BS.CI.Lo",
                                  "std.p.ci.upper" = "BS.CI.Hi",
                                  "std.p.ci.sig" = "BS.CI.Sig",
                                  "std.p.by" = "Selected"),
                  header_funs = list(hdr_select),
                  footer_funs = list(ftr_select))
        lavaan.printer::print_parameterEstimates_table_list(out,
                                                            nd = nd,
                                                            by_group = by_group,
                                                            na_str = na_str)
        return(invisible(x))
      } else {
        # Print only standardized estimates
        out <- lavaan.printer::parameterEstimates_table_list(est1,
                  drop_cols = c("ci.lower",
                                "ci.upper",
                                "est",
                                "se",
                                "z",
                                "pvalue",
                                "std.all",
                                "std.lv",
                                "std.ov"),
                  rename_cols = c("std.p" = "BetaSelect",
                                  "std.p.se" = "SE",
                                  "std.p.z" = "Z",
                                  "std.p.pvalue" = "p-value",
                                  "std.p.sig" = "Sig",
                                  "std.p.ci.lower" = "CI.Lo",
                                  "std.p.ci.upper" = "CI.Hi",
                                  "std.p.ci.sig" = "CI.Sig",
                                  "std.p.by" = "Selected"),
                  header_funs = list(hdr_select),
                  footer_funs = list(ftr_select))
        lavaan.printer::print_parameterEstimates_table_list(out,
                                                            nd = nd,
                                                            by_group = by_group,
                                                            na_str = na_str)
        return(invisible(x))
      }
  }

#' @noRd

add_lavaan_pe_attrib <- function(object) {
    pe_attrib <- attr(object, "pe_attrib")
    pe_attrib$names <- NULL
    pe_attrib$row.names <- NULL
    pe_attrib$class <- NULL
    # Do not overwrite existing attributes
    for (x in names(pe_attrib)) {
        if (!(x %in% names(attr(object, x)))) {
            attr(object, x) <- pe_attrib[[x]]
          }
      }
    object
  }

#' @noRd

add_sig <- function(object,
                    pvalue = "std.p.pvalue",
                    sig = "Sig") {
    tmp <- as.numeric(object[, pvalue, drop = TRUE])
    if (!is.null(tmp)) {
        tmp2 <- stats::symnum(tmp,
                              cutpoints = c(0,
                                            .001,
                                            .01,
                                            .05,
                                            .1,
                                            1),
                              symbols = c("***",
                                          "**",
                                          "*",
                                          ".",
                                          " "),
                              na = "--")
        tmp2_legend <- attr(tmp2, "legend")
        i <- match(pvalue, colnames(object))
        out0 <- list(object[, 1:i],
                     tmp2,
                     object[, seq(i + 1, ncol(object))])
        names(out0) <- c("", sig, "")
        out <- do.call(data.frame, out0)
       attr(out, "sig_legend") <- tmp2_legend
      } else {
        out
      }
    out
  }


#' @noRd

add_ci_sig <- function(object,
                       ci_lo = "std.p.ci.lower",
                       ci_up = "std.p.ci.upper",
                       sig = "CI.Sig") {
    cilo <- object[, ci_lo, drop = TRUE]
    ciup <- object[, ci_up, drop = TRUE]
    if (!is.null(cilo) && !is.null(ciup)) {
        ci_sig <- (cilo > 0) | (ciup < 0)
        ci_sig <- ifelse(ci_sig,
                         "Sig.",
                         "n.s.")
        ci_sig[(ciup - cilo) < sqrt(.Machine$double.eps)] <- ""
        ci_sig[is.na(ciup) | is.na(cilo)] <- ""
        i <- match(ci_up, colnames(object))
        out0 <- list(object[, 1:i],
                     ci_sig,
                     object[, seq(i + 1, ncol(object))])
        names(out0) <- c("", sig, "")
        out <- do.call(data.frame, out0)
      } else {
        out
      }
    out
  }
