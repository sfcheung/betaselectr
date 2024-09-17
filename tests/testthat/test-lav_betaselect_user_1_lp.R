skip_on_cran()
# Long test

# testthat::test_file("./tests/testthat/test_std_selected_lavaan_user_1.R")

library(testthat)
library(lavaan)
# library(manymome)

dat <- HolzingerSwineford1939

mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + d5*x5 + d6*x6
f3 =~ x7 + d8*x8 + d9*x9
f2 ~ a*f1
f3 ~ b*f2
ab := a*b
d1 := d5 - d9
d2 := d8 - d6
dd := d1 * d2
"

fit <- cfa(mod,
           dat)

est <- parameterEstimates(fit,
                          standardized = TRUE,
                          ci = FALSE)
std <- standardizedSolution(fit)
std_nox <- standardizedSolution(fit, type = "std.nox")
std_lv <- standardizedSolution(fit, type = "std.lv")

test_that("User parameters", {
  library(lavaan.printer)
  system.time(out <- lav_betaselect(fit,
                                    standardized = TRUE,
                                    std_se = "delta",
                                    ci = TRUE,
                                    progress = FALSE))
  out_names <- colnames(out)
  i <- out_names[grepl("std.p", out_names, fixed = TRUE)]
  i <- c("lhs", "op", "rhs", i[-match("std.p.by", i)], "std.p.by")
  i <- c(i, setdiff(out_names, i))
  out1 <- out[, i]
  tmp <- attributes(out)
  tmp$names <- attr(out1, "names")
  tmp$row.names <- attr(out1, "row.names")
  mostattributes(out1) <- tmp
  attributes(out1)

  add_sig <- function(object,
                      pvalue = "std.p.pvalue",
                      sig = "Sig") {
      tmp <- object[, pvalue, drop = TRUE]
      if (!is.null(tmp)) {
          tmp[is.na(tmp)] <- 1
          tmp2 <- ifelse(tmp < .001, "***", "")
          i <- match(pvalue, colnames(object))
          tmp3a <- list(object[, 1:i],
                        tmp2,
                        object[, seq(i + 1, ncol(object))])
          names(tmp3a) <- c("", sig, "")
          out <- do.call(data.frame, tmp3a)
        }
      out
    }

  out2 <- parameterEstimates_table_list(out1,
                                        drop_cols = c("ci.lower",
                                                      "ci.upper",
                                                      "est",
                                                      "se",
                                                      "z",
                                                      "pvalue",
                                                      "std.all"),
                                        rename_cols = c("std.p" = "BetaSelect",
                                                        "std.p.se" = "SE",
                                                        "std.p.z" = "Z",
                                                        "std.p.pvalue" = "p-value",
                                                        "std.p.ci.lower" = "CI.Lo",
                                                        "std.p.ci.upper" = "CI.Hi",
                                                        "std.p.by" = "Selected"),
                                        est_funs = list(add_sig))
  print_parameterEstimates_table_list(out2)

  # expect_output(print(out, output = "text"),
  #               "Estimates")
  # expect_output(print(out, output = "text", standardized_only = TRUE),
  #               "Estimates")
})
