#' @noRd
recenter_cond <- function(std_out_i,
                          prods) {
  out <- list(m_i = NULL,
              b_i = NULL)
  lhs <- std_out_i$lhs
  rhs <- std_out_i$rhs
  op <- std_out_i$op
  if (op != "~") return(out)
  all_y <- sapply(prods,
                  function(x) x$y)
  all_x <- sapply(prods,
                  function(x) x$x)
  all_w <- sapply(prods,
                  function(x) x$w)
  if (isFALSE(lhs %in% all_y)) return(out)
  if (rhs %in% all_x) {
    prod_i <- prods[[match(rhs, all_x)]]
    m_i <- prod_i$w
    b_i <- prod_i$b
  } else if (rhs %in% all_w) {
    prod_i <- prods[[match(rhs, all_w)]]
    m_i <- prod_i$x
    b_i <- prod_i$b
  } else {
    return(out)
  }
  out$m_i <- m_i
  out$b_i <- b_i
  out
}

#' @noRd

def_std <- function(std,
                    ptable,
                    def.function) {
     p_free <- which(ptable$free > 0)
     i_free <- order(ptable$free[p_free])
     std_free <- std[p_free, "std.p"][i_free]
     out <- def.function(.x. = std_free)
     out
  }

#' @noRd

lav_betaselect_check_fit <- function(object) {
    if (!inherits(object, "lavaan")) {
        stop("'object' is not a lavaan-class object.")
      }
    if (lavaan::lavInspect(object, what = "nlevels") > 1) {
        stop("Multilevel SEM models are not supported.")
      }
    opt <- lavaan::lavInspect(object, what = "options")
    if (isTRUE(opt$conditional.x)) {
        stop("Does not support models with conditional.x = TRUE.")
      }
    tmp <- tryCatch(lavaan::lavInspect(object, what = "data"),
                    error = function(e) e)
    if (inherits(tmp, "error")) {
        stop("The model needs to be fitted to raw data.")
      }
    return(TRUE)
  }

#' @noRd

fix_to_standardize <- function(object,
                               to_standardize = ".all.",
                               not_to_standardize = NULL,
                               skip_categorical_x = TRUE,
                               prods = NULL) {
    if (!identical(to_standardize, ".all.") && !is.null(not_to_standardize)) {
        stop("Do not specify both to_standardize and not_to_standardize.")
      }
    if (is.null(prods)) {
        prods <- find_all_products(object)
      }
    all_names <- unique(union(lavaan::lavNames(object, "ov"),
                              lavaan::lavNames(object, "lv")))
    cat_vars <- find_categorical(object)
    # Exclude ov.ord from cat_vars
    cat_vars <- setdiff(cat_vars, lavaan::lavNames(object, "ov.ord"))
    if (is.null(not_to_standardize)) {
        if (identical(to_standardize, ".all.")) {
            to_standardize <- all_names
          } else {
            to_standardize <- intersect(all_names, to_standardize)
            if (length(to_standardize) == 0) {
                stop("All variables in to_standardize not in the model.")
              }
          }
      } else {
        to_standardize <- setdiff(all_names, not_to_standardize)
      }
    if ((length(cat_vars) > 0) && skip_categorical_x) {
        to_standardize <- setdiff(to_standardize, cat_vars)
      }
    if (length(prods) > 0) {
        to_standardize <- setdiff(to_standardize, names(prods))
      }
    to_standardize
  }

#' @noRd

boot_out_to_boot_est_map <- function(object,
                                     boot_out) {
    ptable <- lavaan::parameterTable(object)
    ptable_free <- ptable[ptable$free > 0, ]
    ptable_free <- ptable_free[order(ptable_free$free), ]
    boot_out_est <- boot_out[[1]]$est
    if (is.null(boot_out_est$group)) {
        boot_out_est$group <- 1
      }
    boot_out_est$b_id <- seq_len(nrow(boot_out_est))
    out <- merge(boot_out_est[, c("lhs", "op", "rhs", "group", "b_id")],
                 ptable_free[, c("lhs", "op", "rhs", "group", "free")])
    out <- out[order(out$free), ]
    out
  }

boot_out_to_boot_est_i <- function(x,
                                   b_map) {
    as.vector(x$est[b_map, "est"])
  }


#' @noRd

std_boot_user <- function(std,
                          ptable,
                          i,
                          def.function,
                          boot_est) {
    boot_est_split <- asplit(boot_est, MARGIN = 1)
    std_split <- lapply(boot_est_split, function(x) {
        std[i, "std.p"] <- x
        std[, "std.p", drop = FALSE]
      })
    out <- lapply(std_split,
                  def_std,
                  ptable = ptable,
                  def.function = def.function)
    est_std_boot_user <- do.call(rbind, out)
    est_std_boot_user
  }


#' @noRd

std_boot <- function(object,
                     std_fct,
                     std_fct_v,
                     boot_out,
                     bootstrap,
                     parallel,
                     ncpus,
                     cl,
                     iseed,
                     progress = FALSE) {
    vector_form <- is.function(std_fct_v)
    if (!is.null(boot_out)) {
        if (!inherits(boot_out, "boot_out")) {
            stop("boot_out is not an output of manymome::do_boot().")
          }
        boot_map <- boot_out_to_boot_est_map(object = object,
                                             boot_out = boot_out)
        i_ok <- sapply(boot_out, function(x) x$ok)
        boot_out <- boot_out[i_ok]
        boot_est <- sapply(boot_out,
                           FUN = boot_out_to_boot_est_i,
                           b_map = boot_map$b_id)
        boot_est <- t(boot_est)
      } else {
        boot_est <- tryCatch(lavaan::lavInspect(object,
                                                what = "boot"),
                            error = function(e) e)
        if (inherits(boot_est, "error")) {
            # stop("Bootstrap SEs/CIs requested but bootstrap not used when fitting the model.")
            boot_est <- lavaan::bootstrapLavaan(object,
                                                R = bootstrap,
                                                parallel = parallel,
                                                ncpus = ncpus,
                                                cl = cl,
                                                iseed = iseed)
          }
        boot_est_err <- attr(boot_est, "error.idx")
        if (length(boot_est_err) > 0) {
            boot_est <- boot_est[-boot_est_err, ]
          }
      }
    if (progress) {
        cat("\nCompute bootstrapping standardized solution:\n")
        if (vector_form) {
            est_std_boot <- pbapply::pbsapply(asplit(boot_est, 1),
                              std_fct_v,
                              simplify = TRUE)
          } else {
            est_std_boot <- pbapply::pbsapply(asplit(boot_est, 1),
                              function(yy) {
                                  sapply(std_fct, function(xx) xx(yy))
                                },
                              simplify = TRUE)
          }
      } else {
        if (vector_form) {
            est_std_boot <- sapply(asplit(boot_est, 1),
                              std_fct_v,
                              simplify = TRUE)
          } else {
            est_std_boot <- sapply(asplit(boot_est, 1),
                              function(yy) {
                                  sapply(std_fct, function(xx) xx(yy))
                                },
                              simplify = TRUE)
          }
      }
    est_std_boot <- t(est_std_boot)
    est_std_boot
  }

#' @noRd

std_se_boot_all <- function(boot_est) {
    out <- apply(boot_est,
                 MARGIN = 2,
                 FUN = stats::sd)
    out
  }

#' @noRd

std_ci_boot_all <- function(x_est,
                            x_est_boot,
                            level = .95) {
    if (is.null(dim(x_est_boot))) {
        x_est_boot <- matrix(x_est_boot, ncol = 1)
      }
    boot0 <- list(t0 = x_est,
                  t = x_est_boot,
                  R = nrow(x_est_boot))
    p <- ncol(x_est_boot)
    boot_ci_out0 <- lapply(seq_len(p),
        function(xx) {
            tmp <- range(x_est_boot[, xx])
            if (isTRUE(all.equal(tmp[1], tmp[2]))) {
                return(c(x_est[xx], x_est[xx]))
              }
            boot::boot.ci(boot0,
                          type = "perc",
                          index = xx,
                          conf = level)$percent[4:5]
          })
    out <- do.call(rbind, boot_ci_out0)
    colnames(out) <- c("ci.lower", "ci.upper")
    out
  }

#' @noRd

std_pvalue_boot_all <- function(x_est_boot,
                                h0 = 0,
                                min_size = 100,
                                warn = FALSE) {
    if (is.null(dim(x_est_boot))) {
        x_est_boot <- matrix(x_est_boot, ncol = 1)
      }
    p <- ncol(x_est_boot)
    out <- sapply(asplit(x_est_boot, MARGIN = 2),
                  std_pvalue_boot_i,
                  h0 = h0,
                  min_size = min_size,
                  warn = warn)
    out
  }

#' @noRd

std_pvalue_boot_i <- function(x,
                              h0 = 0,
                              min_size = 100,
                              warn = FALSE) {
    # Adapted from manymome:::est2p()
    # Based on the method in
    # https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    if (isTRUE(all.equal(min(x), max(x)))) return(NA)
    if (length(x) < min_size) {
        if (warn) {
          warning(paste("Bootstrap p-value not computed. Less than ",
                        min_size,
                        "bootstrap estimates."))
        }
        return(NA)
      }
    b <- length(x)
    m0 <- sum((x < h0))
    out <- 2 * min(m0 / b, 1 - m0 / b)
    out
  }

#' @noRd

std_se_delta_user <- function(std,
                              ptable,
                              i,
                              def.function,
                              std_fct,
                              std_fct_v,
                              fit_vcov,
                              std_vcov,
                              method = "numDeriv",
                              progress = FALSE) {
    vector_form <- is.function(std_fct_v)

    p_free <- which(ptable$free > 0)
    i_free <- order(ptable$free[p_free])
    i_std_free <- which(ptable[i, "free"] > 0)

    # Compute VCOV of standardized solution
    if (vector_form) {
        # std_vcov already available
      } else {
        std_fct_all <- function(x) {
            suppressWarnings(sapply(std_fct, function(xx) xx(x)))
          }
        a <- lavaan::lav_func_jacobian_complex(func = std_fct_all,
                                            x = ptable[p_free, "est"])
        std_vcov <- a %*% tcrossprod(fit_vcov, a)
      }

    std_free <- std[i, "std.p"][i_std_free]
    std_vcov_free <- std_vcov[i_std_free, i_std_free]
    tmp <- def.function(.x. = std_free)
    p_def <- length(tmp)
    def_names <- names(tmp)
    # This works but is inefficient
    def_split <- lapply(seq_len(p_def),
        function(xx) {
          force(xx)
          out <- function(x) {
                     def.function(.x. = x)[xx]
                   }
          out
        }
      )
    if (progress) {
        cat("\nCompute delta method standard errors for user-parameters:\n")
        out <- pbapply::pbsapply(def_split,
                                 FUN = std_se_delta,
                                 fit_est = std_free,
                                 fit_vcov = std_vcov_free,
                                 method = method)
      } else {
        out <- sapply(def_split,
                      FUN = std_se_delta,
                      fit_est = std_free,
                      fit_vcov = std_vcov_free,
                      method = method)
      }
    names(out) <- def_names
    out
  }

#' @noRd

std_se_delta_all <- function(std_fct,
                             std_fct_v,
                             fit_est,
                             fit_vcov,
                             method = "numDeriv",
                             progress = FALSE) {
    vector_form <- is.function(std_fct_v)
    if (vector_form) {
        a <- lavaan::lav_func_jacobian_complex(func = std_fct_v,
                                                  x = fit_est)
        out0 <- a %*% tcrossprod(fit_vcov, a)
        out <- sqrt(diag(out0))
        attr(out, "std_vcov") <- out0
      } else {
        if (progress) {
            cat("\nCompute delta method standard errors:\n")
            out <- pbapply::pbsapply(std_fct,
                                    FUN = std_se_delta,
                                    fit_est = fit_est,
                                    fit_vcov = fit_vcov,
                                    method = method)
          } else {
            out <- sapply(std_fct,
                          FUN = std_se_delta,
                          fit_est = fit_est,
                          fit_vcov = fit_vcov,
                          method = method)
          }
      }
    out
  }

#' @noRd

std_se_delta <- function(std_fct,
                         fit_est,
                         fit_vcov,
                         method = c("numDeriv", "lavaan")) {
    method <- match.arg(method)
    std_a <- switch(method,
      numDeriv = numDeriv::grad(std_fct,
                                x = fit_est),
      lavaan = suppressWarnings(lavaan::lav_func_gradient_complex(std_fct,
                                  x = fit_est)))
    out <- sqrt(colSums(std_a * (fit_vcov %*% std_a)))
    out
  }

#' @noRd

std_ci_delta_all <- function(x_est,
                             x_se,
                             level = .95) {
    pcrit <- abs(stats::qnorm((1 - level) / 2))
    cilo <- x_est - pcrit * x_se
    cihi <- x_est + pcrit * x_se
    out <- cbind(ci.lower = cilo,
                 ci.upper = cihi)
    out
  }

#' @noRd

std_pvalue_delta_all <- function(est_std_z) {
    out <- stats::pnorm(abs(est_std_z), lower.tail = FALSE) * 2
    out
  }

#' @noRd

pt_id_2_est_id <- function(est_table,
                           ptable) {
    if (is.null(est_table$group)) {
        est_table$group <- 1
        est_table[est_table$op %in% c(":=", "=="), "group"] <- 0
      }
    est_table$est_id <- seq_len(nrow(est_table))
    ptable$pt_id <- seq_len(nrow(ptable))
    out <- merge(est_table[, c("lhs", "op", "rhs", "group", "est_id")],
                 ptable[, c("lhs", "op", "rhs", "group", "pt_id")],
                 sort = FALSE)
    out <- out[order(out$est_id), ]
    out
  }

#' @noRd

std_rows <- function(object,
                     std_intercept = FALSE) {
  ptable <- lavaan::parameterTable(object)
  std <- lavaan::standardizedSolution(object,
                                      se = TRUE,
                                      zstat = TRUE,
                                      pvalue = TRUE,
                                      ci = FALSE,
                                      partable = ptable)
  i <- !is.na(std$z)
  if (std_intercept) {
    i <- i & (std$op != "~1")
  }
  out <- which(i)
  out
}

#' @noRd

gen_std <- function(object,
                    i = NULL,
                    to_standardize = ".all.",
                    prods = NULL,
                    internal_only = FALSE,
                    std_intercept = FALSE) {
    if (is.null(i)) {
        i <- std_rows(object,
                      std_intercept = std_intercept)
      }
    if (is.null(prods)) {
        prods <- find_all_products(object)
      }
    out <- lapply(i,
                  gen_std_i,
                  fit = object,
                  to_standardize = to_standardize,
                  prods = prods,
                  internal_only = internal_only)
    out
  }


#' @noRd
# Adapted from semhelpinghands::standardizedSolution_boot_ci()

gen_std_vector <- function(fit,
                           i_vector,
                           std_fct_vector) {
    pt <- lavaan::parameterTable(fit)
    p_free <- pt$free > 0

    slot_opt <- fit@Options
    slot_pat <- fit@ParTable
    slot_mod <- fit@Model
    slot_smp <- fit@SampleStats
    slot_dat <- fit@Data

    slot_opt1 <- slot_opt
    slot_opt1$do.fit <- FALSE
    slot_opt1$se <- "none"
    slot_opt1$test <- "none"
    slot_opt1$baseline <- FALSE
    slot_opt1$h1 <- FALSE

    out <- function(par) {
        if (missing(par)) {
            par <- get(".x.", parent.frame())
          }
        # Just in case ...
        force(p_free)
        force(slot_opt)
        force(slot_pat)
        force(slot_mod)
        force(slot_smp)
        force(slot_dat)
        force(slot_opt1)
        force(i_vector)
        force(std_fct_vector)
        slot_mod1 <- lavaan::lav_model_set_parameters(slot_mod,
                                                      par)
        slot_pat1 <- slot_pat
        slot_pat1$est[p_free] <- par
        slot_pat1$start[p_free] <- par
        tmp <- (slot_pat1$free == 0) & (slot_pat1$op != ":=") &
               (slot_pat1$start != slot_pat1$est)
        tmp <- which(tmp)
        slot_pat1$start[tmp] <- slot_pat1$est[tmp]
        slot_opt1$start <- par
        fit_new <- lavaan::lavaan(slotOptions = slot_opt1,
                                  slotParTable = slot_pat1,
                                  slotModel = slot_mod1,
                                  slotSampleStats = slot_smp,
                                  slotData = slot_dat)
        fit_cov_all <- lavaan::lavInspect(fit_new,
                                          what = "cov.all",
                                          drop.list.single.group = FALSE)
        fit_sd_all <- lapply(fit_cov_all, function(x) sqrt(diag(x)))
        i_group_vector <- pt[i_vector, "group"]
        fit_sd_all_list <- lapply(i_group_vector,
                                    function(x) {
                                        fit_sd_all[[x]]
                                      })
        if (slot_opt1$meanstructure) {
          fit_m_all_lv <- lavaan::lavInspect(
                          fit_new,
                          what = "mean.lv",
                          drop.list.single.group = FALSE)
          fit_m_all_ov <- lavaan::lavInspect(
                          fit_new,
                          what = "mean.ov",
                          drop.list.single.group = FALSE)
          fit_m_all <- mapply(function(x, y) {c(x, y)},
                              x = fit_m_all_lv,
                              y = fit_m_all_ov,
                              SIMPLIFY = FALSE,
                              USE.NAMES = FALSE)
          fit_m_all_list <- lapply(i_group_vector,
                                    function(x) {
                                        fit_m_all[[x]]
                              })
        } else {
          dat0 <- lavaan::lavInspect(
                    fit_new,
                    what = "data",
                    drop.list.single.group = FALSE)
          fit_m_all <- lapply(dat0,
                              function(x) colMeans(x, na.rm = TRUE))
          fit_m_all_list <- lapply(i_group_vector,
                                    function(x) {
                                        fit_m_all[[x]]
                              })
        }
        # fit_sd_all <- fit_sd_all[[i_group]]
        # std_out_i <- lavaan::parameterTable(fit_new)[i, ]
        ptable_new <- lavaan::parameterTable(fit_new)
        std_out_i_list <- lapply(i_vector,
                                   function(x) {
                                        ptable_new[x, ]
                                      })
        out0 <- lapply(seq_along(std_fct_vector), function(x) {
                    std_fct_vector[[x]](std_out_i = std_out_i_list[[x]],
                                        fit_sd_all = fit_sd_all_list[[x]],
                                        fit_m_all = fit_m_all_list[[x]])
                  })
        out1 <- unlist(out0)
        attr(out1, "std_by") <- lapply(out0,
                                       FUN = attr,
                                       which = "std_by",
                                       exact = TRUE)
        attr(out1, "est_vcov") <- fit_cov_all
        out1
      }
    out
  }


#' @noRd
# Adapted from semhelpinghands::standardizedSolution_boot_ci()

gen_std_i <- function(fit,
                      i,
                      to_standardize = ".all.",
                      prods = list(),
                      internal_only = FALSE) {
    pt <- lavaan::parameterTable(fit)
    p_free <- pt$free > 0
    pt_i <- pt[i, ]

    tmp <- to_standardize_for_i(prods = prods,
                                to_standardize = to_standardize,
                                pt_i = pt_i)
    prod_names <- tmp$prod_names
    to_standardize_i <- tmp$to_standardize_i

    slot_opt <- fit@Options
    slot_pat <- fit@ParTable
    slot_mod <- fit@Model
    slot_smp <- fit@SampleStats
    slot_dat <- fit@Data

    slot_opt1 <- slot_opt
    slot_opt1$do.fit <- FALSE
    slot_opt1$se <- "none"
    slot_opt1$test <- "none"
    slot_opt1$baseline <- FALSE
    slot_opt1$h1 <- FALSE

    std_i_internal <- gen_std_i_internal(fit = fit,
                                         i = i,
                                         to_standardize_i = to_standardize_i,
                                         prod_names = prod_names,
                                         prods = prods)

    if (internal_only) {
        return(std_i_internal)
      }

    out <- function(par) {
        if (missing(par)) {
            par <- get(".x.", parent.frame())
          }
        # Just in case ...
        force(p_free)
        force(slot_opt)
        force(slot_pat)
        force(slot_mod)
        force(slot_smp)
        force(slot_dat)
        force(slot_opt1)
        force(std_i_internal)
        slot_mod1 <- lavaan::lav_model_set_parameters(slot_mod,
                                                      par)
        slot_pat1 <- slot_pat
        slot_pat1$est[p_free] <- par
        slot_pat1$start[p_free] <- par
        tmp <- (slot_pat1$free == 0) & (slot_pat1$op != ":=") &
               (slot_pat1$start != slot_pat1$est)
        tmp <- which(tmp)
        slot_pat1$start[tmp] <- slot_pat1$est[tmp]
        slot_opt1$start <- par
        fit_new <- lavaan::lavaan(slotOptions = slot_opt1,
                                  slotParTable = slot_pat1,
                                  slotModel = slot_mod1,
                                  slotSampleStats = slot_smp,
                                  slotData = slot_dat)
        fit_cov_all <- lavaan::lavInspect(fit_new,
                                          what = "cov.all",
                                          drop.list.single.group = FALSE)
        fit_sd_all <- lapply(fit_cov_all, function(x) sqrt(diag(x)))
        i_group <- pt[i, "group"]
        fit_sd_all <- fit_sd_all[[i_group]]
        std_out_i <- lavaan::parameterTable(fit_new)[i, ]
        out0 <- std_i_internal(std_out_i = std_out_i,
                               fit_sd_all = fit_sd_all)
        out0
      }
    out
  }

#' @noRd

gen_std_i_internal <- function(fit,
                               i,
                               to_standardize_i,
                               prod_names,
                               prods) {
    std_out_i <- lavaan::parameterTable(fit)[i, ]
    std_by <- character(0)
    lhs <- std_out_i$lhs
    rhs <- std_out_i$rhs
    op <- std_out_i$op

    n_x_s <- NULL
    n_w_s <- NULL
    d_x_s <- NULL
    d_w_s <- NULL
    n_x_p <- 1
    n_w_p <- 1
    d_x_p <- 1
    d_w_p <- 1
    m_i <- NULL
    b_i <- 0

    if (op == "~") {
        if (lhs %in% prod_names) {
            # x-term?
            if (prods[[lhs]]$x %in% to_standardize_i) {
                d_x_s <- prods[[lhs]]$x
                d_x_p <- -1
              }
            # w-term?
            if (prods[[lhs]]$w %in% to_standardize_i) {
                d_w_s <- prods[[lhs]]$w
                d_w_p <- -1
              }
          } else {
            # Product term not in LHS
            if (lhs %in% to_standardize_i) {
                d_x_s <- lhs
                d_x_p <- -1
              }
          }
        if (rhs %in% prod_names) {
            # x-term?
            if (prods[[rhs]]$x %in% to_standardize_i) {
                n_x_s <- prods[[rhs]]$x
                n_x_p <- 1
              }
            # w-term?
            if (prods[[rhs]]$w %in% to_standardize_i) {
                n_w_s <- prods[[rhs]]$w
                n_w_p <- 1
              }
          } else {
            # Product term not in RHS
            if (rhs %in% to_standardize_i) {
                n_x_s <- rhs
                n_x_p <- 1
              }
            tmp <- recenter_cond(
                      std_out_i = std_out_i,
                      prods = prods)
            m_i <- tmp$m_i
            b_i <- tmp$b_i
          }
        std_by <- c(std_by,
                    n_x_s,
                    n_w_s,
                    d_x_s,
                    d_w_s)
      }

    # Covariance or variance
    if (op == "~~") {
        if (lhs %in% prod_names) {
            # x-term?
            if (prods[[lhs]]$x %in% to_standardize_i) {
                d_x_s <- prods[[lhs]]$x
                d_x_p <- -1
              }
            # w-term?
            if (prods[[lhs]]$w %in% to_standardize_i) {
                d_w_s <- prods[[lhs]]$w
                d_w_p <- -1
              }
          } else {
            # Product term not in LHS
            if (lhs %in% to_standardize_i) {
                d_x_s <- lhs
                d_x_p <- -1
              }
          }
        if (rhs %in% prod_names) {
            # x-term?
            if (prods[[rhs]]$x %in% to_standardize_i) {
                n_x_s <- prods[[rhs]]$x
                n_x_p <- -1
              }
            # w-term?
            if (prods[[rhs]]$w %in% to_standardize_i) {
                n_w_s <- prods[[rhs]]$w
                n_w_p <- -1
              }
          } else {
            # Product term not in RHS
            if (rhs %in% to_standardize_i) {
                n_x_s <- rhs
                n_x_p <- -1
              }
          }
        std_by <- c(std_by,
                    n_x_s,
                    n_w_s,
                    d_x_s,
                    d_w_s)
      }

    # Factor loading
    if (op == "=~") {
        if (lhs %in% prod_names) {
            # x-term?
            if (prods[[lhs]]$x %in% to_standardize_i) {
                d_x_s <- prods[[lhs]]$x
                d_x_p <- 1
              }
            # w-term?
            if (prods[[lhs]]$w %in% to_standardize_i) {
                d_w_s <- prods[[lhs]]$w
                d_w_p <- 1
              }
          } else {
            # Product term not in LHS
            if (lhs %in% to_standardize_i) {
                d_x_s <- lhs
                d_x_p <- 1
              }
          }
        if (rhs %in% prod_names) {
            # x-term?
            if (prods[[rhs]]$x %in% to_standardize_i) {
                n_x_s <- prods[[rhs]]$x
                n_x_p <- -1
              }
            # w-term?
            if (prods[[rhs]]$w %in% to_standardize_i) {
                n_w_s <- prods[[rhs]]$w
                n_w_p <- -1
              }
          } else {
            # Product term not in RHS
            if (rhs %in% to_standardize_i) {
                n_x_s <- rhs
                n_x_p <- -1
              }
          }
        std_by <- c(std_by,
                    n_x_s,
                    n_w_s,
                    d_x_s,
                    d_w_s)
      }

    std_by <- unique(std_by)
    out_fct <- function(std_out_i,
                        fit_sd_all,
                        fit_m_all = NULL) {
        # Just in case ...
        force(n_x_s)
        force(n_w_s)
        force(d_x_s)
        force(d_w_s)
        force(n_x_p)
        force(n_w_p)
        force(d_x_p)
        force(d_w_p)
        force(prods)
        force(m_i)
        force(b_i)
        a <- ifelse(is.null(n_x_s),
                    1,
                    fit_sd_all[n_x_s]^n_x_p) *
             ifelse(is.null(n_w_s),
                    1,
                    fit_sd_all[n_w_s]^n_w_p) *
             ifelse(is.null(d_x_s),
                    1,
                    fit_sd_all[d_x_s]^d_x_p) *
             ifelse(is.null(d_w_s),
                    1,
                    fit_sd_all[d_w_s]^d_w_p)
        # out0 <- std_out_i$est * a
        m_0 <- ifelse(is.null(m_i),
                      0,
                      fit_m_all[m_i] * b_i)
        out0 <- (std_out_i$est + m_0) * a
        attr(out0, "std_by") <- std_by
        out0
      }
    return(out_fct)
  }

#' @noRd

to_standardize_for_i <- function(prods,
                                 to_standardize,
                                 pt_i) {
    if (length(prods) > 0) {
        prod_names <- names(prods)
        tmp1 <- pt_i$lhs
        if (tmp1 %in% prod_names) {
            tmp1 <- c(prods[[tmp1]]$w, prods[[tmp1]]$x)
          }
        tmp2 <- pt_i$rhs
        if (tmp2 %in% prod_names) {
            tmp2 <- c(prods[[tmp2]]$w, prods[[tmp2]]$x)
          }
      } else {
        prod_names <- character(0)
        tmp1 <- pt_i$lhs
        tmp2 <- pt_i$rhs
      }

    to_standardize_i <- unique(union(tmp1, tmp2))
    if (isFALSE(identical(to_standardize, ".all."))) {
        to_standardize_i <- intersect(to_standardize_i, to_standardize)
      }
    list(to_standardize_i = to_standardize_i,
         prod_names = prod_names)
  }

#' @noRd

find_all_products <- function(fit,
                              parallel = TRUE,
                              ncpus = parallel::detectCores(logical = FALSE) - 1,
                              cl = NULL,
                              progress = FALSE) {

    ptable <- lavaan::parameterTable(fit)
    reg_paths <- all_reg_paths(ptable)
    if (length(reg_paths) == 0) return(list())
    if (parallel) {
        if (is.null(cl)) {
            cl <- parallel::makeCluster(min(nrow(reg_paths), ncpus))
            on.exit(parallel::stopCluster(cl), add = TRUE)
          }
        tmp <- split(reg_paths,
                      seq_len(nrow(reg_paths)),
                      drop = FALSE)
        tmpfct <- function(xx) {
            force(fit)
            manymome::get_prod(x = xx[2],
                                y = xx[1],
                                fit = fit,
                                expand = TRUE)
          }
        if (progress) {
            cat("Finding product terms in the model ...\n")
            prods <- pbapply::pblapply(tmp,
                                       FUN = tmpfct,
                                       cl = cl)
            cat("Finished finding product terms.\n")
          } else {
            prods <- parallel::parLapplyLB(cl = cl,
                                           tmp,
                                           fun = tmpfct,
                                           chunk.size = 1)
          }
      } else {
        if (progress) {
            cat("Finding product terms in the model ...\n")
            prods <- pbapply::pbmapply(manymome::get_prod,
                                       x = reg_paths[, "rhs"],
                                       y = reg_paths[, "lhs"],
                                       MoreArgs = list(fit = fit,
                                                       expand = TRUE),
                                       SIMPLIFY = FALSE)
            cat("Finished finding product terms.\n")
          } else {
            prods <- mapply(manymome::get_prod,
                            x = reg_paths[, "rhs"],
                            y = reg_paths[, "lhs"],
                            MoreArgs = list(fit = fit,
                                            expand = TRUE),
                            SIMPLIFY = FALSE)
          }
      }
    prods_ok <- sapply(prods, function(x) is.list(x))
    if (!any(prods_ok)) {
        return(list())
      }
    prods <- prods[prods_ok]
    prods_ok <- sapply(prods, function(x) !is.null(x$prod))
    if (!any(prods_ok)) {
        return(list())
      }
    prods <- prods[prods_ok]
    prods <- keep_prod_1(prods)
    prods_dup <- !duplicated(sapply(prods, function(x) x$prod))
    prods <- prods[prods_dup]
    prods_names <- sapply(prods, function(x) x$prod)
    names(prods) <- prods_names
    prods
  }

#' @noRd

all_reg_paths <- function(ptable) {
    i <- ptable$op %in% c("~")
    if (length(i) == 0) return(list())
    out <- as.matrix(ptable[i, c("lhs", "rhs")])
    out <- unique(out)
    out
  }

#' @noRd

keep_prod_1 <- function(prods) {
    i <- sapply(prods, function(x) length(x$b))
    out <- prods[i == 1]
    out
  }

#' @noRd

find_categorical <- function(fit,
                             k = 2) {
    fit_datas <- lavaan::lavInspect(fit,
                                    what = "data",
                                    drop.list.single.group = FALSE)
    out <- lapply(fit_datas,
                  find_categorical_i,
                  k = 2)
    out1 <- lapply(out,
                   function(x) names(x[which(x)]))
    out2 <- unlist(out1, use.names = FALSE)
    out2 <- unique(out2)
    out2
  }

#' @noRd

find_categorical_i <- function(fit_data,
                               k = 2) {
    out <- apply(fit_data,
                 MARGIN = 2,
                 find_k,
                 k = k)
    out
  }

#' @noRd
# Fast in identifying a column with more than k unique values
# Slow in identifying a column with k or few unique values

find_k <- function(x,
                   k = 2) {
    x <- stats::na.omit(x)
    if (length(x) < 2) return(NA)
    xs <- numeric(k)
    xs[] <- NA
    j <- 0
    for (i in seq_along(x)) {
        if (x[i] %in% xs) {
            next
          } else {
            j <- j + 1
            if (j > k) {
                return(FALSE)
              } else {
                xs[j] <- x[i]
              }
          }
      }
    return(TRUE)
  }

#' @noRd

check_centered <- function(object,
                           prods = list()) {
  if (length(prods) == 0) {
    return(NULL)
  }
  all_x_w <- lapply(prods,
                    function(xx) c(xx$x, xx$w))
  all_x_w <- unique(unlist(all_x_w))
  dat <- lavaan::lavInspect(object,
                            what = "data",
                            drop.list.single.group = FALSE)
  all_means <- lapply(dat,
                      colMeans,
                      na.rm = TRUE)
  all_x_w_zero_mean <- sapply(all_means,
                              function(xx) {
                                all(xx[all_x_w] < sqrt(.Machine$double.eps))
                              })
  return(all_x_w_zero_mean)
}