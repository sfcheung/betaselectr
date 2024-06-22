#' @noRd

lm_betaselect_check_fit <- function(object) {
    if (!inherits(object, "lm")) {
        stop("'object' is not a lmclass object.")
      }
    return(TRUE)
  }

#' @noRd

fix_to_standardize_lm <- function(object,
                                  to_standardize = ".all.",
                                  not_to_standardize = NULL,
                                  skip_categorical_x = TRUE,
                                  prods = NULL) {
    if (!identical(to_standardize, ".all.") && !is.null(not_to_standardize)) {
        stop("Do not specify both to_standardize and not_to_standardize.")
      }
    # if (is.null(prods)) {
    #     prods <- find_all_products(object)
    #   }

    # Get the data frame

    dat <- stats::model.frame(object)
    k <- ncol(dat)

    all_names <- colnames(dat)

    cat_vars <- find_categorical_lm(object)

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
    # if (length(prods) > 0) {
    #     to_standardize <- setdiff(to_standardize, names(prods))
    #   }
    to_standardize
  }


#' @noRd

fix_to_standardize_lm_data <- function(object,
                                       input_data,
                                       to_standardize = ".all.",
                                       not_to_standardize = NULL,
                                       skip_categorical_x = TRUE,
                                       prods = NULL) {

    if (!identical(to_standardize, ".all.") && !is.null(not_to_standardize)) {
        stop("Do not specify both to_standardize and not_to_standardize.")
      }
    # if (is.null(prods)) {
    #     prods <- find_all_products(object)
    #   }

    # Get the data frame

    k <- ncol(input_data)

    all_names <- colnames(input_data)

    cat_vars <- find_categorical_lm(object)

    cat_vars2 <- sapply(seq_len(k),
                       function(xx) {
                           if (is.numeric(input_data[, xx])) {
                               return(NA)
                             } else {
                               return(colnames(input_data)[xx])
                             }
                         })
    cat_vars2 <- cat_vars2[!is.na(cat_vars2)]
    cat_vars <- unique(union(cat_vars, cat_vars2))

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
    # if (length(prods) > 0) {
    #     to_standardize <- setdiff(to_standardize, names(prods))
    #   }
    to_standardize
  }

#' @noRd

find_categorical_lm <- function(object) {
    object_terms <- stats::terms(object)
    tmp <- attr(object_terms, "dataClasses")
    tmp <- tmp[tmp != "numeric"]
    names(tmp)
  }
