#' @title Number of non-NAs
#'
#' @description Counts the number of elements that are not \code{NA} in a vector
#' @param vec Vector: a vector-like object
#' @return Integer: number of non-NAs
#' @examples
#' n_nna(c(NA, 9, 10))
#' n_nna(c('b', 'c', NA))
#' @export
n_nna <- function(vec) {
    return(sum(!is.na(vec)))
}


#' @title Extract Unique Non-NA Elements
#'
#' @param obj Object: a vector or a data frame or an array or NULL
#' @param ... Ellipsis: arguments passed to \code{\link[base]{unique}}
#' @return See return of \code{\link[base]{unique}}
#' @examples
#' unique_na(c(NA, 9, 10))
#' unique_na(c('b', 'c', NA))
#' @export
#' @seealso \code{\link[base]{unique}}
unique_na <- function(obj, ...) {
    if(length(na.omit(obj)) == 0) return(NA)
    obj_nna <- obj[!is.na(obj)]
    return(unique(obj_nna, ...))
}


#' @title Arithmetic Mean with NAs Removed
#'
#' @description \code{\link[base]{mean}} function with \code{na.rm = TRUE}
#' @param x See \code{x} in \code{\link[base]{mean}}
#' @param ... Ellipsis: arguments passed to \code{\link[base]{mean}}
#' @return See return of \code{\link[base]{mean}}
#' @examples
#' mean_na(c(NA, 9, 10))
#' @export
#' @seealso \code{\link[base]{mean}}
mean_na <- function(x, ...) {
    if(is.null(x)) return(NaN)
    return(base::mean(x, na.rm = TRUE, ...))
}


#' @title Calculate Mean And Standard Deviation
#'
#' @description For use with \code{\link[ggplot2]{stat_summary}}
#' @param x Numeric vector
#' @return A data frame with columns y, ymin, and ymax
#' @examples
#' mean_sd(c(NA, 1:100))
#' @export
#' @seealso \code{\link[ggplot2]{mean_se}}
mean_sd <- function(x) {
    mean_ <- mean_na(x)
    sd_ <- sd(x, na.rm = TRUE)
    data.frame(y = mean_, ymin = mean_ - sd_, ymax = mean_ + sd_)
}


#' @title Calculate Median And IQR
#'
#' @description For use with \code{\link[ggplot2]{stat_summary}}
#' @param x Numeric vector
#' @param type Integer: an integer between 1 and 9. See \code{type} in
#'  \code{\link[stats]{quantile}}
#' @return A data frame with columns y, ymin, and ymax
#' @examples
#' median_iqr(c(NA, 1:100))
#' @export
#' @seealso \code{\link[ggplot2]{mean_se}}
median_iqr <- function(x, type = 2) {
    median_ <- stats::quantile(x, probs = 0.5, na.rm = TRUE, type = 2)
    q1_ <- stats::quantile(x, probs = 0.25, na.rm = TRUE, type = 2)
    q3_ <- stats::quantile(x, probs = 0.75, na.rm = TRUE, type = 2)
    data.frame(y = median_, ymin = q1_, ymax = q3_)
}


#' @title R version ternary function
#'
#' @description 
#' @param obj_x Object x
#' @param obj_y Object y
#' @return \code{obj_x} if \code{expr} is evaluated \code{TRUE} and
#'  \code{obj_y} otherwise
#' @examples
#' ternary(TRUE, list(), list(a = 10))
ternary <- function(expr, obj_x, obj_y) {
    stopifnot(is.logical(expr))
    if(expr)
        return(obj_x)
    else
        return(obj_y)
}
