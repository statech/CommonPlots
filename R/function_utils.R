#' @title Test If An Object Is Empty
#'
#' @description An empty object is any of the following: \code{NULL}, \code{NA}
#'  or vector of \code{NA}s, Object of length \code{0}, \code{''} if
#'  \code{empty_str_triggers} equals to \code{TRUE}, and \code{FALSE} if
#'  \code{false_triggers} equals to \code{TRUE}
#' @param obj Object: an object to be tested
#' @param empty_str_triggers Logical: \code{TRUE} (default) treats empty string
#'  (i.e. \code{''}) as empty and \code{FALSE} otherwise
#' @param false_triggers Logical: \code{TRUE} treats logical value \code{FALSE}
#'  as empty and \code{FALSE} (default) otherwise
#' @return A logical value that indicates whether the tested object is empty or
#'  not
#' @examples
#' is_blank(NA)
#' is_blank('')
#' @export
is_blank <- function(obj, empty_str_triggers = TRUE, false_triggers = FALSE) {
    if(is.function(obj)) return(FALSE)
    return(
        is.null(obj) ||
            length(obj) == 0 ||
            all(is.na(obj)) ||
            (empty_str_triggers && all(obj == '')) ||
            (false_triggers && all(!obj))
    )
}


#' @title Test If A Column Is Found in A Data Frame
#'
#' @description A function that check if \code{column} is one of \code{df}'s
#'  columns and returns an error message if not.
#' @param df List/Data frame
#' @param column Character: A column name
#' @param error_message Character: user-defined error message
#' @param call_ Logical: logical, indicating if the call should become part of
#'  the error message. See \code{call.} in \code{\link[base]{stop}}
#' @return Error message if test fails
#' @examples
#' test_df <- data.frame(a = 1:10, b = 11:20)
#' column_in_dataframe(test_df, 'a')
#' \dontrun{
#' column_in_dataframe(test_df, 'c')
#' }
#' @export
column_in_dataframe <- function(df, column, error_message = NULL, call_ = F) {
    if(!column %in% names(df)) {
        df_name <- deparse(substitute(df))
        column_name <- deparse(substitute(column))
        if(is.null(error_message)) {
            error_message <- paste0(
                df_name, " doesn't have a column named '", column_name, "'"
            )
        }
        stop(error_message, call. = call_)
    }
}


#' @title Argument Verification
#'
#' @description Tests if a passed-in argument takes a value from a table of
#'  candidate values and returns an error message if not.
#' @param arg Character: passed-in argument name
#' @param choices Character vector: a character vector of candidate values
#' @param error_message Character: user-defined error message
#' @param call_ Logical: logical, indicating if the call should become part of
#'  the error message. See \code{call.} in \code{\link[base]{stop}}
#' @return Error message if test fails
#' @examples
#' alignment <- 'left'
#' arg_in_choices(alignment, c('left', 'center', 'right'))
#' @export
arg_in_choices <- function(arg, choices, error_message = NULL, call_ = F) {
    if(!all(arg %in% choices)) {
        arg_name <- deparse(substitute(arg))
        if(is.null(error_message)) {
            error_message <- paste0(
                arg_name, ' must be one of the following:\n',
                paste(choices, collapse = '\n')
            )
        }
        stop(error_message, call. = call_)
    }
}


#' @title Variable Class Test
#'
#' @description Tests if a variable of a certain class and returns an error
#'  message if not
#' @param var Object: an object for class test
#' @param class_method Function vector: class test function, e.g.,
#'  \code{\link[base]{is.numeric}}, \code{\link[base]{is.logical}} and etc.
#' @param class_name Character: name of the class
#' @param error_message Character: user-defined error message
#' @return Error message if test fails
#' @examples
#' obj <- 1:10
#' check_var_class(obj, is.numeric, 'numeric')
#' @export
check_var_class <- function(var, class_method,
                            class_name, error_message = NULL) {
    class_method_name <- deparse(substitute(class_method))
    if(!is.function(class_method)) {
        stop(paste0(class_method_name, ' must be a callable function'))
    }
    if(!class_method(var)) {
        var_name <- deparse(substitute(var))
        if(is.null(error_message)) {
            error_message <- paste0(
                var_name, " must be of class '", class_name, "'"
            )
        }
        stop(error_message, call. = call_)
    }
}






























