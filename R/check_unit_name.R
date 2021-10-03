#' Check if a string is a valid unit name.

#' @name check_unit_name
#' @param x an object to check
#' @param null.ok [logical(1)] If set to TRUE, x may also be NULL.
#' @return \code{check_unit_name}: TRUE if valid. If invalid, returns a string with an error message.

#' @examples

#'  check <- check_unit_name('m')
#'  check
#'  check_unit_name('mk')
#' @export
check_unit_name <- function(x,null.ok=FALSE) {
    # Check if a string is a valid unit name
    res = check_character(x, len = 1,null.ok = null.ok)
    if (!isTRUE(res))
        return(res)
    if (is.null(x)&isTRUE(res))
        return(res)
    if (!is_valid_unit_name(x))
        return(g_stop("'{x}' is not a valid unit name", silent = TRUE))
    return(TRUE)
    # Returns: TRUE if valid. If invalid, returns a string with an error
    # message.
}

