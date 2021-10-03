#' Check if an object is unitable.

#' @name check_unitable
#' @param x
#' @return \code{check_unitable}: TRUE if valid. If invalid, returns a string with an error message.
#' @examples

#'  check_unitable(1)
#'  check_unitable(1)
#'  check_unitable(1)
#'  check_unitable('a')
#'  check_unitable(factor('a'))
#'  check_unitable(data.table())
#'  check_unitable(Sys.time())
#'  check_unitable(Sys.Date())
#' @export
check_unitable <- function(x) {
    # Check if an object is unitable
    res = is_unitable(x)
    if (!isTRUE(res))
        return(glue("Must be data_type 'continous_numeric', not '{data_type(x)}'"))
    return(TRUE)
    # Returns: TRUE if valid. If invalid, returns a string with an error
    # message.
}
assert_unitable=checkmate::makeAssertionFunction(check_unitable)
