#' Extract units from a numeric vector.

#' @name get_units
#' @param x  \code{[numeric]}
#' @param require.numeric  \code{[logical]}  Must have an exact length of \code{1}.  Defaults to \code{TRUE}
#' @return \code{get_units}: \code{[character]} the unit type
#' @examples

#'  x <- set_units(1, 'km*W/s^2/m^2')
#'  get_units(x)
#'  x <- set_units(1, 'mi/gallon')
#'  get_units(x)
#'  x <- set_units(1)
#'  get_units(x)
#'  x <- set_units(1, '1/s')
#'  get_units(x)
#'  x = 'A'
#'  get_units(x, require.numeric = FALSE)
#' @export
get_units <- function(x, require.numeric = TRUE) {
    # Extract units from a numeric vector
    assert_logical(require.numeric, len = 1)
    if (!require.numeric && !isTRUE(check_numeric(x))) 
        return(NA_character_)
    assert_numeric(x)
    as.character(units(x))
    # Returns: \code{[character]} the unit type
}
