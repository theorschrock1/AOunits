#' Test if a vector of units are all compatible.

#' @name all_units_compatible
#' @param x  [character]
#' @return \code{all_units_compatible}: [Logical(1)]
#' @examples

#'  x = c('in', 'inches', 'inch')
#'  all_units_compatible(x)
#'  x = c('ft', 'inches', 'm')
#'  all_units_compatible(x)
#'  x = c('ft', 'inches', 'kg')
#'  all_units_compatible(x)
#' @export
all_units_compatible <- function(x) {
    # Test if a vector of units are all compatible
    assert_character(x)
    all(which_units_compatible(x[1], x))
    # Returns: [Logical(1)]
}
