#' Finds all compatible units within a vector of units.

#' @name which_units_compatible
#' @param x  [character]  Must have an exact length of 1.
#' @param table  [character]
#' @return \code{which_units_compatible}: [Logical] All units compatible with x.
#' @examples
#'  x = 'm'
#'  table = c('km', 'J', 'W', 'N', 'r', 's', 'day')
#'  which_units_compatible(x, table)
#' @export
which_units_compatible <- function(x, table) {
    # Finds all compatible units within a vector of units
    assert_character(x, len = 1)
    assert_character(table)
    sapply(table, is_convertable_unit_pair, x)
    # Returns: [Logical] All units compatible with x.
}
