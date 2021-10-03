#' Test whether a string is a valid unit name.

#' @name is_valid_unit_name
#' @param x  [character(1)]
#' @return  [Logical]

#' @examples

#'  is_valid_unit_name('m')

#'  is_valid_unit_name('J')

#'  is_valid_unit_name('fish')
#' @export
is_valid_unit_name <- function(x) {
    # Test whether a string is a valid unit name
    assert_character(x,len=1)
    out <- try(expr_eval(set_units(1, value = !!x)), silent = T)
    !is_error(out)
    # Returns: [Logical]
}

