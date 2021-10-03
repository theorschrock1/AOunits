#' Test if a vector of units are all identical units.

#' @name all_units_identical
#' @param x  [character]
#' @return \code{all_units_identical}: [Logical(1)]
#' @examples

#'  x = c('in', 'inches', 'inch')
#'  all_units_identical(x)
#'  x = c('ft', 'inches', 'm')
#'  all_units_identical(x)
#'  x = c('ft', 'inches', 'kg')
#'  all_units_identical(x)
#' @export
all_units_identical <- function(x) {
    # Test if a vector of units are all identical units
    assert_character(x)
    if (!all_units_compatible(x))
        return(FALSE)
    one = lapply(x, function(x) expr_eval(set_units(1, !!x)))
    two = lapply(one, function(x, unit) expr_eval(set_units(x, !!unit)), x[1])
    onet = as.numeric(sapply(one, as.numeric))
    twot = as.numeric(sapply(two, as.numeric))
    isTRUE(all.equal(onet, twot))
    # Returns: [Logical(1)]
}
