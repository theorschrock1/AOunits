#' Test if a numeric vector is a specific unit class.

#' @name is_unit
#' @param x  [numeric]
#' @param unit_class  [choice]  Possible values: \code{c( units_classes()$unit, 'count', 'unitless')}.
#' @return \code{is_unit}: [Logical(1)]
#' @examples

#'  x = set_units(1, 'm/s')
#'  y = set_units(1, 'm^2')
#'  z = set_units(1)
#'  is_unit(x, 'speed')
#'  is_unit(y, 'area')
#'  is_unit(z, 'area')
#'  is_unit(z, 'unitless')
#'  is_unit(1, 'unitless')

#' @export
is_unit <- function(x, unit_class) {
    # Test if a numeric vector is a specific unit class
    #bu <- units_classes()
    assert_numeric(x)
    assert_choice(unit_class, choices = c(units_classes()$unit, "count",
        "unitless"))

    unit_class(x) == unit_class
    # Returns: [Logical(1)]
}
