#' Set R vectors with convertible units to the same unit.
#'
#' If argument unit is null, the unit of the first vector in the list will be used.

#' @name set_equal_units
#' @param x  [data.table,list,data.frame]  Each column or list element must inherit from at least one of the provided types: [units]
#' @param unit  [unit_name]  NULL is ok.  Defaults to NULL
#' @return \code{set_equal_units}: [data.table,data.table,or list] depending on input class
#' @examples

#'  DT <- data.table(x = 1:5, y = 2:6, z = rnorm(5))
#'  DT = map_units(DT, units = c('m/s', 'km/hr', 'mi/hr'))
#'  set_equal_units(DT)
#'  set_equal_units(DT[, .(z, y, x)])
#'  set_equal_units(DT[, .(z, y, x)], unit = 'ft/min')

#' @export
set_equal_units <- function(x, unit = NULL) {
    # Set R vectors with convertible units to the same unit. If argument
    # unit is null, the unit of the first vector in the list will be used.
    assert_any(x, check_data_table(types = "units"), check_list(types = "units"),
        check_data_frame(types = "units"))

    assert_unit_name(unit, null.ok = TRUE)
    if (is.null(unit)) {
        unit <- deparse_unit(x[[1]])
    }

    map_units(x, units = unit)
    # Returns: [data.table,data.table,or list] depending on input class
}
