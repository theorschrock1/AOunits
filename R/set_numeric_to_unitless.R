#'  Numeric/units class conversion
#'
#' Set all numeric variables in a data_table not already classed as a unit to class 'units' with unit_class 'unitless'


#' @name set_numeric_to_unitless
#' @param x  [data_table]
#' @return \code{set_numeric_to_unitless}: invisible([data.table])
#' @examples
#' DT=data.table(x=set_units(rnorm(5),"m"),y=rnorm(5),z=rnorm(5),d=Sys.time(),e=letters[1:5])
#'  sapply(DT, class)
#'  sapply(DT, unit_class)
#'  set_numeric_to_unitless(DT)
#'  sapply(DT, class)
#'  sapply(DT, unit_class)

#' @export
set_numeric_to_unitless <- function(x) {
    # Set all numeric variables in a data.table not already classed as a
    # unit to class 'units' with unit_class 'unitless'
    assert_data_table(x)
    tmp = sapply(x, function(x) is_unitable(x) && (!has_units(x) || is_unit(x,
        "unitless")))

    unitless_names = names(tmp)[tmp]

    if(len0(unitless_names))return(invisible(x))
    invisible(expr_eval(x[, `:=`(!!unitless_names, map_units(.SD)), .SDcols = !!unitless_names]))

    # Returns: invisible([data.table])
}
