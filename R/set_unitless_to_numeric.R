#'  Numeric/units class conversion
#'
#'  Drop units of all 'unitless' variables in a data_table.

#' @name set_unitless_to_numeric
#' @param x  [data_table]
#' @return \code{set_unitless_to_numeric}: invisible([data.table])
#' @examples

#'  set_unitless_to_numeric(DT)

#'  sapply(DT, class)

#' @rdname set_numeric_to_unitless
#' @export
set_unitless_to_numeric <- function(x) {
    # Drop units of all 'unitless' variables in a data_table.
    assert_data_table(x)
    tmp = sapply(x, function(x) is_unitable(x) && (!has_units(x) || is_unit(x,
        "unitless")))

    unitless_names = names(tmp)[tmp]
    if(len0(unitless_names))return(invisible(x))
    invisible(expr_eval(x[, `:=`(!!unitless_names, map_units(.SD, drop = TRUE)),
        .SDcols = !!unitless_names]))

    # Returns: invisible([data.table])
}
