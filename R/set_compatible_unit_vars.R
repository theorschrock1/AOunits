#' Set all units of compatible numeric variables to a common unit.

#' @name set_compatible_unit_vars
#' @param DT
#' @return \code{set_compatible_unit_vars}: invisible([list]) A list of numeric variable names grouped by unit.
#' @examples

#'  non_equal = sapply(DT, get_units, require.numeric = FALSE)
#'  set_compatible_unit_vars(DT)
#'  equal = sapply(DT, get_units, require.numeric = FALSE)
#'  rbind(non_equal, equal)

#' @rdname get_compatible_unit_vars
#' @export
set_compatible_unit_vars <- function(DT) {
    # Set all units of compatible numeric variables to a common unit
    groups_out <- get_compatible_unit_vars(DT)

    groups <- groups_out[sapply(groups_out, function(x) length(x) > 1)]
    units = map2(groups, names(groups), function(x, name) rep(name, l(x))) %>%
        unlist(use.names = FALSE)

    groups = unlist(groups, use.names = FALSE)
    unitless_groups = groups[units == "unitless"]
    unit_groups = groups[units != "unitless"]
    if (!len0(unit_groups))
        expr_eval(DT[, `:=`(!!unit_groups, map_units(.SD, units = !!units)),
            .SDcols = !!unit_groups])

    if (!len0(unitless_groups))
        expr_eval(DT[, `:=`(!!unitless_groups, map_units(.SD)), .SDcols = !!unit_groups])

    invisible(groups_out)
    # Returns: invisible([list]) A list of numeric variable names grouped by unit.
}
