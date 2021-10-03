#' Get or set all numeric variables base on units.
#'
#' Groups all numeric variables in a data_table base on compatible unit.

#' @name get_compatible_unit_vars
#' @param DT  [data_table]
#' @param return [choice('list|DT')] Should the function return a named list or data.table?
#' @return \code{get_compatible_unit_vars}: A list of numeric variable names grouped by unit.
#' @examples


#' @export
get_compatible_unit_vars <- function(DT,return='list') {
    # Get or set all numeric variables base on units.  Groups all numeric
    # variables in a data_table base on compatible unit.
    assert_data_table(DT)
    assert_choice(return,choices=c("list",'DT'))
    set_numeric_to_unitless(DT)

    unitable <- DT[, sapply(DT, is_unitable), with = F]
    out = lapply(unitable, deparse_unit)
    table = out %>% unlist(use.names = FALSE)
    out = lapply(out, which_units_compatible, table = table)
    out = out[!duplicated(out)]
    out = lapply(out, function(x, var.names) var.names[x], var.names = names(unitable))
    pvars = names(out)
    names(out) = unlist(lapply(unitable[, ..pvars], get_units))
    names(out)[names(out) == ""] = "unitless"
    if(return=='list')return(out)
    # Returns: A list of numeric variable names grouped by unit.

    common_units = map2(out, names(out), function(x, name) rep(name, l(x))) %>% unlist(use.names = FALSE)
    variable=unlist(out,use.names = FALSE)
    og_units=DT[,sapply(.SD,get_units),.SDcols=  variable]
    unit_classes=DT[,sapply(.SD,unit_class),.SDcols=  variable]
    data.table(variable=variable,common_unit=common_units,current_unit=og_units, unit_class=unit_classes)
    # Returns: a data.table of variable names,original and common units, and unit class
}
