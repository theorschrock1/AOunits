#' Set or drop units across a list, data.table, or, data.frame
#'
#'

#' @name map_units
#' @param x  [data.table,data.frame,list]  Each column or list element must inherit from at least one of the provided types: [numeric]
#' @param units  [character]  Must have an exact length of or equal to one of the following: [1,length(x)].  NULL is ok.  Defaults to NULL
#' @param drop  [logical] Drop existing units? Defaults to FALSE
#' @param force_list  [logical]  Return as a list? Defaults to FALSE
#' @return  [list,data.table,data.frame] depending on the input class. Returns as a list if specified by 'force_list'.

#' @examples

#'  LST = list(x = rnorm(10), y = rnorm(20), z = rnorm(5))
#'  map_units(LST, units = c('m/s'))
#'  DT = data.table(x = rnorm(50), y = rnorm(50), z = rnorm(50))
#'  DT[, `:=`(c('x', 'y', 'z'), map_units(.SD, units = c('m/s')))]
#'  DT
#' DF = as.data.frame(DT)
#' map_units(DF, units = c('m'), drop = TRUE)
#' DT = data.table(x = rnorm(50), y = rnorm(50), z = rnorm(50))
#' map_units(DT)

#' @export
map_units <- function(x, units = NULL, drop = FALSE, force_list = FALSE) {
    # Set or drop units across a list, data.table, or, data.frame
    assert_any(x, check_data_table(types = "numeric"), check_list(types = "numeric"),
        check_data_frame(types = "numeric"))

    assert_any(units, check_character(len = 1, null.ok = TRUE), check_character(len = length(x)))

    assert_logical(drop)
    assert_logical(force_list)
    is_DT = is(x, "data.table")
    is_DF = is(x, "data.frame")
    if (drop)
        x = map(x, function(x) {
            if (has_units(x))
                x = drop_units(x)
            return(x)
        })
    if(!(drop==T&&is.null(units))){

        if (is.null(units)||l(units) == 1){
            if(!is.null(units)){
            units = rep(units, l(x))
            }else{
            units=lapply(1:l(x),function(x)missing_arg())
            }
        }
        x = map2(x, units, function(x, unit) expr_eval(set_units(x, value = !!maybe_missing(unit))))
    }

    if (is_DT) {
        return(as.data.table(x))
    }

    if (is_DF) {
        return(as.data.frame(x))
    }

    x
    # Returns: [list,data.table,data.frame] depending on the input class.
    # Returns as a list if specified by 'force_list'.
}
