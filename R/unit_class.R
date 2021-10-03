#' Determine the unit class of a numeric vector.

#' @name unit_class
#' @param  x a numeric vector

#' @return  the unit class.  If the vector has units and no class is determined, a formula string will be returned.

#' @examples

#'  x <- set_units(1, "m/s")

#'  unit_class(x)

#'  x <- set_units(1, "m/s^4")

#'  unit_class(x)

#'  unit_class(1)

#' @export
unit_class<-function (x)
{
    if(is_unitable(x)==F)
        return(NA_character_)
    if (!has_units(x)||deparse_unit(x)=="")
        return("unitless")
    if (is_unit_numerator(x, "count") && is_unit_demoninator(x,
        "duration")) {
        return("frequency")
    }
    x = deparse_unit(x)
    if (x == "")
        return()
    if (x == "count")
        return(x)
    bu <- fread(.unit_classes(), key = "unit")
    uf = bu$Formula %>% as.list()
    names(uf) <- bu$unit
    out = sapply(uf, is_convertable_unit_pair, x = x)
    out <- names(out %IN% TRUE)
    if (len(out) == 0)
        return(x)
    out
}
