#' Test if a numeric vector's demoninator is of a certain unit class.

#' @name is_unit_demoninator
#' @param x  [numeric]
#' @param unit_class  [choice]
#' @return  Logical
#' @export
is_unit_demoninator<-function (x, unit_class)
{
    assert_numeric(x)
    bu <- fread(.unit_classes(), key = "unit")
    assert_choice(unit_class, c(bu$unit, "count", "unitless"))
    if (!has_units(x) & unit_class == "unitless")
        return(TRUE)
    if (!has_units(x))
        return(FALSE)
    dem = unit_denominator(x)
    if (len(dem) == 0 & unit_class == "unitless")
        return(TRUE)
    if (len(dem) == 0)
        return(FALSE)
    is_convertable_unit_pair(dem, bu[unit_class]$Formula)
}
