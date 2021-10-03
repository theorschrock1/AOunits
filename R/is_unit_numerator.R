#' Test if a numeric vector numerator is of a certain unit class.

#' @name is_unit_numerator
#' @param x  [numeric]
#' @param unit_class  [choice]
#' @return  Logical
#' @export
is_unit_numerator<-function (x, unit_class)
{
    assert_numeric(x)
    bu <- fread(.unit_classes(), key = "unit")
    assert_choice(unit_class, c(bu$unit, "count", "unitless"))
    if (!has_units(x) & unit_class == "unitless")
        return(TRUE)
    if (!has_units(x))
        return(FALSE)
    num = unit_numerator(x)
    if (len(num) == 0 && unit_class == "unitless")
        return(TRUE)
    if (len(num) == 0)
        return(FALSE)
    if (num == "count" & unit_class == "count")
        return(TRUE)

    is_convertable_unit_pair(num, bu[unit_class]$Formula)
}
