#' Test if a numeric vector has a denominator.

#' @name unit_has_denominator
#' @param x  [numeric]
#' @return  Logical
#' @export
unit_has_denominator<-function (x)
{
    assert_numeric(x)
    if (!has_units(x))
        return(FALSE)
    len(units(x)$denominator) > 0
}
