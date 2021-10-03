#' Test if a numeric vector has a numerator.

#' @name unit_has_numerator
#' @param x  [numeric]
#' @return  Logical
#' @export
unit_has_numerator<-function (x)
{
    assert_numeric(x)
    if (!has_units(x))
        return(FALSE)
    len(units(x)$numerator) > 0
}
