#' Test if a unit is a count.

#' @name is_unit_count
#' @param x  [numeric]
#' @return  Logical
#' @export
is_unit_count<-function (x)
{
    assert_numeric(x)
    unit_numerator(x) == "count" && is_null(unit_denominator(x))
}
