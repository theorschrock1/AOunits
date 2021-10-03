#' Get the unit denominator of an numeric vector.

#' @name unit_denominator
#' @param x  [numeric]
#' @return  [Character] The denominator formula
#' @export
unit_denominator<-function (x)
{
    assert_numeric(x)
    if (!has_units(x))
        return(NULL)
    paste(units(x)$denominator,collapse="*") %or% NULL
}

