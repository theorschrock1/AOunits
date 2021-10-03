#' Get the unit numerator of an numeric vector.

#' @name unit_numerator
#' @param x  [numeric]
#' @return  [Character] The numerator formula
#' @export
unit_numerator<-function (x)
{
    assert_numeric(x)
    if (!has_units(x))
        return(NULL)
  paste(units(x)$numerator,collapse="*") %or% NULL
}


