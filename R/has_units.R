#' Test if a numeric vector has defined units.

#' @name has_units
#' @param x  [numeric]
#' @return  Logical
#' @export
has_units<-function (x)
{
    assert_numeric(x)
    assert_true(is_data_type(x,c("float","integer")))
    is(x, "units")
}
#' @export
is_unitless=function(x){
  assert_unitable(x)
  if(!is(x,"units"))
    return(TRUE)
  x<-units(x)
  if(len0(x$numerator)&len0(x$denominator))
    return(TRUE)
  return(FALSE)
}

