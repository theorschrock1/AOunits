#' Assert if a string is a valid unit name.

#' @name assert_unit_name
#' @param x an object to assert

#' @return  \code{assert_unit_name}: If valid, returns x invisibly. If invalid, throws an error

#' @examples
#'  assert <- assert_unit_name('m')
#'  assert
#'  assert_unit_name('mk')

#' @rdname check_unit_name
#' @export
assert_unit_name =function (x, null.ok = FALSE, .var.name = checkmate::vname(x), add = NULL)
{
  if (missing(x))
    stop(sprintf("argument \"%s\" is missing, with no default",
                 .var.name))
  res = check_unit_name(x, null.ok)
  checkmate::makeAssertion(x, res, .var.name, add)
}
