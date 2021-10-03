#' Determine if a unit is user defined.
#'
#'

#' @name is_unit_user_defined
#' @param unit_name  [character]  Must have an exact length of 1.
#' @return  [Logical]

#' @examples

#'  is_unit_user_defined("kg")
#' @export
is_unit_user_defined<-function (unit_name)
{
    assert_character(unit_name, len = 1)
    uxml <- get_unit_xml(unit_name)[[1]]
    "userdefined" %in% xml_name(xml_children(uxml))
}
