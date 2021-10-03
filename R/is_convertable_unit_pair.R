#' Check if a unit pair is convertible
#'
#'

#' @name is_convertable_unit_pair
#' @param x A character string which is parseable into a udunits compatible unit.
#' @param y Another character string which is also parseable into a udunits compatible unit.
#' @return Logical. True if unit pair is convertable
#' @export
is_convertable_unit_pair=function(x,y){
  expr(ud_are_convertible(!!x,!!y)) %>%
    eval(envir=environment(units::set_units))
}
