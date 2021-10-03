#' Remove a unit class from unit_class database
#'
#'

#' @name remove_unit_class
#' @param unit_name the unit name

#' @return Invisibly returns the unit_class DB
#' @export
remove_unit_class=function(unit_name){

  bu<-fread(.unit_classes())

  if(unit_name%nin%bu$unit)stop(glue('unit name "{unit_name}" does not exist in database'))
  bu<-bu[unit!=unit_name]
  fwrite(bu,.unit_classes())
  invisible(bu)
}
