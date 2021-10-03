#' Rename a unit class
#'
#'

#' @name rename_unit_class
#' @param old_name the old unit name
#' @param new_name the new unit name
#' @return Invisibly returns the unit_class DB
#' @export
rename_unit_class=function(old_name,new_name){

  bu<-fread(.unit_classes())
  if(new_name%in%bu$unit)stop(glue('unit name "{new_name}" exists in database'))
  if(old_name%nin%bu$unit)stop(glue('unit name "{old_name}" does not exist in database'))
  bu[unit==old_name,unit:=new_name]
  fwrite(bu,.unit_classes())
  invisible(bu)
}
