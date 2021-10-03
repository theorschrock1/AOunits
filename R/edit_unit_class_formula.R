#' Edit a base unit to dataable
#'
#'

#' @name edit_unit_class_formula
#' @param unit_name the unit name
#' @param formula the formula in langauge format
#' @return Invisibly returns the base_units DB
#' @export
edit_unit_class_formula=function(unit_name,formula){
  formula<-deparse(enexpr(formula))

  expr(set_units(1,!!formula)) %>% eval()
  bu<-fread(.unit_classes())
  if(unit_name%nin%bu$unit)stop(glue('unit name "{unit_name}" does not exist in database'))
  if( formula%in%bu$Formula  )stop(glue('{formula} exists in database'))
  tmp<-sapply(bu$Formula,is_convertable_unit_pair,formula)
  if(any(tmp)){
    stop(glue('Formula already represented by {glue_collapse(names(tmp[tmp==T]),sep=",")}'))
  }
  bu[unit==unit_name,Formula:=formula]
  fwrite(bu,.unit_classes())
  invisible(bu)
}

