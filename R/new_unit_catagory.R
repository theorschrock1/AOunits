#' Add a new base unit class to database
#'
#'

#' @name new_unit_class
#' @param unit_name the unit name
#' @param formula the formula in langauge format
#' @return Invisibly returns the unit_class DB

new_unit_class=function(unit_name,formula){
  formula<-deparse(enexpr(formula))
  expr(set_units(1,!!formula)) %>% eval()
  bu<-fread(.unit_classes())
  if(unit_name%in%bu$unit)stop(glue('{unit_name} exists in database'))
  if( formula%in%bu$Formula  )stop(glue('{formula} exists in database'))
  tmp<-sapply(bu$Formula,is_convertable_unit_pair,formula)
  if(any(tmp)){
    stop(glue('Formula already represented by {glue_collapse(names(tmp[tmp==T]),sep=",")}'))
  }
  newbu=rbindlist(list(bu,data.table(unit=unit_name,Formula=formula)), use.names=TRUE)
  fwrite(newbu,.unit_classes())
  invisible(newbu)
}

