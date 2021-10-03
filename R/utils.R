.unit_classes=function(){
  system.file("extdata", "unit_classes.csv", package = "AOunits")
}
units_classes=function(){
  fread(.unit_classes())
}
get_unit_xml=function(name,tree=NULL){

  .get_unit_xml=function(name,x){
    one=xml_find_all(x,xpath=glue('//aliases//*[text()="{name}"]'))

    if(len(one)==0)
      one=xml_find_all(x,xpath=glue('//name//*[text()="{name}"]'))
    if(len(one)==0)
      one<- xml_find_all(x,xpath=glue('//symbol[text()="{name}"]'))

    if(len(one)==0)stop("Name not found in DB")
    out=one %>% xml_parents()
    out[xml_name(out)=='unit']
  }

  if(!is.null(tree)){
    return(.get_unit_xml(name,tree))
  }
  uxml<-load_units_xml()
  out<-try(.get_unit_xml(name,uxml$common),silent=T)
  if(!is_error(out))return(list(common=out))
  out<-try(.get_unit_xml(name,uxml$derived),silent=T)
  if(!is_error(out))return(list(derived=out))
  out<-try(.get_unit_xml(name,uxml$accepted),silent=T)
  if(!is_error(out))return(list(accepted=out))
  list(base=.get_unit_xml(name,uxml$base))

}
load_units_xml=function(type=c('base','derived','accepted','common',"prefixes")){
  assert_subset(type,c('base','derived','accepted','common',"prefixes"))
  paths<-c("udunits2-base.xml",
           "udunits2-derived.xml",
           "udunits2-accepted.xml",
           "udunits2-common.xml",
           "udunits2-prefixes.xml")
  plist=paths %>% as.list()


  names(plist)<-c('base','derived','accepted','common',"prefixes")
  x<-plist$derived
  x=glue('{system.file("share","udunits",package = "units")}/{x}')


  all_units<-lapply(plist[type],function(x) read_xml(glue('{system.file("share","udunits",package = "units")}/{x}')))
  all_units
}
write_units_xml=function(x){

  if(is.list(x)&len(x)!=1||!is(x[[1]],"xml_document")||!is_named(x)||names(x)%nin%c('base','derived','accepted','common','prefixes'))
    stop("x must be a list length of 1, contents containing an XML document, and must be name one of the following:'base','derived','accepted','common','prefixes',")
  paths<-c("udunits2-base.xml",
           "udunits2-derived.xml",
           "udunits2-accepted.xml",
           "udunits2-common.xml",
           "udunits2-prefixes.xml")
  path<-paths[grep(names(x),paths)]
  xbackup=load_units_xml(names(x))
  tmp_path<-paste0("f",str_remove_all(Sys.time(),"[[:punct:]]"),path)
  write_xml(xbackup[[1]],glue('~/AOunits/backupUnitsDB/{tmp_path}'))
  write_xml(x[[1]],glue('{system.file("share","udunits",package = "units")}/{path}'))
}
get_user_defined_units=function(x){
  assert_multi_class(x,classes=c("xml_document","xml_node","xml_nodeset"))
  xml_find_all(x,"//userdefined") %>% xml_parent()
}
# look=valid_udunits()
# set_units(30,'mi')/set_units(3.785412e-3,'m^3')
# unit_formula('gallon')
get_unit_formula=function(x){
  assert_class(x,classes='xml_nodeset')
  formula<- xml_children(x)[xml_name( xml_children(x))=='def'] %>%       xml_text()
  name<-xml_children(x)[xml_name( xml_children(x))=='name'] %>% xml_text()
  aliases<-xml_children(x)[xml_name( xml_children(x))=='aliases'] %>% xml_text()
  c(name,aliases)[1]
  names(formula)<- c(name,aliases)[1]
  formula
}
get_user_defined_unit_dependencies=function(){
  ulist<-load_units_xml(c('common','derived','accepted'))


  out<-lapply(ulist,get_user_defined_units)
  out<-out[sapply(out,len)>0]
  if(len(out)==0)return()
  fd<-lapply(out,function(x)lapply(x,get_unit_formula)) %>% unlist()

  out<-lapply(parse_exprs(fd),expr_find_names)

  out=lapply(out,function(x)unlist(lapply(x,get_unit_aliases),use.names = FALSE))
  names(out)<-names(fd)
  out
}
ud_class=function(x){
  out<-data_type(x)
  if(is_data_type(x,"continuous_numeric")){
    out<-paste0("continuous_numeric_",unit_class(x))
  }
  out
}
#' @export
unit_formula=function(unit_name){
  assert_character(unit_name,len=1)
  xml<-get_unit_xml(name=unit_name)
  if(names(xml)=='base')return(NULL)
  get_unit_formula(xml[[1]])
}
is_mass_units=function(x){
  is_convertable_unit_pair(get_units(x),'kg')
}
is_speed_units=function(x){
  is_convertable_unit_pair(get_units(x),'m/s')
}
is_capacitance_units=function(x){
  is_convertable_unit_pair(get_units(x),'C/V')
}
is_power_units=function(x){
  is_convertable_unit_pair(get_units(x),'J/s')
}
is_duration_units=function(x){
  is_convertable_unit_pair(get_units(x),'s')
}
is_energy_units=function(x){
  is_convertable_unit_pair(get_units(x),'J')
}
is_time_frequency_units=function(x){
  is_convertable_unit_pair(get_units(x),'min/hr')
}
is_distance_units=function(x){
  is_convertable_unit_pair(get_units(x),'m')
}
is_luminous_intensity_units=function(x){
  is_convertable_unit_pair(get_units(x),'cd')
}
is_chemical_mass_units=function(x){
  is_convertable_unit_pair(get_units(x),'mole')
}
is_force_units=function(x){
  is_convertable_unit_pair(get_units(x),'kg*m/s^2')
}
is_pressure_units=function(x){
  is_convertable_unit_pair(get_units(x),'N/m^2')
}
is_inductance_units=function(x){
  is_convertable_unit_pair(get_units(x),'Wb/A')
}
is_illumination_units=function(x){
  is_convertable_unit_pair(get_units(x),'lm/m^2')
}
is_catalytic_activity_units=function(x){
  is_convertable_unit_pair(get_units(x),'mol/s')
}
is_radiation_units=function(x){
  is_convertable_unit_pair(get_units(x),'J/kg')
}
is_volume_units=function(x){
  is_convertable_unit_pair(get_units(x),'m^3')
}
is_area_units=function(x){
  is_convertable_unit_pair(get_units(x),'m^2')
}
is_acceleration_units=function(x){
  is_convertable_unit_pair(get_units(x),'cm/s^2')
}
is_electric_current_units=function(x){
  is_convertable_unit_pair(get_units(x),'A')
}
is_temperature_units=function(x){
  is_convertable_unit_pair(get_units(x),'K')
}
is_mass_density_units=function(x){
  is_convertable_unit_pair(get_units(x),'kg/m')
}
is_density_units=function(x){
  is_convertable_unit_pair(get_units(x),'kg/m^3')
}
is_value_units=function(x){
  is_convertable_unit_pair(get_units(x),'dollar')
}
is_units_number=function(x){
  is(x,'units')&is_unitless(x)
}
no_units=function(x){
  !is(x,'units')
}



#' @export
data_type=function(x,tail=FALSE){
  if(is(x,"grouped aggregates"))return("grouped aggregates")
  out=c(base_agg_data_type(x),base0_data_type(x),base1_data_type(x),base2_data_type(x))
  out=paste(out,collapse ="_")
  if(grepl("float|integer",out)){
    un<-units_type(x)
    if(un!=" ")
      out=paste(out,un,sep="_")
  }
  if(tail)
    out<-last(str_split(out,'_')[[1]])

  out
}
#' @export
is_data_type=function(data,type,combine="|",is=TRUE){
  assert_choice(combine,c("&","|"))
  assert_choice(is,c(TRUE,FALSE))
  dtype=c("unaggregated","aggregated","mass","speed","capacitance","power","duration","energy","time_frequency","distance","luminous_intensity","chemical_mass","force","pressure","inductance","illumination","catalytic_activity","radiation","volume","area","acceleration","electric_current","temperature","mass_density","density","date_time","units","float","date","integer","logical","factor","ordered","character","identity","continuous")
  #dtype2<-paste0(dtype,"$")
  type2=type
  type=str_remove_all(type,'[\\!\\$]')
  assert_subset(type,dtype)
  type=type2
  type[grepl("\\!",type)]<-str_remove_all(type[grepl("\\!",type)],'\\!') %>% not_containing()
  type[type%in%c("unaggregated","aggregated")]<-paste0("^",  type[type%in%c("unaggregated","aggregated")])
  data_type=data_type(data)
  out<-sapply(type,str_detect,string=data_type)
  if(combine=="|")
    return(any(out)==is)
  if(combine=="&")
    return(all(out)==is)
}
#' @export
check_dtype=function(x,dtype,combine="|",is=TRUE){
  res<-is_data_type(x,dtype,combine=combine,is=is)
  if(res==FALSE){

    dtype=str_remove(dtype,"\\$")
    out=str_replace_all(data_type(x),"_"," ")
    if(all(dtype %in% c("unaggregated", "aggregated", "continuous", "identity")))
      out = str_split(out, " ")[[1]][1:2]%sep%" "

      if (all(dtype %nin% c("unaggregated", "aggregated")))
        out = out %>%
          str_remove_all("unaggregated") %>%
          str_remove_all("aggregated")
      if (all(dtype %nin% c("continuous", "identity")))
        out = out %>%
          str_remove_all(" continuous") %>%
          str_remove_all(" identity")
    cp="or"
    if(combine=="&") cp="and"
    if(is==FALSE)return(glue("Input must not be {dtype%sep%cp}"))
    return(glue("input must be {dtype%sep%cp}, not {str_trim(out)}"))
  }
  res
}
#' @export
assert_dtype=function(x,dtype,combine="|",is=TRUE,silent=FALSE,.vname=NULL,.fname=NULL){
  xname=enexpr(x)
  if(!is.null(.vname)) xname=.vname
     fname=""
  if(!is.null(.fname))
    fname=glue(' in {.fname}({xname})')
  check<-check_dtype(x,dtype,combine,is)
  if(isTRUE(check))return(invisible(x))
  g_stop("Incompatible variable '{xname}'{fname}: {check}",silent=silent)
}
check_unit_compatibility<-function(x,y){

  if(!is(x,'units')&is(y,'units'))
    return(glue("Cannot convert 'unitless' into {get_units(y)}"))
  if(is(x,'units')&!is(y,'units'))
    return(glue("Cannot convert {get_units(x)} into 'unitless'"))
  if(!is(x,'units')&!is(y,'units'))
    return(TRUE)
  ux=get_units(x)
  uy=get_units(y)
  res<-is_convertable_unit_pair(ux,uy)
  if(isFALSE(res))
    return(glue("Cannot convert '{ux}' into '{uy}'"))
  return(TRUE)
}
#' @export
assert_unit_compatibility<-function(x,y,.xname,.yname,.fname=NULL,silent=FALSE){
  res=check_unit_compatibility(x,y)
  if(isTRUE(res))return(invisible(res))
  if(!is.null(.fname))
  g_stop("Incompatible units [ '{expr_to_str(.xname)}' {.fname} '{expr_to_str(.yname)}' ]\n{res}",silent=silent)
  g_stop("Incompatible units [ '{expr_to_str(.xname)}' , '{expr_to_str(.yname)} ]\n{res}",silent=silent)
}
#' @export
`%dtype%`<-function(x,class){
  dnames<-x%dtype_nms%class
  if(is(x,'data.table')){
   ..dnames=dnames
   return(x[,..dnames])
  }
  x[dnames]
}


#' @export
`%dtype_nms%`<-function(x,dtype){
  dtype<-enexpr(dtype)
  dtype<-expr_remove_parenthesis(dtype)
  if(is_call(dtype,'c')|is.character(dtype)){
    bod<-expr({is_data_type(x,!!dtype)})
    fn<-new_function(pairlist2(x=),bod)
  }
  if(is_call(dtype,c('&',"|"))){
    strings<-expr_find_strings(dtype)
    replace<-expr_glue('is_data_type(x,"{strings}")')
    dtype=expr_find_replace_all(as.list(strings),replace,dtype)
    fn<-new_function(pairlist2(x=),expr({!!dtype}))
  }
 names(x)[sapply(x,fn)]
}
#' @export
is_all_dtype=function(...,dtype){
  if(dtype=="numeric")dtype="float|integer"
  dtypes<-sapply(.(...),data_type)
  all(grepl(dtype,dtypes))
}
#' @export
data_compatibiliy_type=function(x){

  dtype<-data_type(x)
  agg_type="unaggregated"
  btype=NULL
  if(grepl("^aggregated",dtype))agg_type='aggregated'
  if(grepl("float|integer",dtype))btype="numeric"
  if(grepl("identity",dtype))btype="identity"
  if(grepl("date",dtype))btype="date"
  if(grepl("ordered",dtype))btype="ordered"
  if(is_null(btype))g_stop("incompatible data object of type: {typeof(x)}")
  return(paste0(agg_type,"_",btype))
}
#' @export
assert_data_stackable=function(...){
  dtypes<-sapply(.(...),data_compatibiliy_type)
  if(!all(grepl('unaggregated',dtypes)))
    g_stop("All group variables must be either:
           1) aggregated and a compatible type.
           2) unaggregated and a compatible type.
           not [{dtypes%sep%','}]")
  dtypes2=str_remove(dtypes,"unaggregated_") %>% str_remove("aggregated_")
  if(l(unique(dtypes2))!=1)
    g_stop("All group variables must be a compatible type, not [{dtypes2%sep%','}]")
  return(invisible(dtypes))
}
#' @export
assert_data_meltable=function(...){
  dtypes<-sapply(.(...),data_compatibiliy_type)
  if(!all(grepl('^aggregated',dtypes)))
    g_stop("All group variables must be either:
           1) aggregated and a compatible type.
           2) unaggregated and a compatible type.
           not [{dtypes%sep%','}]")
  dtypes2=str_remove(dtypes,"unaggregated_") %>% str_remove("aggregated_")
  if(l(unique(dtypes2))!=1)
    g_stop("All group variables must be a compatible type, not [{dtypes2%sep%','}]")
  return(invisible(dtypes))
}


