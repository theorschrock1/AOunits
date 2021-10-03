#' @export
ifelse_fn=function(...,el){
  tmp<-enexprs(...)
  create_if_cond=function(cond,return){
    out=expr(if(cond(x))return(!!return))
    out[[2]][[1]]<-enexpr(cond)
    out
  }
  out=expr(return(!!enexpr(el)))

  bod<-c(map2(tmp,names(tmp),create_if_cond),out)

  rlang::new_function(arg=rlang::pairlist2(x=),body =  expr({!!!bod}))
}
#' @export
is_data_date=function(x){
  is(x,'POSIXt')|is(x,'POSIXct')|is(x,'Date')
}
#' @export
is_data_time=function(x){
  is(x,'POSIXt')|is(x,'POSIXct')
}
#' @export
is_data_aggregrate=function(x){
  is(x,"aggregate")
}
#' @export
base_agg_data_type=function (x)
{
  if (is_data_aggregrate(x))
    return("aggregated")
  return("unaggregated")
}
# base0_data_type=ifelse_fn(
#   continuous=is.double,
#   identity=is.factor,
#   continuous=is.integer,
#   identity=is.character,
#   identity=is.logical,
#   el=typeof(x))
#' @export
base0_data_type=function (x)
{
  if (is.double(x))
    return("continuous")
  if (is.factor(x))
    return("identity")
  if (is.integer(x))
    return("continuous")
  if (is.character(x))
    return("identity")
  if (is.logical(x))
    return("identity")
  return(typeof(x))
}
# base1_data_type=ifelse_fn(date=is_data_date,
#                           factor=is.factor,
#                           character=is.character,
#                           logical=is.logical,
#                           float=is.double,
#                           integer=is.integer,
#                           el=NULL)
#' @export
base1_data_type=function (x)
{
  if (is_data_date(x))
    return("date")
  if (is.factor(x))
    return("factor")
  if (is.character(x))
    return("character")
  if (is.logical(x))
    return("logical")
  if (is.double(x))
    return("float")
  if (is.integer(x))
    return("integer")
  return(NULL)
}

# base2_data_type=ifelse_fn(
#   time=is_data_time,
#   ordered=is.ordered,
#   el=NULL)
#' @export
base2_data_type=function (x)
{
  if (is_data_time(x))
    return("time")
  if (is.ordered(x))
    return("ordered")
  return(NULL)
}
# units_type = ifelse_fn(
#   ' ' = no_units,
#   'units'=is_units_number,
#   'mass_units' = is_mass_units,
#   'speed_units' = is_speed_units,
#   'capacitance_units' = is_capacitance_units,
#   'power_units' = is_power_units,
#   'duration_units' = is_duration_units,
#   'energy_units' = is_energy_units,
#   'time_frequency_units' = is_time_frequency_units,
#   'distance_units' = is_distance_units,
#   'luminous_intensity_units' = is_luminous_intensity_units,
#   'chemical_mass_units' = is_chemical_mass_units,
#   'force_units' = is_force_units,
#   'pressure_units' = is_pressure_units,
#   'inductance_units' = is_inductance_units,
#   'illumination_units' = is_illumination_units,
#   'catalytic_activity_units' = is_catalytic_activity_units,
#   'radiation_units' = is_radiation_units,
#   'volume_units' = is_volume_units,
#   'area_units' = is_area_units,
#   'acceleration_units' = is_acceleration_units,
#   'electric_current_units' = is_electric_current_units,
#   'temperature_units' = is_temperature_units,
#   'mass_density_units' = is_mass_density_units,
#   'density_units' = is_density_units,
#   'value_units' = is_value_units,
#   el = "undefined_units"
# )
#' @export
units_type = function (x)
{
  if (no_units(x))
    return(" ")
  if (is_units_number(x))
    return("units")
  if (is_mass_units(x))
    return("mass_units")
  if (is_speed_units(x))
    return("speed_units")
  if (is_capacitance_units(x))
    return("capacitance_units")
  if (is_power_units(x))
    return("power_units")
  if (is_duration_units(x))
    return("duration_units")
  if (is_energy_units(x))
      return("energy_units")
  if (is_time_frequency_units(x))
    return("time_frequency_units")
  if (is_distance_units(x))
    return("distance_units")
  if (is_luminous_intensity_units(x))
    return("luminous_intensity_units")
  if (is_chemical_mass_units(x))
    return("chemical_mass_units")
  if (is_force_units(x))
    return("force_units")
  if (is_pressure_units(x))
    return("pressure_units")
  if (is_inductance_units(x))
    return("inductance_units")
  if (is_illumination_units(x))
    return("illumination_units")
  if (is_catalytic_activity_units(x))
    return("catalytic_activity_units")
  if (is_radiation_units(x))
    return("radiation_units")
  if (is_volume_units(x))
    return("volume_units")
  if (is_area_units(x))
    return("area_units")
  if (is_acceleration_units(x))
    return("acceleration_units")
  if (is_electric_current_units(x))
    return("electric_current_units")
  if (is_temperature_units(x))
    return("temperature_units")
  if (is_mass_density_units(x))
    return("mass_density_units")
  if (is_density_units(x))
    return("density_units")
  if (is_value_units(x))
    return("value_units")
  return("undefined_units")
}
