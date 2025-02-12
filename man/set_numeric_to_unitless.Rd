% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_numeric_to_unitless.R,
%   R/set_unitless_to_numeric.R
\name{set_numeric_to_unitless}
\alias{set_numeric_to_unitless}
\alias{set_unitless_to_numeric}
\title{Numeric/units class conversion}
\usage{
set_numeric_to_unitless(x)

set_unitless_to_numeric(x)
}
\arguments{
\item{x}{[data_table]}
}
\value{
\code{set_numeric_to_unitless}: invisible([data.table])

\code{set_unitless_to_numeric}: invisible([data.table])
}
\description{
Set all numeric variables in a data_table not already classed as a unit to class 'units' with unit_class 'unitless'

Drop units of all 'unitless' variables in a data_table.
}
\examples{
DT=data.table(x=set_units(rnorm(5),"m"),y=rnorm(5),z=rnorm(5),d=Sys.time(),e=letters[1:5])
 sapply(DT, class)
 sapply(DT, unit_class)
 set_numeric_to_unitless(DT)
 sapply(DT, class)
 sapply(DT, unit_class)
 set_unitless_to_numeric(DT)
 sapply(DT, class)
}
