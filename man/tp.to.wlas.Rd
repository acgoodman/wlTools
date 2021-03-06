% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TotalPressureToWaterLevelAboveSensor.R
\name{tp.to.wlas}
\alias{tp.to.wlas}
\title{Calculate compensated water levels above sensor}
\usage{
tp.to.wlas(logger.data, baro.data, h2o.density)
}
\arguments{
\item{logger.data}{a data frame of standardized logger pressure data. Must have fields 'time', 'total.pressure'}

\item{baro.data}{a data frame of baro pressure data. Must have fields 'time', 'baro.pressure'}

\item{h2o.density}{the actual water density in kg/m^3}
}
\description{
This function performs barometric compensation and density compensation
}
\examples{
tp.to.wlas(petaluma.standardized, bj.baro.standard, 1080.34)
}
