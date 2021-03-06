% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CompareLevelsToNOAA.R
\name{compare.to.NOAA}
\alias{compare.to.NOAA}
\title{Compares high tides to NOAA station tides}
\usage{
compare.to.NOAA(data.path, station.id)
}
\arguments{
\item{data.path}{path to CSV file of input data}

\item{station.id}{numeric code for NOAA station}
}
\description{
Compares high tides in water level time series to corresponding NOAA high tides
Input data source must have fields "DateTime" and "Level"
Outputs and prints a ggplot plot with a blue line representing mean difference
}
\examples{
myplot <- compare.NOAA.data('MadRiverLevels.csv', 9418767)
}
