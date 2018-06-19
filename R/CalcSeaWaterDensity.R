#' Calculate density of seawater
#'
#' Calculates density in kg/m^3, after Millero and Poisson (1981)
#' @param temperature temperature in degrees C
#' @param salinity salinity in PSU
#' @export
#' @examples seawater.density(25, 30)
seawater.density <- function(temperature, salinity){
  999.842594+0.06793952*temperature-
    0.00909529*temperature^2+
    0.0001001685*temperature^3-
    0.000001120083*temperature^4+
    0.000000006536322*temperature^5+
    0.824493*salinity-
    0.0040899*temperature*salinity+
    0.000076438*temperature^2*salinity-
    0.00000082467*temperature^3*salinity+
    0.0000000053875*temperature^4*salinity-
    0.00572466*salinity^1.5+
    0.00010227*temperature*salinity^1.5+
    0.0000016546*temperature^2*salinity^1.5+
    0.00048314*salinity^2
}

