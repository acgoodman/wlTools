# script to calculate water level above sensor using variable salinity

#' Calculate compensated water levels above sensor using monthly water densities
#'
#' This function performs barometric compensation and density compensation using monthly water quality data
#' @param logger.data a data frame of standardized logger pressure data. Must have fields 'time', 'total.pressure'
#' @param baro.data a data frame of baro pressure data. Must have fields 'time', 'baro.pressure'
#' @param wq.df is a data frame of water quality by month and year. Must have month and year as
#' 'Mo' and 'Yr' respectively and mean temperature and salinity as 'MeanT' and 'MeanS' respectively
#' @export
#' @examples tp.to.wlas(petaluma.standardized, bj.baro.standard, 1080.34)

tp.to.wlas.vardens<-function(logger.data, baro.data, wq.df) {
  logger.data.columns <- c('time', 'total.pressure')
  baro.data.columns <- c('time', 'baro.pressure')
  if(!length(intersect(logger.data.columns, colnames(logger.data))) == length(logger.data.columns))
    stop('Logger data must have columns: "time", "total.pressure"')
  if(!length(intersect(baro.data.columns, colnames(baro.data))) == length(baro.data.columns))
    stop('Baro data must have columns: "time", "baro.pressure"')
  if(!any(class(logger.data$time) == 'POSIXct'))
    stop('Logger data time must be POSIXct')
  if(!any(class(baro.data$time) == 'POSIXct'))
    stop('Baro data time must be POSIXct')
  if(min(logger.data$time) < min(baro.data$time) || max(logger.data$time) > max(baro.data$time))
    warning(sprintf('URGENT: Time range for logger data (%s - %s) is not contained within time range for baro data (%s - %s). Data will be incorrectly compensated.', format(min(logger.data$time), '%Y-%m-%d %H:%M'), format(max(logger.data$time), '%Y-%m-%d %H:%M'), format(min(baro.data$time), '%Y-%m-%d %H:%M'), format(max(baro.data$time), '%Y-%m-%d %H:%M')))
  if(min(na.omit(logger.data$total.pressure)) < 90 || max(logger.data$total.pressure > 150))
    warning('Logger data total pressure not contained in range 90:150. Check units (should be kPa).')
  if(min(na.omit(baro.data$baro.pressure)) < 90 || max(baro.data$baro.pressure > 150))
    warning('Baro pressure not contained in range 90:150. Check units (should be kPa).')
  
  baro.compensation <- function(logger.data, baro.data) {
    logger.dt <- data.table(na.omit(logger.data), key = 'time')
    logger.dt <- logger.data %>%
      filter(!is.na(time),
             !is.na(total.pressure)) %>%
      data.table(key = 'time')
    baro.dt <- data.table(filter(baro.data,!is.na(time), !is.na(baro.pressure)),
                          key = 'time')
    logger.baro.dt <- baro.dt[logger.dt, roll = 'nearest']
    logger.baro.dt %>%
      mutate(water.pressure = total.pressure - baro.pressure) %>%
      select(-total.pressure, -baro.pressure)
    
  }
  
  density.conversion <- function(h2o.df){
    #data frame must have fields 'MeanT' and 'MeanS'
    #must have month and year as 'Mo' and 'Yr'
    h2o.df %>%
      mutate(
        wdens=999.842594+0.06793952*MeanT-
          0.00909529*MeanT^2+
          0.0001001685*MeanT^3-
          0.000001120083*MeanT^4+
          0.000000006536322*MeanT^5+
          0.824493*MeanS-
          0.0040899*MeanT*MeanS+
          0.000076438*MeanT^2*MeanS-
          0.00000082467*MeanT^3*MeanS+
          0.0000000053875*MeanT^4*MeanS-
          0.00572466*MeanS^1.5+
          0.00010227*MeanT*MeanS^1.5+
          0.0000016546*MeanT^2*MeanS^1.5+
          0.00048314*MeanS^2)
  }
  
  level.conversion <- function(logger.data, h2o.df) {
    h2o.df <- density.conversion(h2o.df)
    h2o.dens<-transmute(h2o.df, mo=Mo, yr=Yr, wdens=wdens)
    logger.merged<-logger.data %>%
      mutate(mo=lubridate::month(time), yr=lubridate::year(time))%>%
      inner_join(h2o.dens, by=c('yr','mo'))
    logger.merged %>%
      mutate(water.level.above.sensor = water.pressure * 1000 / (wdens * 9.80665)) %>%
      select(-water.pressure)
  }
  
  logger.data %>%
    baro.compensation(baro.data) %>%
    level.conversion(wq.df)
}