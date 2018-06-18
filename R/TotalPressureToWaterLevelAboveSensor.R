

#' Calculate compensated water levels above sensor
#'
#' This function performs barometric compensation and density compensation
#' @param logger.data a data frame of standardized logger pressure data. Must have fields 'time', 'total.pressure'
#' @param baro.data a data frame of baro pressure data. Must have fields 'time', 'baro.pressure'
#' @param h2o.density the actual water density in kg/m^3
#' @export
#' @examples tp.to.wlas(petaluma.standardized, bj.baro.standard, 1080.34)

# Total pressure to water level above senseor
tp.to.wlas <- function(logger.data, baro.data, h2o.density) {
  
  baro.compensation <- function(logger.data, baro.data) {
    logger.dt <- logger.data %>%
      filter(!is.na(time),
             !is.na(total.pressure)) %>%
      data.table(key = 'time')
    baro.dt <- data.table(filter(baro.data,!is.na(time),
                                 !is.na(baro.pressure)),
                          key = 'time')
    logger.baro.dt <- baro.dt[logger.dt, roll = 'nearest']
    logger.baro.dt %>%
      mutate(water.pressure = total.pressure - baro.pressure) %>%
      select(-total.pressure, -baro.pressure)
    
  }
  
  level.conversion <- function(logger.data, h2o.density) {
    logger.data %>%
      mutate(water.level.above.sensor = water.pressure * 1000 / (h2o.density * 9.80665)) %>%
      select(-water.pressure)
  }
  
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
  if(h2o.density < 900 || h2o.density > 1100)
    stop('H2O density should be between 950 and 1050 kg/m^3')
  if(min(logger.data$time) < min(baro.data$time) || max(logger.data$time) > max(baro.data$time))
    warning(sprintf('URGENT: Time range for logger data (%s - %s) is not contained within time range for baro data (%s - %s). Data will be incorrectly compensated.', format(min(logger.data$time), '%Y-%m-%d %H:%M'), format(max(logger.data$time), '%Y-%m-%d %H:%M'), format(min(baro.data$time), '%Y-%m-%d %H:%M'), format(max(baro.data$time), '%Y-%m-%d %H:%M')))
  if(min(na.omit(logger.data$total.pressure)) < 90 || max(logger.data$total.pressure > 150))
    warning('Logger data total pressure not contained in range 90:150. Check units (should be kPa).')
  if(min(na.omit(baro.data$baro.pressure)) < 90 || max(baro.data$baro.pressure > 150))
    warning('Baro pressure not contained in range 90:150. Check units (should be kPa).')
  
  logger.data %>%
    baro.compensation(baro.data) %>%
    level.conversion(h2o.density)
}
