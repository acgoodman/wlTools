

#' Standardize Solinst Levelogger Data
#'
#' This function converts water level data exported from the Levelogger software into kilopascals
#' @param solinst.file the path to your solinst csv file
#' @param programmed.h20.density the water density programmed into the logger on deployment, in g/L. Defaults to 1000
#' @export
#' @examples standardize.solinst('Petaluma01_120815.csv')
standardize.solinst <- function(solinst.file, programmed.h2o.density=1000) {
  if(programmed.h2o.density < 900 || programmed.h2o.density > 1100)
    stop('H2O density should be between 950 and 1050 kg/m^3')
  
  # Only use files directly from Solinst's software. Nothing that has been formatted.
  clean.solinst <- function(solinst.file) {
    # Throw away header rows
    solinst.headers <- readLines(solinst.file, n = 25)
    header.count = suppressWarnings(min(grep('^Date,Time,.*', solinst.headers)) - 1)
    solinst.csv <- read.csv(solinst.file, stringsAsFactors = FALSE, skip = header.count)
    results <- solinst.csv %>%
      filter(LEVEL >= 0, LEVEL <= 20) %>%  # Exclude erroneous readings
      transmute(time = if(any(grepl('m', Time))) as.POSIXct(paste(Date, Time), format='%m/%d/%Y %I:%M:%S %p', tz = 'Etc/GMT+8') else as.POSIXct(paste(Date, Time), format='%Y/%m/%d %H:%M:%S', tz = 'Etc/GMT+8'),
                level = if(max(LEVEL) < 9.5) LEVEL + 9.5 else LEVEL,
                temperature = TEMPERATURE,
                conductivity = if(any(names(solinst.csv) == 'CONDUCTIVITY')) CONDUCTIVITY else NA)
    
    rows.excluded <- sum((solinst.csv$LEVEL < 0) | (solinst.csv$LEVEL > 20), na.rm = TRUE)
    if(rows.excluded > 0)
      warning(sprintf('%i rows excluded for levels outside of [0, 20]', rows.excluded))
    if(any(names(solinst.csv) == 'CONDUCTIVITY') && any(solinst.csv$CONDUCTIVITY > 100))
      warning('Specific conductance values greater than 100, suggesting logger error.')
    
    results
  }
  
  # If programmed H2O density is unspecified it defaults to 1000 kg/m^3
  unit.conversion <- function(solinst.data, programmed.h2o.density) {
    # convert m of H2O to kPa and specific conductivity to salinity
    solinst.data %>%
      mutate(total.pressure = level * programmed.h2o.density * 9.80665 / 1000,
             salinity = 0.012+(-0.2174*((conductivity)/53.087)^0.5)+(25.3283*((conductivity)/53.087)^1)+(13.7714*((conductivity)/53.087)^1.5)+(-6.4788*((conductivity)/53.087)^2)+(2.5842*((conductivity)/53.087)^2.5)) %>%
      select(-level, -conductivity)
  }

  solinst.data <- clean.solinst(solinst.file)
  unit.conversion(solinst.data, programmed.h2o.density)
}

#' Standardize Hobo Levelogger Data
#'
#' This function cleans water level data exported from the Hoboware software
#' @param hobo.file the path to your solinst csv file
#' @param logger.id the serial number of your logger, necessary for temperature compensation
#' @param temperature data frame of an external source of temperature data,
#' if the logger was launched without temperature. Must have fields 'time' in POSIX format
#' and 'temperature' in degrees C
#' @export
#' @examples standardize.hobo('Petaluma01_120815.csv')
#' standardize.hobo('Petaluma01_110412.csv', logger.id = 11201228, temperature = my.temps)

standardize.hobo <- function(hobo.file, logger.id = NULL, temperature = NULL) {
  # function to adjust pressure for temperature-less hobo data based on an
  # external temperature source and also logger-specific constants
  temperature.calibration <- function(logger.id, temperature, pressure.15) {
    # logger constants loaded with package
    
    # Merge pressure and temperature
    temperature.dt <- temperature %>%
      filter(!is.na(time),
             !is.na(temperature)) %>%
      data.table(key = 'time')
    pressure.15.dt <- pressure.15 %>%
      filter(!is.na(time),
             !is.na(total.pressure)) %>%
      select(-temperature) %>%
      data.table(key = 'time')
    temp.pres <- temperature.dt[pressure.15.dt, roll = 'nearest'] %>%
      as.data.frame()
    
    # Create compensation function
    temperature.compensation.FUN <- function(pressure.15, temperature) {
      dt <- temperature - 15
      this.logger.constants <- filter(logger.constants, logger == logger.id)
      if(nrow(logger.constants) == 0) stop(sprintf('Logger ID %i not found in logger constant table', logger.id))
      with(this.logger.constants, 
           cprime * dt^2 + (m * pressure.15 + bprime) * dt + pressure.15)
    }
    
    # Compensate pressure for temperature
    temp.pres %>%
      mutate(total.pressure = temperature.compensation.FUN(total.pressure, temperature),
             temperature = NA) %>%
      select(time, total.pressure, temperature, salinity)
  }
  
  # function to clean up hobo file
  clean.hobo <- function(hobo.file) {
    hobo.csv <- read.csv(hobo.file, stringsAsFactors = FALSE, skip = 1)
    
    # Standardize names of columns
    TimeIdx <- min(grep('^Date.Time', names(hobo.csv)))
    PressureIdx <- min(grep('^Abs.Pres', names(hobo.csv)))
    TemperatureIdx <- suppressWarnings(min(grep('^Temp', names(hobo.csv)))) # Will be Inf in files without temperature
    if(is.infinite(TemperatureIdx)) {
      hobo.csv$temperature <- NA
    } else {
      names(hobo.csv)[TemperatureIdx] <- 'temperature'
    }
    names(hobo.csv)[c(TimeIdx, PressureIdx)] <- c('time', 'total.pressure')
    
    results <- hobo.csv %>%
      filter(!is.na(total.pressure), total.pressure >= 0) %>%
      mutate(time = as.POSIXct(time, format = '%m/%d/%y %I:%M:%S %p', tz="Etc/GMT+8")) %>%
      select(time, total.pressure, temperature)
    results$salinity <- NA
    
    rows.excluded <- sum(hobo.csv$Pressure < 0, na.rm = TRUE)
    if(rows.excluded > 0) 
      warning(sprintf('%i rows excluded for pressures outside of [0, 200]', rows.excluded))
    
    results
  }
  
  
  hobo.data <- clean.hobo(hobo.file)
  if(all(is.na(hobo.data$temperature))) {
    hobo.data <- temperature.calibration(logger.id, temperature, hobo.data)
  }
  hobo.data
}

#' Standardize Solinst Barologger Data
#'
#' This function cleans barometric pressure data exported from the Levelogger software
#' @param solinst.file the path to your solinst csv file
#' @export
#' @examples standardize.baro('HumboldtBaro_120815.csv')

# Only use files directly from Solinst's software. Nothing that has been formatted.

standardize.baro <- function(solinst.file) {
  # Throw away header rows
  solinst.headers <- readLines(solinst.file, n = 25)
  header.count = suppressWarnings(min(grep('^Date,Time,.*', solinst.headers)) - 1)
  solinst.csv <- read.csv(solinst.file, stringsAsFactors = FALSE,
                          skip = header.count)
  results <- solinst.csv %>%
    transmute(time = if(any(grepl('m', Time))) as.POSIXct(paste(Date, Time), format='%m/%d/%Y %I:%M:%S %p', tz = 'Etc/GMT+8') else as.POSIXct(paste(Date, Time), format='%Y/%m/%d %H:%M:%S', tz = 'Etc/GMT+8'),
              baro.pressure = if(max(LEVEL) < 9.5) (LEVEL + 9.5)*9.80665 else if(max(LEVEL)<100) LEVEL*9.80665 else LEVEL,
              temperature = TEMPERATURE
    )
  
  
  results
}

#' Standardize Entire Folder of Solinst Barologger Data
#'
#' This function cleans a whole folder of barometric pressure data exported from the Levelogger software
#' @param baro.dir the path to the whole directory of solinst csv files
#' @export
#' @examples standardize.baro.folder('myproject/raw_baro_data')

standardize.baro.folder <- function(baro.dir){
  baro.files<-list.files(baro.dir, full.names = TRUE)
  baro.standardized <- lapply(baro.files, standardize.baro)
  
  # combine all files into one data frame
  baro.sorted <- bind_rows(baro.standardized) %>% arrange(time) %>% unique()
  # get time gap between readings
  baro.timediff <- diff(baro.sorted$time)
  # get starts and ends of gaps that are greater than 10800 seconds (3 hours)
  baro.gaps <- data.frame(start = baro.sorted[c(baro.timediff, 1)>lubridate::hours(3),]$time,
                          end =baro.sorted[c(1, baro.timediff)>lubridate::hours(3),]$time)
  if(nrow(baro.gaps)>0)
    warning('Baro data has gaps of over 3 hours. Logger data will be incorrectly compensated')
  print(list(start.time.baro = min(baro.sorted$time),
             end.time.baro = max(baro.sorted$time),
             gaps = baro.gaps))
  baro.sorted
}
