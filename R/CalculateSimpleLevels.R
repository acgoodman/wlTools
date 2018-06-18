# script for metadata generation

record.metadata <- function(save.dir = getwd()){
  metadata <- function(){
    sprintf('script run by %s at on %s.
  User ran the following command:
  %s', Sys.info()['user'], Sys.time(),
            stringr::str_squish(paste(deparse(sys.call(1)), collapse="")))
  }
  
  write(metadata(),
        file=file.path(save.dir,sprintf('metadata %s.txt', Sys.Date())),
        append = TRUE)
}

#' Calculate compensated water levels above sensor using a pair of baro pressure and water pressure files
#'
#' This function performs barometric compensation and density compensation and save the files
#' @param wll_csv the path to a csv file exported directly from the logger software
#' @param baro_csv the path to a csv file exported directly from the baro logger software
#' @param wll.type the brand of water logger: either 'Hobo' or 'Solinst'
#' @param baro.type the brand of baro logger: either 'Hobo' or 'Solinst' or 'df' if using a data frame
#' @param output.dir the path to the output directory for the saved files
#' @param water.density the average water density for the site, in g/L
#' @param baro.df a data frame of baro pressure data and time if not using a CSV. Must have fields "time" and "baro.pressure"
#' @param sensor.elev an optional field of absolute sensor elevation
#' @param navd.dir the path in which to save logger data with NAVD elevations
#' @export
#' @examples calculate_levels_simple(wll_csv = wl.files[[1]],
#' baro_csv = 'my_baro.csv',
#' wll.type = 'Solinst',
#' baro.type = "Solinst",
#' output.dir = 'CSVs/4_BaroDensCompensated',
#' water.density = 1003,
#' sensor.elev = -0.02,
#' navd.dir = 'CSVs/5_compensated_levels_with_sensor_height'
#' )
#inputs: water level file, baro file
calculate_levels_simple <-function(wll_csv, baro_csv, wll.type, baro.type = solinst,
                                   output.dir, water.density, baro.df = NULL,
                                   sensor.elev, navd.dir){
  
  # compensate baro data, depending on type, catch errors
  if(baro.type == 'Solinst'){
    baro.data <- standardize.baro(solinst.file = baro_csv)
  } else if(baro.type == 'Hobo'){
    baro.data <- standardize.hobo(baro_csv) %>%
      select(time, baro.pressure = total.pressure)
  } else if(baro.type == 'df'){
    if(!('time' %in% names(baro.df) && 'baro.pressure' %in% names(baro.df))){
      stop('Baro data must have columns: "time", "baro.pressure"')
    }
    if(!any(class(baro.df$time) == 'POSIXct'))
      stop('Baro data time must be in POSIXct format')
    baro.data <- baro.df
  } else{
    stop(paste0('Stop! baro.type must be "Solinst", "Hobo," or "df".',
                '\n  "df" is for advanced users using pre-formatted data frames'))
  }
  
  # standardize water level data, catch errors
  if(wll.type == 'Solinst'){
    wl.data <- standardize.solinst(solinst.file = wll_csv)
  } else if(wll.type == 'Hobo'){
    wl.data <- standardize.hobo(wll_csv)
  } else{
    stop('Stop! wll.type must be "Solinst" or"Hobo".')
  }
  
  # trim water data to fit in baro data if necessary
  trim.range <- c(min(baro.data$time), # start and end times for baro
                  max(baro.data$time))
  if(min(wl.data$time) > trim.range[2] ||
     max(wl.data$time) < trim.range[1]){
    stop('Baro data and water level data do not overlap!')
  }
  if(min(wl.data$time) < trim.range[1] ||
     max(wl.data$time) > trim.range[2]){
    warning(sprintf(paste0('URGENT: Time range for water level data (%s - %s)',
    'is not fully contained within time range for baro data (%s - %s).',
    ' Data will be trimmed. Use baro data from another time period to recover',
    ' trimmed section.'),
    format(min(wl.data$time), '%Y-%m-%d %H:%M'),
    format(max(wl.data$time), '%Y-%m-%d %H:%M'),
    format(min(baro.data$time), '%Y-%m-%d %H:%M'),
    format(max(baro.data$time), '%Y-%m-%d %H:%M'), collapse =''))
  }
  wl.data <- filter(wl.data, # trim the data
                    time >= trim.range[1],
                    time <= trim.range[2])

  
  # generate water levels above sensor
  wlas <- tp.to.wlas(wl.data, baro.data, h2o.density = water.density)
  
  # generate plot of water levels above sensor for QA
  wlas.plot <- ggplot(wlas, aes(time, water.level.above.sensor))+
    geom_point(size = 0.5) + theme_bw() +
    labs(y = 'water level above sensor')
    
  
  # adjust for NAVD elevations (use with caution)
  if(!missing(sensor.elev) && !missing(navd.dir)){
    warning('WARNING: use NAVD adjustment with caution - double check all RTK values and calculations before proceeding')
    threshold <- function (x, y) ifelse(x > 0.5, x+y, NA)
    navd <- bind_rows(water.level) %>%
      mutate(sensor.elev = sensor.elev,
             navd.level = threshold(water.level.above.sensor, sensor.elev))
  }
  
  # record the metadata for this call
  record.metadata(output.dir)
  
  # save water levels above sensor to output directory
  wl.title <- basename(tools::file_path_sans_ext(wll_csv))
  write.csv(wlas, file.path(output.dir, sprintf('%s_BaroDensCompensated.csv',
                                                wl.title)),
            row.names = FALSE)
  ggsave(file.path(output.dir, sprintf('%s_wl_above.sensor.png',
                                        wl.title)),
         wlas.plot, width = 5, height = 4, units = 'in')
  if(!missing(sensor.elev) && !missing(navd.dir)){
    write.csv(navd, file.path(navd.dir,
                              sprintf('%s_CompensatedLevelsSensorHeight.csv',
                                      wl.title)),
              row.names = FALSE)
  }


}
