#' Filter Odyssey Data Based on Water Level Data
#'
#' Filters odyssey data to select only observations taken at the same times as a water level logger reports being inundated
#' Outputs optional data frame and optionally saves file
#' If input csv is used, output save file retains base name. Otherwise, output file is named by timestamp
#' @param ody_csv path to csv file of cleaned odyssey data
#' @param waterlevel_csv path to csv file of water-level-above-sensor data
#' @param height.offset height difference between sensors or threshold water level at which to accept odyssey data
#' @param output.dir optional directory in which to save cleaned data
#' @param ody_df data frame of cleaned odyssey data if ody_csv not used
#' @param waterlevel_df data frame of cleaned water level data if waterlevel_csv not used
#' @param return.data boolean, whether or not to return data as data frame
#' @export
#' @examples filter_ody(ody_csv = 'MadOdy_2437.csv', waterlevel_csv = 'Mad01-A_BaroDensCompensated.csv', height.offset = 0.07, output.dir = 'Mad01-A')
filter_ody <- function(ody_csv, waterlevel_csv, height.offset, output.dir,
                       ody_df, waterlevel_df, return.data = FALSE) {
  # make sure there is a height offset
  if(missing(height.offset)) stop('Odyssey height offset required to filter water levels!')
  
  # warn about duplicate data inputs
  if(!missing(ody_csv) && !missing(ody_df)){
    warning('Odyssey data must be either csv or data frame, not both',
            call. = FALSE, immediate. = TRUE)
  }
  if(!missing(waterlevel_csv) && !missing(waterlevel_df)){
    warning('Water Level Above Sensor data must be either csv or data frame, not both',
            call. = FALSE, immediate. = TRUE)
  }

  # read in Odyssey data
  if(!missing(ody_csv)){
    ody.data <- read.csv(ody_csv) %>%
      mutate(date.time = as.POSIXct(date.time, tz = "Etc/GMT+8"))
  } else ody.data <- ody_df
  if(!('salinity' %in% names(ody.data)) &&
     !('salinity.calc' %in% names(ody.data))){
    stop('Odyssey data must contain the field "salinity or salinity.calc"')
  }

  if('calc.salinity' %in% names(ody.data)){
    ody.data <- rename(ody.data, salinity = salinity.calc)}
  # read in water level data
  if(!missing(waterlevel_csv)){
    wll.data <- read.csv(waterlevel_csv) %>%
      mutate(time = as.POSIXct(time, tz = "Etc/GMT+8"))
  } else wll.data <- waterlevel_df
  if(!('water.level.above.sensor' %in% names(wll.data))){
    stop('Water Level data must contain the field "water.level.above.sensor"')
  }
  # warn if odyssey data is not contained within wll data
  if(min(wll.data$time) > min(ody.data$date.time) ||
     max(wll.data$time) < max(ody.data$date.time)){
    warning(sprintf(paste0('URGENT: Time range for odyssey data (%s - %s)',
                           'is not fully contained within time range for water level data (%s - %s).',
                           ' Data will be trimmed. Use leve data from another time period to recover',
                           ' trimmed section.'),
                    format(min(ody.data$date.time), '%Y-%m-%d %H:%M'),
                    format(max(ody.data$date.time), '%Y-%m-%d %H:%M'),
                    format(min(wll.data$time), '%Y-%m-%d %H:%M'),
                    format(max(wll.data$time), '%Y-%m-%d %H:%M'), collapse =''),
            call. = FALSE, immediate. = TRUE)}
  # check height offset
  if(height.offset < 0.05){
    warning('Minimum threshold of 5cm will be used to filter water levels.',
            call. = FALSE, immediate. = TRUE)
  }
  # filter water data using the provided threshold or 5 cm, whichever is bigger
  filtered.water <- wll.data %>%
    filter(water.level.above.sensor > max(c(0.05, height.offset))) %>%
    transmute(date.time = time,
              water.level.above.wll = water.level.above.sensor)

  # join data frames to keep matching ody data
  filtered.ody <- inner_join(ody.data, filtered.water, by = 'date.time')
  print(sprintf('Filtering complete. %s observations removed from odyssey data',
        nrow(ody.data) - nrow(filtered.ody)))
  
  # save in output directory
  if(!missing(output.dir)){
    record.metadata(output.dir)
    if(!missing(ody_csv)){
      ody.title <- basename(tools::file_path_sans_ext(ody_csv))
    } else {
      ody.title <- strftime(Sys.time(), format="%Y-%m-%d-%H%M%S")
    }
    
    write.csv(filtered.ody,
              file.path(output.dir, sprintf('%s_%sm_filtered.csv', ody.title,
                                            max(c(0.05, height.offset)))))
  }
  if(return.data) return(filtered.ody)
}