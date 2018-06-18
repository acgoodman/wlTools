library(dplyr)
library(foreach)

# 'by' must be 'overall' or 'year'
get.low.tidal.height.offset <- function(wll.data.path, noaa.station.id, by = 'overall') {
  if(!(by %in% c('overall', 'year'))) stop("'by' must be 'overall' or 'year'")
     
  # Read WLL data
  WLL.data.csv <- read.csv(wll.data.path)
  WLL.data <- mutate(WLL.data.csv, DateTime = as.POSIXct(DateTime, tz="Etc/GMT+8", format = "%m/%d/%Y %H:%M")) %>%
    select(DateTime, Level)
  
  # Calculate date windows
  date.start <- as.Date(min(WLL.data$DateTime, na.rm = TRUE))
  date.end <- as.Date(max(WLL.data$DateTime, na.rm = TRUE))
  date.intervals <- seq(date.start, date.end + 31, by = 'month')
  
  noaa.low.water <- foreach(d1 = head(date.intervals, -1), d2 = tail(date.intervals, -1), .combine = rbind) %do% {
    address <- sprintf('http://tidesandcurrents.noaa.gov/api/datagetter?product=high_low&application=NOS.COOPS.TAC.WL&station=%s&begin_date=%s&end_date=%s&datum=NAVD&units=metric&time_zone=LST&format=json',
                       noaa.station.id, 
                       strftime(d1, '%Y%m%d'), 
                       strftime(d2, '%Y%m%d'))
    conn <- url(address)
    noaa.data <- data.frame(t(sapply(RJSONIO::fromJSON(paste(readLines(conn, n=-1L, ok=TRUE), collapse=""))$data, c))) %>%
      filter(ty %in% c('L ', 'LL')) %>%
      mutate(DateTime = as.POSIXct(t, tz="Etc/GMT+8", format = "%Y-%m-%d %H:%M"), 
             v = as.numeric(levels(v)[v]),
             ty = sub('(L{1,2}) ?', 'M\\1W', ty)) %>%
      select(tide.type = ty, noaa.date = DateTime, noaa.value = v)
    close(conn)
    noaa.data
  }
  
  # Calculate search bounds for low/lower water in WLL data (plus/minus two hours)
  lower.bounds <- noaa.low.water$noaa.date - 4 * 60 * 60
  upper.bounds <- noaa.low.water$noaa.date + 4 * 60 * 60
  
  # Find WLL low water
  wll.low.water <- noaa.low.water
  wll.low.water$wll.value <- foreach(lb = lower.bounds, ub = upper.bounds, .combine = 'c') %do% 
    min(filter(WLL.data, DateTime > lb & DateTime < ub)$Level)
  

  # Run comparison
  LW.diffs <- wll.low.water %>%
    filter(!is.infinite(wll.value)) %>%
    mutate(diff = wll.value - noaa.value)

  if(by == 'overall') {
    results <- LW.diffs %>%
      select(tide.type, diff) %>%
      group_by(tide.type) %>%
      dplyr::summarize(offset = mean(diff, na.rm=TRUE))
    results[results$tide.type=='MLW', 2] <- mean(LW.diffs$diff, na.rm=TRUE)
    return(as.data.frame(results))
  }
  

  else {
    LW.diffs <- mutate(LW.diffs, year = as.POSIXlt(noaa.date)$year + 1900)
    MLW.offset <- LW.diffs %>% 
      select(year, diff) %>% 
      group_by(year) %>% 
      dplyr::summarize(offset = mean(diff)) %>% 
      mutate(tide.type = 'MLW')
    MLLW.offset <- LW.diffs %>% 
      filter(tide.type == 'MLLW') %>% 
      select(year, diff) %>% 
      group_by(year) %>% 
      dplyr::summarize(offset = mean(diff)) %>% 
      mutate(tide.type = 'MLLW')
    return(rbind(MLW.offset, MLLW.offset))
  }
}

# 'by' must be 'overall' or 'year'
get.corrected.noaa.lw.datums <- function(station.id, tidal.offsets, by = 'overall') {
  if(!(by %in% c('overall', 'year'))) stop("'by' must be 'overall' or 'year'")
  
  # NOAA tidal datums. This should be made into something cleaner in the future.
  address <- sprintf('http://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20150101&end_date=20150101&station=%07d&product=datums&units=metric&time_zone=lst&application=web_services&format=json',
                     station.id)
  conn <- url(address)
  raw.JSON <- RJSONIO::fromJSON(paste(readLines(conn), collapse = ''))
  close(conn)
  
  raw.noaa.tidal.datums <- foreach(datum = raw.JSON$datums, .combine = 'rbind') %do% {
    data.frame(tide.type = datum['n'], level = datum['v'], stringsAsFactors = FALSE)
  }
  raw.noaa.tidal.datums$level <- as.numeric(raw.noaa.tidal.datums$level)
  noaa.tidal.datums <- filter(raw.noaa.tidal.datums, tide.type %in% c('MLLW', 'MLW'))
  noaa.tidal.datums$level <- noaa.tidal.datums$level - filter(raw.noaa.tidal.datums, tide.type == 'NAVD')$level

  # Add offset
  datums <- merge(tidal.offsets, filter(noaa.tidal.datums, station.id == station.id)) %>%
    mutate(corrected.level = level + offset)
  
  if(by == 'overall')
    return(select(datums, tide.type, level = corrected.level))
  else
    return(select(datums, tide.type, year, level = corrected.level))
}

#' Get local low water datums
#'
#' Calculates low and lower high water datums from a short term tide station using a NOAA reference station
#' @param time.series a csv file that contains the fields DateTime representing DateTime in excel format
#' and Level representing NAVD water levels in meters
#' @param station the numeric ID of the NOAA reference station
#' @param by whether to calculate the datums overall or by year. It is almost always most appropriate to calculate it by 'overall'.
#' @param out.dir an optional output directory in which to save results
#' @export
#' @examples get.local.lw.datums('BrownsIsland01.csv', 9415144)
get.local.lw.datums <- function(time.series, station, out.dir, by = 'overall'){
  offset <- get.low.tidal.height.offset(time.series, station, by)
  datums <-get.corrected.noaa.lw.datums(station, offset, by)
  if(missing(out.dir)){
    return(datums)
  } else{
    record.metadata(out.dir)
    capture.output(datums,
                   file = file.path(out.dir,
                                    sprintf('Low_datums %s.txt', Sys.Date())))
    return(datums)
  }
}
