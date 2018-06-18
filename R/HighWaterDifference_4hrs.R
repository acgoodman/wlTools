# this function calculates the difference between observed local high water levels and
# observed NOAA high water levels given a time series of local water levels and a
# NOAA reference station.
# 'by' must be 'overall' or 'year'

get.tidal.height.offset <- function(wll.data.path, noaa.station.id, by = 'overall') {
  if(!(by %in% c('overall', 'year'))) stop("'by' must be 'overall' or 'year'")
     
  # Read WLL data
  WLL.data.csv <- read.csv(wll.data.path)
  WLL.data <- mutate(WLL.data.csv, DateTime = as.POSIXct(DateTime, tz="Etc/GMT+8", format = "%m/%d/%Y %H:%M")) %>%
    select(DateTime, Level)
  
  # Calculate date windows
  date.start <- as.Date(min(WLL.data$DateTime, na.rm = TRUE))
  date.end <- as.Date(max(WLL.data$DateTime, na.rm = TRUE))
  date.intervals <- seq(date.start, date.end + 31, by = 'month')

  # collect NOAA data
  noaa.high.water <- foreach(d1 = head(date.intervals, -1), d2 = tail(date.intervals, -1), .combine = rbind) %do% {
    address <- sprintf('http://tidesandcurrents.noaa.gov/api/datagetter?product=high_low&application=NOS.COOPS.TAC.WL&station=%s&begin_date=%s&end_date=%s&datum=NAVD&units=metric&time_zone=LST&format=json',
                       noaa.station.id, 
                       strftime(d1, '%Y%m%d'), 
                       strftime(d2, '%Y%m%d'))
    conn <- url(address)
    noaa.data <- data.frame(t(sapply(RJSONIO::fromJSON(paste(readLines(conn, n=-1L, ok=TRUE), collapse=""))$data, c))) %>%
      filter(ty %in% c('H ', 'HH')) %>%
      mutate(DateTime = as.POSIXct(t, tz="Etc/GMT+8", format = "%Y-%m-%d %H:%M"), 
             v = as.numeric(levels(v)[v]),
             ty = sub('(H{1,2}) ?', 'M\\1W', ty)) %>%
      select(tide.type = ty, noaa.date = DateTime, noaa.value = v)
    close(conn)
    noaa.data
  }
  
  # Calculate search bounds for high/higher water in WLL data (plus/minus two hours)
  lower.bounds <- noaa.high.water$noaa.date - 4 * 60 * 60
  upper.bounds <- noaa.high.water$noaa.date + 4 * 60 * 60
  
  # Find WLL high water
  wll.high.water <- noaa.high.water
  wll.high.water$wll.value <- foreach(lb = lower.bounds, ub = upper.bounds, .combine = 'c') %do% 
    max(filter(WLL.data, DateTime > lb & DateTime < ub)$Level)

  # Run comparison
  HW.diffs <- wll.high.water %>%
    filter(!is.infinite(wll.value)) %>%
    mutate(diff = wll.value - noaa.value)

  if(by == 'overall') {
    results <- HW.diffs %>%
      select(tide.type, diff) %>%
      group_by(tide.type) %>%
      dplyr::summarize(offset = mean(diff, na.rm=TRUE))
    results[results$tide.type=='MHW', 2] <- mean(HW.diffs$diff, na.rm=TRUE)
    return(as.data.frame(results))
  }
  

  else {
    HW.diffs <- mutate(HW.diffs, year = as.POSIXlt(noaa.date)$year + 1900)
    MHW.offset <- HW.diffs %>% 
      select(year, diff) %>% 
      group_by(year) %>% 
      dplyr::summarize(offset = mean(diff)) %>% 
      mutate(tide.type = 'MHW')
    MHHW.offset <- HW.diffs %>% 
      filter(tide.type == 'MHHW') %>% 
      select(year, diff) %>% 
      group_by(year) %>% 
      dplyr::summarize(offset = mean(diff)) %>% 
      mutate(tide.type = 'MHHW')
    return(rbind(MHW.offset, MHHW.offset))
  }
}

# this function calculated local datums by adding the tidal offsets
# to the published NOAA datums
# 'by' must be 'overall' or 'year'
get.corrected.noaa.datums <- function(station.id, tidal.offsets, by = 'overall') {
  if(!(by %in% c('overall', 'year'))) stop("'by' must be 'overall' or 'year'")
  
  # NOAA tidal datums. This should be made into something cleaner in the future.
  address <- sprintf('http://tidesandcurrents.noaa.gov/api/datagetter?station=%s&product=datums&units=metric&time_zone=lst&application=web_services&format=json',
                     station.id)
  conn <- url(address)
  raw.JSON <- RJSONIO::fromJSON(paste(readLines(conn), collapse = ''))
  close(conn)
  
  raw.noaa.tidal.datums <- foreach(datum = raw.JSON$datums, .combine = 'rbind') %do% {
    data.frame(tide.type = datum['n'], level = datum['v'], stringsAsFactors = FALSE)
  }
  raw.noaa.tidal.datums$level <- as.numeric(raw.noaa.tidal.datums$level)
  noaa.tidal.datums <- filter(raw.noaa.tidal.datums, tide.type %in% c('MHHW', 'MHW'))
  noaa.tidal.datums$level <- noaa.tidal.datums$level - filter(raw.noaa.tidal.datums, tide.type == 'NAVD')$level
  
  # Add offset
  datums <- merge(tidal.offsets, filter(noaa.tidal.datums, station.id == station.id)) %>%
    mutate(corrected.level = level + offset)
  
  if(by == 'overall')
    return(select(datums, tide.type, level = corrected.level))
  else
    return(select(datums, tide.type, year, level = corrected.level))
}

#' Get water level data stats
#'
#' calculate start and end dates, highest observed water levels, gaps, etc.
#' @param wll.data.path a csv file that contains the fields DateTime representing DateTime in excel format
#' and Level representing NAVD water levels in meters
#' @export
#' @examples wll.data.stats('Petaluma.csv')
wll.data.stats <- function(wll.data.path) {
  # dplyr::mutate doesn't play nice with POSIXlt objects, so we use a wrapper function.
  get.year <- function(posixct.date) as.POSIXlt(posixct.date)$year + 1900
  # Read WLL data
  WLL.data <- read.csv(wll.data.path, stringsAsFactors = FALSE) %>%
    mutate(DateTime = as.POSIXct(DateTime, tz="Etc/GMT+8", format = "%m/%d/%Y %H:%M"),
           Year = get.year(DateTime))
  gaps <- diff(WLL.data$DateTime)
  units(gaps) <- 'days'
  gaps <- c(NA, gaps)
  gapIdx <- which(gaps > 7)
  WLL.gaps <- WLL.data[c(gapIdx - 1, gapIdx),]

  with(WLL.data,
       list(first.obs = min(DateTime, na.rm = TRUE),
            last.obs = max(DateTime, na.rm = TRUE),
            HOWL = max(Level, na.rm = TRUE),
            annual.HOWL = WLL.data %>%
              group_by(Year) %>%
              dplyr::summarize(HOWL = max(Level, na.rm = TRUE)) %>%
              na.omit(),
            gaps = data.frame(gap.start = DateTime[gapIdx-1],
                              gap.end = DateTime[gapIdx],
                              gap.duration = gaps[gapIdx])))
}

#' Get local high water datums
#'
#' Calculates high and higher high water datums from a short term tide station using a NOAA reference station
#' @param time.series a csv file that contains the fields DateTime representing DateTime in excel format
#' and Level representing NAVD water levels in meters
#' @param station the numeric ID of the NOAA reference station
#' @param by whether to calculate the datums overall or by year. It is almost always most appropriate to calculate it by 'overall'.
#' @param out.dir an optional output directory in which to save results
#' @export
#' @examples get.local.hw.datums('Petaluma.csv', 9414290)

# easy function to get datums and stats in one step.
get.local.hw.datums <- function(time.series, station, out.dir, by = 'overall'){
  offset <- get.tidal.height.offset(time.series, station, by)
  local.datums <- get.corrected.noaa.datums(station, offset, by)
  ts.stats <- wll.data.stats(time.series)
  datums <- list(datums=local.datums, stats=ts.stats)
  if(missing(out.dir)){
    return(datums)
  } else{
    record.metadata(out.dir)
    capture.output(datums,
                   file = file.path(out.dir,
                                    sprintf('High_datums %s.txt', Sys.Date())))
    return(datums)
  }
}
