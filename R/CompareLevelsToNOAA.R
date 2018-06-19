# CONVENIENCE FUNCTIONS ETC----------------------------------------------------

# Lil class definition and function to read Excel dates as POSIXct
setClass('ExcelDateTime')
setAs('character','ExcelDateTime',
      function(from) as.POSIXct(from, format='%m/%d/%Y %H:%M', tz= "Etc/GMT+8"))


# FUNCTION TO GET NOAA DATA ---------------------------------------------------
get.extreme.level.in.range <- function(df, ub, lb){
  # df needs to have numeric field called 'Level' and POSIX field called DateTime
  tideframe <- filter(df, DateTime > lb & DateTime < ub)
  if(nrow(na.omit(tideframe)) == 0){
    return(data.frame('DateTime' = ub - lubridate::hours(4),
                      'Level' = NA, 'start.window' = lb))
  } else {
    return(cbind(slice(tideframe, which.max(Level)), 'start.window' = lb))
  }
}

# WLL.data must have POSIXct field called DateTime
get.tides <- function(WLL.data, noaa.station.id) {
  
  # Calculate date windows
  date.start <- as.Date(min(WLL.data$DateTime, na.rm = TRUE))
  date.end <- as.Date(max(WLL.data$DateTime, na.rm = TRUE))
  date.intervals <- seq(date.start, date.end + 31, by = 'month')
  noaa.tides <- foreach(d1 = head(date.intervals, -1), d2 = tail(date.intervals, -1), .combine = rbind) %do% {
    address <- sprintf('http://tidesandcurrents.noaa.gov/api/datagetter?product=high_low&application=NOS.COOPS.TAC.WL&station=%s&begin_date=%s&end_date=%s&datum=NAVD&units=metric&time_zone=LST&format=json',
                       noaa.station.id, 
                       strftime(d1, '%Y%m%d'), 
                       strftime(d2, '%Y%m%d'))
    message(sprintf('fetching observed tides for %s ... ', strftime(d1,'%m-%Y')),
            appendLF = FALSE)
    
    conn <- url(address)
    noaa.data <- data.frame(t(sapply(RJSONIO::fromJSON(paste(readLines(conn, n=-1L, ok=TRUE), collapse=""))$data, c)))
    close(conn)
    if(length(noaa.data)>0){
      noaa.data <- noaa.data %>%
        mutate(DateTime = as.POSIXct(t, tz="Etc/GMT+8", format = "%Y-%m-%d %H:%M"), 
               v = as.numeric(levels(v)[v])) %>%
        select(tide.type = ty, noaa.date = DateTime, noaa.value = v)
      noaa.data
    }
  }
  noaa.high.water <- filter(noaa.tides, tide.type %in% c('H ', 'HH'))
  
  # Calculate search bounds for high/low water in WLL data (plus/minus four hours)
  lower.hw.bounds <- noaa.high.water$noaa.date - lubridate::hours(4)
  upper.hw.bounds <- noaa.high.water$noaa.date + lubridate::hours(4)
  
  # Find WLL high water. Maybe these could be separate functions for simplicity
  wll.high.water <- foreach(lb = lower.hw.bounds, ub = upper.hw.bounds,
                            .combine = 'rbind') %do% 
    get.extreme.level.in.range(WLL.data, ub, lb)
  high.water <- cbind(wll.high.water, noaa.high.water)
  
  return(high.water)
  
}
# RETRIEVE AND COMPARE PUBLIC DATA --------------------------------------------

# generate a short list of comparison data and statistics
# this function is slow because it pulls a lot of data from NOAA
# and then does a lot of inefficient averaging
calculate.NOAA <- function(data.path, station.id){
  # read in data frame
  my.levels<-read.csv(data.path,colClasses = c('ExcelDateTime', 'numeric'))
  
  # get start and end observations
  endpoints <- c(start.time = min(na.omit(my.levels$DateTime)),
                end.time   = max(na.omit(my.levels$DateTime))) 

  # get and format differences between NOAA observed and USGS predicted high water
  my.tides <- get.tides(my.levels, station.id)
  my.observed <- my.tides %>%
    mutate(high.diff = Level - noaa.value,
           time.delay = difftime(DateTime, noaa.date, units='mins'))
  #return summary list
  my.observed
  
}

# PICK TIME PERIODS, RUN OTHER FUNCTIONS --------------------------------------

#' Compares high tides to NOAA station tides
#'
#' Compares high tides in water level time series to corresponding NOAA high tides
#' Input data source must have fields "DateTime" and "Level"
#' Outputs and prints a ggplot plot with a blue line representing mean difference
#' @param data.path path to CSV file of input data
#' @param station.id numeric code for NOAA station
#' @export
#' @examples myplot <- compare.NOAA.data('MadRiverLevels.csv', 9418767)

compare.to.NOAA <- function(data.path, station.id){

  # compare usgs data with with noaa data
  full.ts <- calculate.NOAA(data.path, station.id)

  # put some data in a graph
  plot.all.obs <- ggplot(full.ts, aes(DateTime, high.diff)) +
    xlab('Date')+
    geom_smooth(method='gam', se = FALSE, size = 0.3)+
    geom_point(size = 0.8)+
    ylab('local observed - NOAA observed')
  print(plot.all.obs)
  return(plot.all.obs)
}


