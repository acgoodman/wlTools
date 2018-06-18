# Written 2017-03-03 by Ari Goodman, modified from one of Max's scripts
# Download barometric pressure data from NOAA stations

# get.noaa.baro requires as input:
# 1. station.id = a numeric ID of a NOAA station with baro data e.g. 9432780
# 2. start.t = the start date of the requested baro data in POSIX format
# 3. end.t = the end date of the requested baro data in POSIX format

#' Collect Baro Data from NOAA meteorological station
#'
#' Collects barometric pressure data from NOAA
#' @param station.id a numeric station ID for the NOAA station
#' @param start.t the start time of the desired observations, in POSIXct format
#' @param end.t  the end time of the desired observations, in POSIXct format
#' @param interval the desired interval of observation, either '6' for six minutes or 'h' for hours
#' @export
#' @examples get,noaa.baro(time1, time2, 9415115)
get.noaa.baro <- function(station.id, start.t, end.t, interval = c('6', 'h')){
  
  # split time period up into months.
  date.intervals <- unique(c(seq(start.t, end.t + 31, by = 'month'), end.t))
  

  # connect to NOAA station and download for each month, combine the results
  noaa.baro <- foreach(d1 = head(date.intervals, -1),
                        d2 = tail(date.intervals, -1),
                        .combine = rbind) %do% {
                          
    # generate address for each month
    address <- sprintf('http://tidesandcurrents.noaa.gov/api/datagetter?product=air_pressure&application=NOS.COOPS.TAC.MET&begin_date=%s&end_date=%s&station=%s&time_zone=LST&units=metric&interval=%s&format=json',
                       strftime(d1, '%Y%m%d'), 
                       strftime(d2, '%Y%m%d'),
                       station.id,
                       interval)
    # connect to address
    conn <- url(address)
    
    # look at the top of the JSON file, check if it contains data
    noaa.list <- RJSONIO::fromJSON(readLines(conn, n=-1L, ok=TRUE))
    close(conn)
    
    # see what kind of data is provided: data and metadata or errors
    headings <- names(noaa.list)
    
    if('data' %in% headings){
      # turn JSON file into a data frame
      noaa.data <- data.frame(t(sapply(noaa.list$data, c))) %>%
        
        # format data and select fields
        mutate(DateTime = as.POSIXct(t, tz="Etc/GMT+8", format = "%Y-%m-%d %H:%M"), 
               v = as.numeric(levels(v)[v])/10) %>%
        select(time = DateTime, baro.pressure = v)
      noaa.data
    }
      
  } %>% unique()
}
