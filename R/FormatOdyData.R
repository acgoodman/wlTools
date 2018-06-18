#' Calculate salinity from SpCond
#'
#' generates salinity vector from specific conductance vector
#' @param spcond specific conductance
#' @export
#' @examples CalcSalinity(50)
# function to calculate salinity from SpCond
CalcSalinity <- function(spcond){
  sal <- 0.012+(-0.2174*((spcond/53.087)^0.5))+
    (25.3283*(spcond/53.087))+(13.7714*((spcond/53.087)^1.5))+
    (-6.4788*((spcond/53.087)^2))+(2.5842*((spcond/53.087)^2.5))
  return(sal)
  
}


#' Clean Odyssey CSVs
#'
#' cleans odyssey CSVs, outputs data frame with the following fields:
#' 'serial.number', 'month', 'date', 'time', 'date.time', 'temperature.calibrated', 'spcond.calibrated'
#' @param odyssey.file the path to an odyssey file directly exported from the Odyssey software
#' @export
#' @examples clean.odyssey('PetOdy_2438.CSV')

clean.odyssey <- function(odyssey.file){
  
  #read the top of the file to figure out where the data starts
  odyssey.header <- readLines(odyssey.file, n=15)
  skipn = grep('RAW VALUE | Raw Value', odyssey.header) + 1
  
  #get serial number from file
  logger.id <- grep('Logger S', odyssey.header, value = TRUE)
  sn <- stringr::str_extract(logger.id, '\\d{4}')
  
  #read in file
  colnames <- c('scan','date','time','temperature.raw','temperature.calibrated',
                'spcond.raw','spcond.calibrated')
  odyssey.csv<-read.csv(odyssey.file, col.names=colnames, header=FALSE,
                        strip.white=TRUE, skip=skipn)
  
  #format and select columns
  results <- odyssey.csv %>%
    mutate(
      date.time = as.POSIXct(paste(date,time), format='%d/%m/%Y %H:%M'),
      date = as.POSIXct(date, format='%d/%m/%Y'),
      month = strftime(date, format='%Y-%m'),
      serial.number = sn,
      salinity = CalcSalinity(spcond.calibrated))%>%
    select(c(serial.number, month, date,time,date.time,temperature.calibrated,
             spcond.calibrated, salinity))
  results
}


#' Clean Entire Directory of Odyssey CSVs
#'
#' runs clean.odyssey function on entire directory of raw odyssey files.
#' Make sure there are no other CSV files in directory.
#' Outputs list of data frames
#' @param directory directory in which data are stored
#' @export
#' @examples clean.ody.directory('~/my_odyssey_files/raw')


clean.ody.directory <-function(directory){
  raw.files<-list.files(path=directory, full.names=TRUE, pattern='.CSV')
  lapply(raw.files,clean.odyssey)
} 

#' Clean Irregular Odyssey CSVs v.1
#'
#' cleans irregular odyssey CSVs with 2 header rows, finds serial numbers from file name
#'outputs data frame with the following fields:
#' 'serial.number', 'month', 'date', 'time', 'date.time', 'temperature.calibrated', 'spcond.calibrated'
#' @param odyssey.file the path to an odyssey file directly exported from the Odyssey software
#' @export
#' @examples clean.irregular.odyssey2('PetOdy_2438.CSV')
clean.irregular.odyssey <- function(odyssey.file){
  
  # get sn from file name
  sn <- stringr::str_extract(basename(odyssey.file),'_\\d{4}_') %>%
    substr(2,5)

  # read in file
  colnames <- c('scan','date','time','temperature.raw','temperature.calibrated',
                'spcond.raw','spcond.calibrated')
  lines <- readLines(odyssey.file)
  # remove spaces around commas and turn single spaces into commas
  lines.clean <- stringr::str_replace_all(lines, ' *, *| ', ',')
  odyssey.csv<-read.csv(text = lines.clean, col.names=colnames, header=FALSE,
                        strip.white=TRUE, skip=2)
  
  # format and select columns
  results <- odyssey.csv %>%
    na.omit() %>%
    mutate(
      date.time = as.POSIXct(paste(date,time), format='%d/%m/%Y %H:%M'),
      date = as.POSIXct(date, format='%d/%m/%Y'),
      month = strftime(date, format='%Y-%m'),
      serial.number = sn,
      salinity = CalcSalinity(spcond.calibrated))%>%
    select(c(serial.number, month, date,time,date.time,temperature.calibrated,
             spcond.calibrated, salinity))
  results
}


#' Clean Irregular Odyssey CSVs v.2
#'
#' cleans irregular odyssey CSVs with variable header lengths, requires a 'raw value' line
#'outputs data frame with the following fields:
#' 'serial.number', 'month', 'date', 'time', 'date.time', 'temperature.calibrated', 'spcond.calibrated'
#' @param odyssey.file the path to an odyssey file directly exported from the Odyssey software
#' @export
#' @examples clean.irregular.odyssey2('PetOdy_2438.CSV')
clean.irregular.odyssey2 <- function(odyssey.file){
  
  # get sn from file name
  sn <- stringr::str_extract(basename(odyssey.file),'_[0-9]{4}_') %>%
    substr(2,5)
  
  #read the top of the file to figure out where the data starts
  odyssey.top <- readLines(odyssey.file, n=15)
  skipn = grep('RAW VALUE | Raw Value', odyssey.top) + 1
  
  # read in file
  colnames <- c('scan','date','time','temperature.raw','temperature.calibrated',
                'spcond.raw','spcond.calibrated')
  lines <- readLines(odyssey.file)
  # remove spaces around commas and turn single spaces into commas
  lines.clean <- stringr::str_replace_all(lines, ' *, *| ', ',')
  odyssey.csv<-read.csv(text = lines.clean, col.names=colnames, header=FALSE,
                        strip.white=TRUE, skip=skipn)
  
  # format and select columns
  results <- odyssey.csv %>%
    na.omit() %>%
    mutate(
      date.time = as.POSIXct(paste(date,time), format='%d/%m/%Y %H:%M'),
      date = as.POSIXct(date, format='%d/%m/%Y'),
      month = strftime(date, format='%Y-%m'),
      serial.number = sn ,
      salinity = CalcSalinity(spcond.calibrated))%>%
    select(c(serial.number, month, date,time,date.time,temperature.calibrated,
             spcond.calibrated, salinity))
  results
}