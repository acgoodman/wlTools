# a welcome message
# roxygen imports
#' @title side effects loaded with package
#' @title side effects loaded with package
#' @import dplyr
#' @importFrom data.table data.table key rbindlist
#' @import foreach
#' @import ggplot2
.onAttach <- function(libname, pkgname){
  packageStartupMessage(paste0('The wlTools package contains a set of tools',
    ' for managing and processing water level data. \n Remember to use this',
    ' kind "/" of slash not this kind "\\" in your file paths!'))
}

