#' Convert Day of Year (DOY) to Day of Water Year (DOWY)
#'
#' California water years begin on October 1. \code{doy_to_dowy()} returns the
#' day of the year (DOWY) if you enter a day of year.
#'
#' @examples
#' doy_to_dowy(150,"2024")
#' doy_to_dowy("150",2024,verbose=TRUE)
#' doy_to_dowy("150","2024")
#'
#' @importFrom lubridate leap_year
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @param doy A day of year, entered as a number from 1 to 365 (inclusive); 366 is accepted in leap years.
#' Accepted classes are \code{numeric}, \code{character} and \code{factor}.
#' @param year A four-digit year. Accepted classes are \code{numeric}, \code{character} and \code{factor}.
#' @param verbose Logical. Display reminder about years vs water years? Default is \code{FALSE}.
#' @order 0
#' @return Returns a \code{numeric} day of water year (DOWY), where water year begins on 01 October.
#' @details Note that the \code{doy} argument assumes that \code{doy} 1 is January 1,
#' whereas \code{as.POSIXlt("yyyy-mm-dd")$yday} begins as 0 on January 1.
#' @seealso [dowy_to_doy()],
#' [dowy_to_date()],
#' [date_to_dowy()],
#' [date_to_doy()],
#' [doy_to_date()],
#' [date_to_mowy()],
#' [mowy_to_month()],
#' [month_to_mowy()],
#' [date_to_wy()]
#' @export
doy_to_dowy <- function(doy,year,verbose=FALSE){

  year <- as.numeric(as.character(year))
  year <- ifelse(is.na(year),3987,year)
  doy <- as.numeric(as.character(doy))
  leap <- ifelse(year==3987,NA,lubridate::leap_year(paste0(year,"-01-01")))
  year <- ifelse(year==3987,NA,year)

  leap_table <- dplyr::tibble(year,doy) %>%
    mutate(leap = leap,
           err_366 = ifelse(year==3987|is.na(doy), NA,
                            ifelse(leap & !doy %in% 1:366, T, F)),
           err_365 = ifelse(year==3987|is.na(doy), NA,
                            ifelse(!leap & !doy %in% 1:365, T, F))) %>%
    suppressWarnings()

  if(sum(leap_table$err_365,na.rm=T)>0|sum(leap_table$err_366,na.rm=T)>0){
    warning("At least one NA returned due to nonexistent doy input.")
  }

  date_table <- suppressWarnings(leap_table %>%
    mutate(date = as.Date(paste0(year,"-01-01"))+doy-1,
           date = ifelse(err_365|err_366,NA,date),
           date = as.Date(date),
           dowy = ifelse(leap,
                         ifelse(doy>274, doy-274, doy+92),
                         ifelse(doy>273, doy-273, doy+92)),
           dowy = ifelse(err_365|err_366,NA,dowy)))

  if(verbose & sum(date_table$dowy < 93,na.rm=T)>0){
    message(paste0("Reminder: if date is in 01 October - 31 December, water ",
                   "year is year + 1."))
  }

  date_table$dowy
}

