#' Convert Day of Water Year (DOWY) to day of year (DOY)
#'
#' California water years begin on October 1. \code{dowy_to_doy()} returns the
#' DOY if you enter a DOWY and a year.
#'
#' @examples
#' dowy(1,"1999")
#' dowy("365",1999)
#' dowy("150","1999",verbose=T)
#'
#' @importFrom lubridate leap_year
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @param dowy A day of water year between 1 and 365 (inclusive)' 366 accepted in leap years.
#' Accepted classes are \code{numeric}, \code{character} and \code{factor}.
#' @param wyear A four-digit water year. Accepted classes are \code{numeric}, \code{character} and \code{factor}.
#' @param verbose Logical. Display reminder about years vs water years? Default is \code{FALSE}.
#' @order 0
#' @return Returns a \code{numeric} day of year (DOY). If \code{dowy} <= 92 (i.e., if the date is
#' October 1 - December 31), the DOY returned will be DOY of the year preceding the (\code{wyear}) entered.
#' @details If \code{dowy} <= 92, the DOY returned is DOY of the year preceding
#' the water year (\code{wyear}) entered. If \code{verbose=TRUE}, a reminder message
#' to this effect will be displayed.
#' @seealso [doy_to_dowy()],
#' [dowy_to_date()],
#' [date_to_dowy()],
#' [date_to_doy()],
#' [doy_to_date()],
#' [date_to_mowy()],
#' [mowy_to_month()],
#' [month_to_mowy()],
#' [date_to_wy()]
#' @export
dowy_to_doy <- function(dowy,wyear,verbose=FALSE){

  wyear <- as.numeric(as.character(wyear))
  wyear <- ifelse(is.na(wyear),3987,wyear)
  dowy <- as.numeric(as.character(dowy))
  leap <- ifelse(wyear==3987,NA,lubridate::leap_year(paste0(wyear,"-01-01")))
  wyear <- ifelse(wyear==3987,NA,wyear)

  leap_table <- suppressWarnings(dplyr::tibble(wyear,dowy) %>%
    mutate(leap = leap,
           err_366 = ifelse(wyear==3987|is.na(dowy), NA,
                            ifelse(leap & !dowy %in% 1:366, T, F)),
           err_365 = ifelse(wyear==3987|is.na(dowy), NA,
                            ifelse(!leap & !dowy %in% 1:365, T, F))))

  if(sum(leap_table$err_365,na.rm=T)>0|sum(leap_table$err_366,na.rm=T)>0){
    warning("At least one NA returned due to nonexistent dowy input.")
  }

  date_table <- suppressWarnings(leap_table %>%
    mutate(date = as.Date(paste0(wyear-1,"-09-30")) + dowy,
           year = lubridate::year(date),
           doy = ifelse(err_365|err_366,NA,as.POSIXlt(date)$yday+1)))

  if(verbose & sum(as.numeric(as.character(na.omit(dowy))) < 93,na.rm=T)>0){
    message(paste0("Reminder: if date is in 01 October - 31 December, year ",
                   "is wyear - 1."))
  }
  return(date_table$doy)
}

