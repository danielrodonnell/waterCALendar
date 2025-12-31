#' Convert Day of Year (DOY) to Date
#'
#' \code{doy_to_date()} returns the DOY of the date entered. \code{year} is required
#' to account for leap years.
#'
#' @examples
#' doy_to_date("10",2022)
#' doy_to_date(10,"2022")
#' doy_to_date("10","2022")
#'
#' @importFrom lubridate leap_year
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @param doy A day of year between 1 and 365 (inclusive); 366 accepted in leap years.
#' Accepted classes are \code{numeric}, \code{character} and \code{factor}.
#' @param year A four-digit year. Accepted classes are \code{numeric}, \code{character} and \code{factor}.
#' @order 0
#' @return Returns the date (as a \code{Date} object) of the \code{doy} entered.
#' @details Note that the \code{doy} argument assumes that \code{doy} 1 is January 1,
#' whereas \code{as.POSIXlt("yyyy-mm-dd")$yday} begins as 0 on January 1.
#' @seealso [date_to_doy()],
#' [doy_to_dowy()],
#' [dowy_to_doy()],
#' [date_to_dowy()],
#' [dowy_to_date()],
#' [date_to_mowy()],
#' [mowy_to_month()],
#' [month_to_mowy()],
#' [date_to_wy()]
#' @export
doy_to_date <- function(doy,year){

  year <- as.numeric(as.character(year))
  year <- ifelse(is.na(year),3987,year)
  doy <- as.numeric(as.character(doy))
  leap <- ifelse(year==3987,NA,lubridate::leap_year(paste0(year,"-01-01")))
  year <- ifelse(year==3987,NA,year)

  leap_table <- suppressWarnings(dplyr::tibble(year,doy) %>%
    mutate(leap = leap,
           err_366 = ifelse(year==3987|is.na(doy), NA,
                            ifelse(leap & !doy %in% 1:366, T, F)),
           err_365 = ifelse(year==3987|is.na(doy), NA,
                            ifelse(!leap & !doy %in% 1:365, T, F))))

  if(sum(leap_table$err_365,na.rm=T)>0|sum(leap_table$err_366,na.rm=T)>0){
    warning("At least one NA returned due to nonexistent doy input.")
  }

  date_table <- suppressWarnings(leap_table %>%
    mutate(date = ifelse(leap_table$err_365|leap_table$err_366,NA,
                         as.Date(paste0(year,"-01-01"))+doy-1),
           date = as.Date(date)))

  date_table$date
}


