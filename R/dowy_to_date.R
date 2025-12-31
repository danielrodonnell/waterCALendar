#' Convert Day of Water Year (DOWY) to Date
#'
#' California water years begin on October 1. \code{dowy_to_date()} returns the
#' date of the \code{dowy} entered. Water year (\code{wyear}) is required, to
#' account for leap years.
#'
#' @examples
#' dowy_to_date(45,"1999")
#' dowy_to_date("45",1999)
#' dowy_to_date("45","1999")
#'
#' @importFrom lubridate leap_year
#' @importFrom dplyr mutate tibble
#' @importFrom magrittr %>%
#' @param dowy A day of water year from 1 to 365; 366 is accepted in leap years.
#' Accepted classes are \code{numeric}, \code{character} and \code{factor}.
#' @param wyear A four-digit water year. Accepted classes are \code{numeric}, \code{character} and \code{factor}.
#' @order 0
#' @return Returns the date of the \code{dowy} entered as a \code{Date} object.
#' @seealso [date_to_dowy()],
#' [doy_to_dowy()],
#' [dowy_to_doy()],
#' [doy_to_date()],
#' [date_to_doy()],
#' [date_to_mowy()],
#' [mowy_to_month()],
#' [month_to_mowy()],
#' [date_to_wy()]
#' @export
dowy_to_date <- function(dowy,wyear){

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
    mutate(date = ifelse(leap_table$err_365|leap_table$err_366,NA,
                         as.Date(paste0(wyear-1,"-09-30")) + dowy),
           date = as.Date(date)))

  date_table$date
}

