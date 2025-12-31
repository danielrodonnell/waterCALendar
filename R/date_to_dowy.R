#' Convert Date to Day of Water Year (DOWY)
#'
#' California water years begin on October 1. \code{date_to_dowy()} returns the
#' day of water year when you enter a date.
#'
#' @examples
#' date_to_dowy("2024-01-01",verbose=TRUE)
#' date_to_dowy(as.Date("2024-01-01"))
#' date_to_dowy(as.POSIXct("2024-01-01"))
#' date_to_dowy("1/1/2024 12:00")
#' date_to_dowy(as.POSIXlt("2024-01-01"),verbose=TRUE)
#'
#' @importFrom lubridate leap_year month
#' @importFrom dplyr mutate tibble
#' @importFrom magrittr %>%
#' @param date A date or datetime of any format EXCEPT those containing words
#' like "January" or abbreviations like "Jan". Accepted classes are
#' \code{character}, \code{Date}, \code{POSIXct} and \code{POSIXlt}.
#' @param verbose Logical. Display reminder about years vs water years?
#' Default is \code{FALSE}.
#' @order 0
#' @return Returns a \code{numeric} day of water year (DOWY).
#' @details Note that if the date entered falls between October 1 and
#' December 31 (inclusive), the DOWY
#' returned will be DOWY of the year following that of the date entered.
#' If \code{verbose=TRUE}, a
#' reminder message to this effect will be displayed.
#' @seealso [dowy_to_date()],
#' [dowy_to_doy()],
#' [doy_to_dowy()],
#' [doy_to_date()],
#' [date_to_doy()],
#' [date_to_mowy()],
#' [month_to_mowy()],
#' [mowy_to_month()],
#' [date_to_wy()],
#' [date_to_wyt()],
#' [date_to_Date()]
#' @export
date_to_dowy <- function(date,verbose=FALSE){

  date <- date_to_Date(date)

  date <- as.Date(date, tz="America/Los_Angeles")

  if(sum(lubridate::month(as.Date(date)) %in% 10:12, na.rm=T)>0 & verbose){
    message(paste0("Reminder: if date is in October 1 - December 31, water ",
                   "year is year+1."))
  }

  date_table <- dplyr::tibble(date = as.Date(date)) %>%
    mutate(doy = as.POSIXlt(date)$yday+1,
           leap = ifelse(lubridate::leap_year(date),T,F),
           dowy = ifelse(leap,
                         ifelse(doy>274, doy-274, doy+92),
                         ifelse(doy>273, doy-273, doy+92)))

  date_table$dowy
}

