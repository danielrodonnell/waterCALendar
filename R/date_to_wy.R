#' Convert Date to Water Year
#'
#' California water years begin on October 1. \code{date_to_wy()} returns the
#' water year when you enter a date. In practice, this means that if the date is
#' before October 1, water year is just year, otherwise water year is year+1.
#'
#' @examples
#' date_to_wy("2024-11-01")
#' date_to_wy("1/1/2024")
#' date_to_wy(as.Date("2024-11-01"))
#' date_to_wy(as.POSIXct("2024-11-01 12:45:30"))
#'
#' @importFrom lubridate year month
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @param date A date or datetime of any format EXCEPT those containing words
#' like "January" or abbreviations like "Jan". Accepted classes are
#' \code{character}, \code{Date}, \code{POSIXct} and \code{POSIXlt}.
#' @param verbose Logical. Display reminder about years vs water years? Default
#' is \code{FALSE}.
#' @order 0
#' @return Returns \code{numeric} water year of the date entered.
#' @seealso [date_to_mowy()],
#' [mowy_to_month()],
#' [month_to_mowy()],
#' [date_to_dowy()],
#' [dowy_to_date()],
#' [doy_to_dowy()],
#' [dowy_to_doy()],
#' [doy_to_date()],
#' [date_to_doy()]
#' [date_to_wyt()],
#' [date_to_Date()]
#' @export
date_to_wy <- function(date,verbose=FALSE){

  date <- date_to_Date(date)

  date <- as.Date(date, tz="America/Los_Angeles")

  date_table <- dplyr::tibble(date = as.Date(date)) %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date),
           wy = ifelse(month>9,year+1,year))

  wy <- date_table$wy

  if(sum(date_table$month %in% 10:12,na.rm=T)>0 & verbose){
    message(paste0("Reminder: if date is in 01 October - 31 December, water ",
                   "year is year+1"))
  }
  wy
}


