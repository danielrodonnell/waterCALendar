#' Convert Date to Month of Water Year (MOWY)
#'
#' California water years begin on October 1. [date_to_mowy()] returns the month
#' of water year (MOWY). For example, if the date is in December, the MOWY is 3.
#'
#' @examples
#' date_to_mowy("2024-01-01",verbose=T)
#' date_to_mowy(as.Date("2024-01-01"))
#' date_to_mowy("1/1/2024")
#' date_to_mowy(as.POSIXct("2024-01-01))
#' date_to_mowy(as.POSIXlt("2024-01-01))
#'
#' @importFrom lubridate month
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @param date A date or datetime of any format EXCEPT those containing words
#' like "January" or abbreviations like "Jan". Accepted classes are
#' \code{character}, \code{Date}, \code{POSIXct} and \code{POSIXlt}.
#' @order 0
#' @return Returns a \code{numeric} month of water year (MOWY) between 1 and 12
#' (inclusive).
#' @seealso [month_to_mowy()],
#' [mowy_to_month()],
#' [date_to_dowy()],
#' [dowy_to_date()],
#' [doy_to_dowy()],
#' [dowy_to_doy()],
#' [doy_to_date()],
#' [date_to_doy()],
#' [date_to_wy()],
#' [date_to_wyt()],
#' [date_to_Date()]
#' @export
date_to_mowy <- function(date){

  date <- date_to_Date(date)

  date <- as.Date(date, tz="America/Los_Angeles")

  date_table <- tibble(date = as.Date(date)) %>%
    mutate(month = month(date),
           mowy = ifelse(month>9,month-9,month+3))

  date_table$mowy
}
