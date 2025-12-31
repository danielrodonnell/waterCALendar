#' Convert Date to Day of Year (DOY)
#'
#' \code{date_to_doy()} returns the DOY of the date entered.
#'
#' @examples
#' date_to_doy("2024-01-01")
#' date_to_doy(as.Date("2024-01-01"), verbose=T)
#' date_to_doy("1/1/2024")
#' date_to_doy(as.POSIXct("2024-01-01 12:45:30"), verbose=T)
#' date_to_doy(as.POSIXlt("2024-01-01 12:45:30"))
#'
#' @importFrom dplyr mutate tibble
#' @importFrom magrittr %>%
#' @param date A date or datetime of any format EXCEPT those containing words
#' like "January" or abbreviations like "Jan". Accepted classes are
#' \code{character}, \code{Date}, \code{POSIXct} and \code{POSIXlt}.
#' @param verbose Logical. Display explanatory messages with output? Default is
#' \code{FALSE}.
#' @order 0
#' @return Returns a \code{numeric} day of year (DOY).
#' @details Note that the DOY returned begins as 1 on January 1, whereas
#' \code{as.POSIXlt("yyyy-mm-dd")$yday} begins as 0 on January 1.
#' @seealso [doy_to_date()],
#' [doy_to_dowy()],
#' [dowy_to_doy()],
#' [date_to_dowy()],
#' [dowy_to_date()],
#' [date_to_mowy()],
#' [mowy_to_month()],
#' [month_to_mowy()],
#' [date_to_wy()],
#' [date_to_wyt()],
#' [date_to_Date()]
#' @export
date_to_doy <- function(date,verbose=FALSE){

  date <- date_to_Date(date)

  date <- as.Date(date, tz="America/Los_Angeles")

  date_table <- dplyr::tibble(date = as.Date(date)) %>%
    mutate(doy = as.POSIXlt(date)$yday+1)

  doy <- date_table$doy

  if(sum(doy==366,na.rm=T)>0 & verbose){
    message(paste0("At least one DOY 366 returned for 31 December of a ",
                   "leap year."))
  }
  doy
}

