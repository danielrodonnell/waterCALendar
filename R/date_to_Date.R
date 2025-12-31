#' Convert dates of most standard formats to class \code{Date} (base R)
#'
#' Dates and datetimes may be written in many formats and may be of many
#' different data classes in R. \code{date_to_Date()} converts dates and
#' datetimes of most standard formats into base R's \code{Date} class. Date and
#' datetime formats not accepted are any containing words like "January" or
#' abbreviations like "January". NOTE: \code{date_to_Date()} is run internally
#' by all exported waterCALendar functions that take \code{date} as an argument.
#'
#' @examples
#' date_to_Date("2024-11-01 12:00:00")
#' date_to_Date(as.POSIXct("2024-11-01"))
#' date_to_Date("11/01/2024 12:00:00")
#' date_to_Date("11/1/2024")
#'
#' @importFrom stringr str_squish
#' @importFrom lubridate mdy
#' @importFrom lubridate mdy_hm
#' @importFrom lubridate mdy_hms
#' @param date A date or datetime of any format EXCEPT those containing words
#' like "January" or abbreviations like "Jan". Accepted classes are
#' \code{character}, \code{Date}, \code{POSIXct} and \code{POSIXlt}.
#' @order 0
#' @return Returns entered date or datetime as date of class \code{Date}
#' (base R) with time zone "America/Los_Angeles".
#' @seealso [date_to_mowy()],
#' [mowy_to_month()],
#' [month_to_mowy()],
#' [date_to_dowy()],
#' [dowy_to_date()],
#' [doy_to_dowy()],
#' [dowy_to_doy()],
#' [doy_to_date()],
#' [date_to_doy()]
#' [date_to_wyt()]
#' @export
date_to_Date <- function(date){

  date <- str_squish(date)

  format_error <- paste0("All elements of date must be of the same format. ",
                         "If date is a column in a tibble or data.frame, ",
                         "Consider using:\nrowwise() %>%\ndate_to_Date(",
                         "date) %>%\nungroup()\nbefore applying other ",
                         "waterCALendar functions. If date is a vector,",
                         " use as.Date(sapply(date, date_to_Date)).")

  if(any(grepl("-",date))&any(!grepl("-",date))){
    stop(format_error)
  }
  if(any(grepl("/",date))&any(!grepl("/",date))){
    stop(format_error)
  }
  if(!length(unique(nchar(date))) == 1){
    stop(format_error)
  }

  if(grepl("/",date[1])){
    if(nchar(date[1])<11){
      date <- mdy(date,tz="America/Los_Angeles")
    }else if(nchar(date[1])>16){
      date <- mdy_hms(date,tz="America/Los_Angeles")
    }else{
      date <- mdy_hm(date,tz="America/Los_Angeles")
    }
  }else{
    date <- substr(date,1,10)
  }

  as.Date(date, tz="America/Los_Angeles")
}
