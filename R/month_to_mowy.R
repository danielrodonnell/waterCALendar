#' Convert Month to Month of Water Year (MOWY)
#'
#' California water years begin on October 1. \code{month_to_mowy()} returns the
#' month of water year (MOWY) if you enter a month.
#'
#' @examples
#' month_to_mowy(5)
#' month_to_mowy("Mar")
#' month_to_mowy("December")
#'
#' @importFrom lubridate leap_year month
#' @importFrom readr parse_number
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @param month A month, entered as a number from 1 to 12 (inclusive), a three-letter month abbreviation,
#' (e.g. "Jan") or a full month name (e.g. "January"). Accepted classes are \code{numeric}, \code{character} and \code{factor}.
#' @order 0
#' @return Returns a \code{numeric} month of water year (MOWY).
#' @seealso [mowy_to_month()],
#' [date_to_mowy()],
#' [date_to_dowy()],
#' [dowy_to_date()],
#' [doy_to_dowy()],
#' [dowy_to_doy()],
#' [doy_to_date()],
#' [date_to_doy()],
#' [date_to_wy()]
#' @export
month_to_mowy <- function(month){

  month_table <- dplyr::tibble(month) %>%
    mutate(month = as.character(month),
           month = ifelse(nchar(as.character(month))>2 &
                            suppressWarnings(
                              is.na(as.numeric(as.character(month)))
                              ), paste0(toupper(substr(month,1,1)),
                                        tolower(substr(month,2,10))),month),
           month = ifelse(!(suppressWarnings(
             as.numeric(as.character(month)) %in% 1:12
             )|month %in% lubridate::month(1:12,label=T)|
               month %in% lubridate::month(1:12,label=T,abbr=F)),
             NA, month))

  if(sum(is.na(month_table$month))>0){
    warning(paste0("NAs introduced by coercion. month argument must be a ",
                   "number from 1 to 12, a three-letter month abbreviation ",
                   "or the full name of\n  a month. Accepted classes are ",
                   "numeric, character and factor. Check input vector for ",
                   "malformed months."))
  }

  month_table2 <- month_table %>%
    mutate(Mmm = ifelse(nchar(month)==3,T,F),
           long = ifelse(nchar(month)>3,T,F),
           num = ifelse(!is.na(suppressWarnings(
             readr::parse_number(as.character(month)))),T,F),
           month = ifelse(long|Mmm, paste0(toupper(substr(month,1,1)),
                                           tolower(substr(month,2,3))),month),
           month = ifelse(long|Mmm,
                          which(lubridate::month(1:12,label=T)==month),
                          month),
           month = as.numeric(as.character(month)),
           mowy = ifelse(month>9,month-9,month+3))

  month_table2$mowy
}
