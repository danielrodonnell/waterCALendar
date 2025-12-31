#' Convert Month of Water Year (MOWY) to Month
#'
#' California water years begin on October 1. \code{mowy_to_month()} returns the
#' month of the year if you enter a month of water year (MOWY).
#'
#' @examples
#' mowy_to_month(5,form="Mmm")
#' mowy_to_month("5",form="long")
#' mowy_to_month("5")
#'
#' @importFrom lubridate leap_year month
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @param mowy A month of water year (MOWY) between 1 and 12 (inclusive).
#' Accepted classes are \code{numeric}, \code{character} and \code{factor}.
#' @param form One of "numeric", "Mmm" or "long". Default is "numeric".
#' @order 0
#' @return If form = "numeric", returns a \code{numeric} month of the year, 1 (January) to 12 (December).
#' If form = "Mmm" returns a three-letter month code (e.g. "Jan") as an ordered \code{factor}. If form = "long",
#' returns a full month name (e.g. "January") as an ordered \code{factor}.
#' @seealso [month_to_mowy()],
#' [date_to_mowy()],
#' [date_to_dowy()],
#' [dowy_to_date()],
#' [doy_to_dowy()],
#' [dowy_to_doy()],
#' [doy_to_date()],
#' [date_to_doy()],
#' [date_to_wy()]
#' @export
mowy_to_month <- function(mowy,form="numeric"){

  if(!is.null(form) & !form %in% c("numeric","Mmm","long")){
    stop(paste0('form argument must be one of "numeric", "Mmm", or "long". ",
                "Default is "numeric".'))
  }

  if(sum(!is.na(mowy) & !suppressWarnings(
    as.numeric(as.character(mowy)) %in% 1:12),na.rm=T)>0){
    warning(paste0("NAs introduced by coercion. mowy argument must be a ",
                   "number from 1 to 12. Accepted classes are numeric, ",
                   "character and factor."))
  }

  month_table <- dplyr::tibble(mowy) %>%
    mutate(mowy = suppressWarnings(as.numeric(as.character(mowy))),
           mowy = ifelse(mowy %in% 1:12, mowy, NA),
           month = ifelse(mowy<=3,mowy+9,mowy-3))

  if(form=="Mmm"){
    month_table$month <- lubridate::month(month_table$month,label=T)
  }
  if(form=="long"){
    month_table$month <- lubridate::month(month_table$month,label=T,abbr=F)
  }

  month_table$month
}


