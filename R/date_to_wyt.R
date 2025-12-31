
#' Convert Date to Water Year Type
#'
#' California water years (beginning October 1) in the Sacramento and San
#' Joaquin Valleys are separately assigned one of five hydrologic
#' classifications (water year types, WYT), based on measured unimpaired runoff:
#' Wet (W); Above Normal (AN); Below Normal (BN); Dry (D); or Critical (C).
#' \code{date_to_wyt()} returns the water year type when you enter a date and a
#' region (Sacramento or San Joaquin).
#'
#' @examples
#' date_to_wyt("2024-11-01", "Sac)
#' date_to_wyt(as.Date("2024-11-01"), "SJ)
#' date_to_wyt("1/1/2024","San Joaquin")
#'
#' @importFrom dplyr tibble mutate left_join filter
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract_all
#' @importFrom purrr map_chr
#' @param date A date or datetime of any format EXCEPT those containing words
#' like "January" or abbreviations like "Jan". Accepted classes are
#' \code{character}, \code{Date}, \code{POSIXct} and \code{POSIXlt}.
#' @param region Sacramento or San Joaquin Valley? Region may be abbreviated
#' (or not) in any of several ways, e.g., "San", "sj", "San Joaquin", "sac",
#' "Sacramento", etc. Only \code{character} is accepted.
#' @param abbreviate Abbreviate the water year type (e.g., show "Dry" as "D")?
#' @order 0
#' @return Returns \code{character} water year type of the date and region
#' entered.
#' @seealso [date_to_mowy()],
#' [mowy_to_month()],
#' [month_to_mowy()],
#' [date_to_dowy()],
#' [dowy_to_date()],
#' [doy_to_dowy()],
#' [dowy_to_doy()],
#' [doy_to_date()],
#' [date_to_doy()]
#' [date_to_Date()]
#' @export
date_to_wyt <- function(date,region,abbreviate=F){

  if(is.null(region)){
    stop(paste0('argument "region" is missing from date_to_wyt(date,region), ',
                'with no default'))
  }
  if(grepl("san",region,ignore.case=T)|grepl("sj",region,ignore.case=T)){
    region <- "San Joaquin Valley"
  }else if(grepl("sac",region,ignore.case=T)){
    region <- "Sacramento Valley"
  }else{
    stop(paste0('date_to wyt() could not recognize region as entered. Please ',
                'enter a region containing\nany of "sac", "Sac","san","San",',
                'or similar to indicate Sacramento or San Joaquin Valley.'))
  }

  date <- date_to_Date(date)

  wy <- date_to_wy(date)

  if(any(!wy %in% wy_indices$Water_year)){

    wy_bad <- unique(wy[!wy %in% wy_indices$Water_year])

    if(length(wy_bad)>1){
      warning(paste0(paste(wy_bad,collapse=" ")," WYTs are not yet available ",
                     "in wy_indices dataset. WYT for ",
                     paste(wy_bad,collapse=" ")," will be NA"))
    }else{
      warning(paste0(wy_bad, " WYT is not yet available in wy_indices dataset.",
                     " WYT for ",wy_bad," will be NA"))
    }
  }

  wyt_table <- tibble(Date=date,
                      Water_year = date_to_wy(date))

  wyt_table <- wyt_table %>%
    left_join(wy_indices %>%
                filter(Region == region,
                       Water_year %in% wyt_table$Water_year)) %>%
    suppressMessages()

  if(abbreviate){
    wyt_table <- wyt_table %>%
      mutate(WYT = str_extract_all(WYT, "[A-Z]") %>%
             map_chr(~paste(.x, collapse = "")))
    }else{
      wyt_table <- wyt_table %>%
        uniform_wyi(.)
    }

  if(all(!wy %in% wy_indices$Water_year)){
    cat("")
  }else{
    wyt_table$WYT
  }
}

