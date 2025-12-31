#' Convert water year type abbreviations to full names, e.g., "D" to "Dry". This
#' function is not exported to namespace:waterCALendar, but is run internally by
#' \code{update_wyt()} (also not exported).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @param dat \code{character} column or vector containing a water year type
#' abbreviation, as in: "W", "AN", "BN", "D" or "C".

uniform_wyi <- function(dat){

  dat %>%
    mutate(WYT = ifelse(grepl("W",WYT),"Wet",WYT),
           WYT = ifelse(grepl("A",WYT),"Above Normal",WYT),
           WYT = ifelse(grepl("B",WYT),"Below Normal",WYT),
           WYT = ifelse(grepl("D",WYT),"Dry",WYT),
           WYT = ifelse(grepl("C",WYT),"Critical",WYT))
}
