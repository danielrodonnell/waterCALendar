# Update the wy_indices dataset.

#' If waterCALendar::wy_indices dataset is not current, \code{update_wyt()}
#' imports recent water year index and water year type data from
#' "https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST". This function
#' is not exported to namespace:waterCALendar, but is run whenever
#' \code{library(waterCALendar)} is called. \code{update_wyt()} will return a
#' warning if no internet connection is found.
#'
#' @importFrom rvest read_html html_text
#' @importFrom magrittr %>% set_names
#' @importFrom lubridate now
#' @importFrom stringr str_remove str_split str_squish
#' @importFrom dplyr bind_rows as_tibble mutate_at filter arrange distinct vars mutate

update_wyt <- function(){

  wsh_url <- "https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST"

  data("wy_indices", envir = environment())

  wyi <- wy_indices
  wsh_text <- try({
    rvest::read_html(wsh_url) %>%
      rvest::html_text()
  }, silent = T)

  if(inherits(wsh_text, "try-error")){
    warning(paste0("Unable to connect to CDEC. Package waterCALendar has been",
                   " loaded, but wy_indices dataset may not be current.\nTo",
                   " ensure wy_indices is up to date, check your internet",
                   " connection before running library(waterCALendar)."))
    return(invisible(NULL))
  }

  recent <- max(wyi$Water_year,na.rm=T):date_to_wy(now())

  wsh_lines <- str_remove(str_split(wsh_text,"\n")[[1]],"\r") %>%
    str_squish()
  wsh_head <- str_remove(wsh_lines[grepl("WYsum",wsh_lines)],"\r") %>%
    str_squish()
  wsh_data_lines <- wsh_lines[as.numeric(substr(wsh_lines,1,4)) %in% recent &
                                nchar(wsh_lines)>40] %>% suppressWarnings()
  wsh_lines2 <- c(wsh_head,wsh_data_lines)
  wsh_sep <- str_split(wsh_lines2," ")

  if(as.numeric(wsh_sep[[length(wsh_sep)]][1]) %in% wy_indices$Water_year){
    message(paste0("Your wy_indices dataset is up to date. More current water",
                   " year type and index information may be available online."))
    return(invisible(NULL))

  }else{
     cat("Updating wy_indices dataset...\n")

     wsh_mat <- matrix(ncol=length(wsh_sep[[1]]),nrow=length(wsh_sep)-1,
                       dimnames=list(NULL,wsh_sep[[1]]))

     fill_wsh <- lapply(1:nrow(wsh_mat), function(i){
       wsh_mat[i,] <<- wsh_sep[-1][[i]]})

     if(any(!wsh_mat[,1] %in% wy_indices$Water_year)){

       wy_ind <- bind_rows(as_tibble(wsh_mat[,1:6]),
                           as_tibble(wsh_mat[,7:11])) %>%
         mutate(location = ifelse(is.na(WY),"San Joaquin Valley",
                                  "Sacramento Valley"),
                WY = rep(WY[!is.na(WY)],2)) %>%
         set_names(names(wyi)) %>%
         mutate_at(vars(-WYT,-Region),
                   ~as.numeric(.)) %>%
         filter(!WYT %in% wyi$Water_year)

       new_wy <- unique(wy_ind$Water_year[!wy_ind$Water_year %in%
                                            wy_indices$Water_year])
       message(paste0("Water years added to wy_indices dataset: ",
                      paste(new_wy,collapse=", ")))

       wy_ind_udt <- wyi %>%
         bind_rows(wy_ind) %>%
         arrange(Region,Water_year) %>%
         set_names(names(wyi)) %>%
         uniform_wyi(.) %>%
         distinct()

       }else{
         wy_ind_udt <- wy_indices %>%
           uniform_wyi(.) %>%
           distinct()
       }

     assign("wy_indices", wy_ind_udt, envir = pkg_env)
     }
}
