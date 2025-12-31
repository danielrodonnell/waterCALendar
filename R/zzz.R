#' Run function \code{update_wyt} whenever \code{library(waterCALendar)} is
#' called. Not exported to namespace:waterCALendar.

#' Create package environment to hold updated data
pkg_env <- new.env(parent = emptyenv())

#' Run \code{update_wyt()}
.onAttach <- function(libname, pkgname) {

  data("wy_indices", envir = environment())

  if (interactive()){
    update_wyt()
  }
}

get_wy_indices <- function() {
  if (exists("wy_indices", envir = pkg_env)) {
    return(get("wy_indices", envir = pkg_env))
  } else {
    return(wy_indices)  # Return original package data
  }
}
