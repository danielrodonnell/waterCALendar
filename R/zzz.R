
# Create package environment to hold updated data
pkg_env <- new.env(parent = emptyenv())

# Internal function - runs when package is loaded
# Updates wy_indices data if in interactive session
.onAttach <- function(libname, pkgname) {
  utils::data("wy_indices", envir = environment())
  if (interactive()) {
    update_wyt()
  }
}

# Internal function to retrieve wy_indices
get_wy_indices <- function() {
  if (exists("wy_indices", envir = pkg_env)) {
    return(get("wy_indices", envir = pkg_env))
  } else {
    return(wy_indices)  # Return original package data
  }
}
