#' @keywords internal
#' @importFrom utils data
#' @importFrom stats na.omit

"_PACKAGE"

# Suppress R CMD check notes for dplyr NSE variables
utils::globalVariables(c(
  ".", "Mmm", "Region", "WY", "WYT", "Water_year",
  "dowy", "doy", "err_365", "err_366", "leap", "long",
  "wy_indices"
))

## usethis namespace: start
## usethis namespace: end

NULL
