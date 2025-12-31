#' 1901-2024 water year index table, modified from
#' https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST. Metadata
#' provided below is a brief summary. Follow the CDEC link for more detailed
#' explanations.
#'
#' Original table header reads:
#'
#' ".T WRWSIHIST 2505081541/
#' Department of Water Resources
#' California Cooperative Snow Surveys

#' Chronological Reconstructed Sacramento and San Joaquin Valley
#' Water Year Hydrologic Classification Indices

#' Based on measured unimpaired runoff (in million acre-feet), subject to
#' revision."
#'
#' If you are currently living in the future (post-2024), \code{update_wyt()}
#' will automatically update the \code{wy_indices} dataset with the most current
#' water year index and water year type data from CDEC, as long as you are
#' connected to the internet (you are, after all, living in the future).
#'
#' @format A data frame with 248 rows and 7 columns:
#' \describe{
#'   \item{Water_year}{Water year, beginning in 1901 for San Joaquin Valley and
#'                     1906 for Sacramento Valley}
#'   \item{Oct_Mar}{October - March total unimpaired runoff}
#'   \item{Apr_Jul}{April - July total unimpaired runoff}
#'   \item{WY_sum}{Annual sum of unimpaired runoff for Oct - Sep}
#'   \item{Index}{Water year hydrologic index}
#'   \item{WYT}{Water year type (hydrologic classification)}
#'   \item{Region}{Sacramento or San Joaquin Valley}
#' }
#' @source California Department of Water Resources
"wy_indices"

