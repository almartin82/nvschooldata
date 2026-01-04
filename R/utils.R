# ==============================================================================
# Utility Functions
# ==============================================================================

#' @importFrom rlang .data
NULL


#' Get available years for enrollment data
#'
#' Returns information about the range of school years for which enrollment
#' data is available from the Nevada Department of Education.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{min_year}{The earliest available school year end (e.g., 2016 = 2015-16)}
#'     \item{max_year}{The most recent available school year end (e.g., 2024 = 2023-24)}
#'     \item{description}{A human-readable description of data availability}
#'   }
#' @export
#' @examples
#' years <- get_available_years()
#' years$min_year
#' years$max_year
get_available_years <- function() {
  list(
    min_year = 2016,
    max_year = 2026,
    description = "Nevada Department of Education enrollment data is available from 2016 (2015-16 school year) through 2026 (2025-26 school year), except 2025 (2024-25) which is unavailable from NDE. Years 2016-2017 use legacy format with limited data extraction. Years 2018-2020 use intermediate format. Years 2021-2026 use modern format with full demographics."
  )
}
