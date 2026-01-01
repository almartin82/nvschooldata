# ==============================================================================
# Enrollment Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading enrollment data from the
# Nevada Department of Education (NDE).
#
# Data is available from 2015-16 (end_year = 2016) through 2025-26 (end_year = 2026).
#
# ==============================================================================

#' Fetch Nevada enrollment data
#'
#' Downloads and processes enrollment data from the Nevada Department of
#' Education. Data is available from the NDE website.
#'
#' Nevada provides enrollment data from 2015-16 through the current year.
#' Data formats vary by year:
#' \itemize{
#'   \item 2016-2017: Legacy format (limited data extraction)
#'   \item 2018-2020: Intermediate format (consolidated data)
#'   \item 2021-2026: Modern format (full school/district data with demographics)
#' }
#'
#' @param end_year A school year. Year is the end of the academic year - eg 2024-25
#'   school year is year '2025'. Valid values are 2016 through 2026.
#' @param tidy If TRUE (default), returns data in long (tidy) format with subgroup
#'   column. If FALSE, returns wide format with one row per entity/grade.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from NDE.
#' @return Data frame with enrollment data. Tidy format includes columns:
#'   \itemize{
#'     \item end_year: School year end (e.g., 2025 for 2024-25)
#'     \item lea_code: Local Education Agency code
#'     \item lea_name: LEA name
#'     \item district_code: Master District code
#'     \item district_name: District name
#'     \item school_code: School code (NA for district-level)
#'     \item school_name: School name (NA for district-level)
#'     \item entity_type: "District" or "School"
#'     \item grade_level: Grade level or "TOTAL"
#'     \item subgroup: Demographic or population subgroup
#'     \item n_students: Student count
#'     \item pct: Percentage of total enrollment
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Get 2025 enrollment data (2024-25 school year)
#' enr_2025 <- fetch_enr(2025)
#'
#' # Get wide format (one row per entity/grade)
#' enr_wide <- fetch_enr(2025, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' enr_fresh <- fetch_enr(2025, use_cache = FALSE)
#'
#' # Get multiple years
#' enr_multi <- purrr::map_df(2021:2025, fetch_enr)
#' }
fetch_enr <- function(end_year, tidy = TRUE, use_cache = TRUE) {

  # Validate year
  available_years <- list_enr_years()
  if (!end_year %in% available_years) {
    stop(paste(
      "end_year must be between", min(available_years), "and", max(available_years), ".",
      "Use list_enr_years() to see available years."
    ))
  }

  # Warn about legacy format limitations
  if (end_year <= 2017) {
    warning(paste(
      "Years 2016-2017 use legacy format with limited data extraction.",
      "Consider using years 2021+ for complete data."
    ))
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "tidy" else "wide"

  # Check cache first
  if (use_cache && cache_exists(end_year, cache_type)) {
    message(paste("Using cached data for", end_year))
    return(read_cache(end_year, cache_type))
  }

  # Get raw data
  message(paste("Downloading enrollment data for", end_year, "..."))
  raw <- get_raw_enr(end_year)

  # Process to standard schema
  processed <- process_enr(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_enr(processed) |>
      id_enr_aggs()
  }

  # Cache the result
  if (use_cache) {
    write_cache(processed, end_year, cache_type)
    message(paste("Cached data for", end_year))
  }

  processed
}


#' Fetch enrollment data for multiple years
#'
#' Convenience function to download enrollment data for a range of years.
#' Results are combined into a single data frame.
#'
#' @param start_year First year to fetch
#' @param end_year Last year to fetch
#' @param tidy If TRUE (default), returns tidy format
#' @param use_cache If TRUE (default), uses cached data when available
#' @return Combined data frame with enrollment data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 5 years of enrollment data
#' enr_history <- fetch_enr_range(2021, 2025)
#' }
fetch_enr_range <- function(start_year, end_year, tidy = TRUE, use_cache = TRUE) {
  years <- start_year:end_year
  purrr::map_df(years, function(y) {
    tryCatch(
      fetch_enr(y, tidy = tidy, use_cache = use_cache),
      error = function(e) {
        warning(paste("Could not fetch data for", y, ":", e$message))
        NULL
      }
    )
  })
}


#' Get Nevada statewide enrollment summary
#'
#' Returns a summary of statewide enrollment totals by year.
#'
#' @param end_year School year end (or vector of years)
#' @param use_cache If TRUE (default), uses cached data
#' @return Data frame with statewide enrollment by year
#' @export
#' @examples
#' \dontrun{
#' # Get statewide summary for 2025
#' state_summary <- get_state_enrollment(2025)
#'
#' # Get 5-year trend
#' state_trend <- get_state_enrollment(2021:2025)
#' }
get_state_enrollment <- function(end_year, use_cache = TRUE) {
  purrr::map_df(end_year, function(y) {
    df <- fetch_enr(y, tidy = TRUE, use_cache = use_cache)

    # Aggregate to state level
    df |>
      dplyr::filter(
        entity_type == "District",
        subgroup == "total_enrollment",
        grade_level == "TOTAL"
      ) |>
      dplyr::summarize(
        end_year = y,
        n_districts = dplyr::n_distinct(district_code),
        total_enrollment = sum(n_students, na.rm = TRUE),
        .groups = "drop"
      )
  })
}
