# ==============================================================================
# Enrollment Data Tidying Functions
# ==============================================================================
#
# This file contains functions for transforming enrollment data from wide
# format to long (tidy) format and identifying aggregation levels.
#
# ==============================================================================

#' Tidy enrollment data
#'
#' Transforms wide enrollment data to long format with subgroup column.
#' Each row represents a single observation (entity + grade + subgroup).
#'
#' @param df A wide data.frame of processed enrollment data
#' @return A long data.frame of tidied enrollment data
#' @export
#' @examples
#' \dontrun{
#' wide_data <- fetch_enr(2025, tidy = FALSE)
#' tidy_data <- tidy_enr(wide_data)
#' }
tidy_enr <- function(df) {

  # Invariant columns (identifiers that stay the same)
  invariants <- c(
    "end_year", "lea_code", "lea_name",
    "district_code", "district_name",
    "school_code", "school_name",
    "entity_type", "grade_level"
  )
  invariants <- invariants[invariants %in% names(df)]

  # Demographic subgroups to tidy
  demo_cols <- c(
    "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial"
  )
  demo_cols <- demo_cols[demo_cols %in% names(df)]

  # Gender columns
  gender_cols <- c("female", "male")
  gender_cols <- gender_cols[gender_cols %in% names(df)]

  # Special population subgroups
  special_cols <- c(
    "free_reduced_lunch", "special_ed", "lep", "migrant", "foster", "military", "homeless"
  )
  special_cols <- special_cols[special_cols %in% names(df)]

  all_subgroups <- c(demo_cols, gender_cols, special_cols)

  # Transform subgroups to long format
  if (length(all_subgroups) > 0) {
    tidy_subgroups <- purrr::map_df(
      all_subgroups,
      function(.x) {
        df |>
          dplyr::rename(n_students = dplyr::all_of(.x)) |>
          dplyr::select(dplyr::all_of(c(invariants, "n_students", "enrollment_total"))) |>
          dplyr::mutate(
            subgroup = .x,
            pct = n_students / enrollment_total
          ) |>
          dplyr::select(dplyr::all_of(c(invariants, "subgroup", "n_students", "pct")))
      }
    )
  } else {
    tidy_subgroups <- NULL
  }

  # Extract total enrollment as a "subgroup"
  if ("enrollment_total" %in% names(df)) {
    tidy_total <- df |>
      dplyr::select(dplyr::all_of(c(invariants, "enrollment_total"))) |>
      dplyr::mutate(
        n_students = enrollment_total,
        subgroup = "total_enrollment",
        pct = 1.0
      ) |>
      dplyr::select(dplyr::all_of(c(invariants, "subgroup", "n_students", "pct")))
  } else {
    tidy_total <- NULL
  }

  # Combine all tidy data
  dplyr::bind_rows(tidy_total, tidy_subgroups) |>
    dplyr::filter(!is.na(n_students))
}


#' Identify enrollment aggregation levels
#'
#' Adds boolean flags to identify state, district, and school level records.
#'
#' @param df Enrollment dataframe, output of tidy_enr
#' @return data.frame with boolean aggregation flags
#' @export
#' @examples
#' \dontrun{
#' tidy_data <- fetch_enr(2025)
#' with_flags <- id_enr_aggs(tidy_data)
#' }
id_enr_aggs <- function(df) {

  result <- df |>
    dplyr::mutate(
      # State level: entity_type == "State"
      is_state = entity_type == "State",

      # District level: entity_type == "District"
      is_district = entity_type == "District",

      # School level: entity_type == "School"
      is_school = entity_type == "School",

      # Aggregation flag based on ID presence
      aggregation_flag = dplyr::case_when(
        !is.na(school_code) & school_code != "" ~ "campus",
        !is.na(district_code) & district_code != "" ~ "district",
        TRUE ~ "state"
      )
    )

  # Charter schools (SPCSA districts) - only if lea_name exists
  if ("lea_name" %in% names(df)) {
    result <- result |>
      dplyr::mutate(
        is_charter = !is.na(lea_name) &
          grepl("Charter|SPCSA", lea_name, ignore.case = TRUE)
      )
  } else if ("district_name" %in% names(df)) {
    result <- result |>
      dplyr::mutate(
        is_charter = !is.na(district_name) &
          grepl("Charter|SPCSA", district_name, ignore.case = TRUE)
      )
  } else {
    result$is_charter <- FALSE
  }

  result
}


#' Custom Enrollment Grade Level Aggregates
#'
#' Creates aggregations for common grade groupings: K-8, 9-12 (HS), K-12.
#' Useful for comparing elementary and secondary enrollment patterns.
#'
#' @param df A tidy enrollment df from tidy_enr
#' @return df of aggregated enrollment data with K8, HS, K12 grade levels
#' @export
#' @examples
#' \dontrun{
#' tidy_data <- fetch_enr(2025)
#' grade_aggregates <- enr_grade_aggs(tidy_data)
#' }
enr_grade_aggs <- function(df) {

  # Group by invariants (everything except grade_level and counts)
  group_vars <- c(
    "end_year", "lea_code", "lea_name",
    "district_code", "district_name",
    "school_code", "school_name",
    "entity_type", "subgroup",
    "is_state", "is_district", "is_school", "is_charter"
  )
  group_vars <- group_vars[group_vars %in% names(df)]

  # K-8 aggregate
  k8_agg <- df |>
    dplyr::filter(
      subgroup == "total_enrollment",
      grade_level %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08")
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarize(
      n_students = sum(n_students, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      grade_level = "K8",
      pct = NA_real_
    )

  # High school (9-12) aggregate
  hs_agg <- df |>
    dplyr::filter(
      subgroup == "total_enrollment",
      grade_level %in% c("09", "10", "11", "12")
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarize(
      n_students = sum(n_students, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      grade_level = "HS",
      pct = NA_real_
    )

  # K-12 aggregate (excludes PK)
  k12_agg <- df |>
    dplyr::filter(
      subgroup == "total_enrollment",
      grade_level %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08",
                         "09", "10", "11", "12")
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarize(
      n_students = sum(n_students, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      grade_level = "K12",
      pct = NA_real_
    )

  dplyr::bind_rows(k8_agg, hs_agg, k12_agg)
}


#' Filter enrollment data by district
#'
#' Convenience function to filter enrollment data to a specific district
#' using the LEA code or district code.
#'
#' @param df Enrollment dataframe
#' @param code District code (LEA code or Master District Code)
#' @param include_schools If TRUE (default), include school-level data.
#'   If FALSE, only return district-level aggregates.
#' @return Filtered data frame
#' @export
#' @examples
#' \dontrun{
#' # Get Clark County School District (code 02)
#' ccsd <- fetch_enr(2025) |> filter_district("02")
#' }
filter_district <- function(df, code, include_schools = TRUE) {
  # Ensure code is properly formatted
  code <- sprintf("%02d", as.integer(code))

  if (include_schools) {
    df |>
      dplyr::filter(lea_code == code | district_code == code)
  } else {
    df |>
      dplyr::filter(
        (lea_code == code | district_code == code),
        entity_type == "District"
      )
  }
}


#' Filter enrollment data by county
#'
#' Convenience function to filter enrollment data to a specific Nevada county.
#' Matches on district name containing the county name.
#'
#' @param df Enrollment dataframe
#' @param county_name Name of Nevada county (case-insensitive)
#' @return Filtered data frame
#' @export
#' @examples
#' \dontrun{
#' # Get Clark County schools
#' clark <- fetch_enr(2025) |> filter_county("Clark")
#' }
filter_county <- function(df, county_name) {
  df |>
    dplyr::filter(
      grepl(county_name, district_name, ignore.case = TRUE) |
      grepl(county_name, lea_name, ignore.case = TRUE)
    )
}
