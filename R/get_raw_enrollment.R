# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from the
# Nevada Department of Education (NDE).
#
# Data is available from the NDE website at:
# https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/accountability/data-requests/enrollment-for-nevada-public-schools
#
# Historical data is available from 2015-16 through the current year,
# though 2024-25 (end_year = 2025) is currently unavailable from NDE.
# File formats vary by year:
# - 2015-16 to 2016-17: Legacy format with separate sheets per district
# - 2017-18 to 2019-20: Intermediate format with consolidated sheets
# - 2020-21 onwards: Modern format with LEA codes and full structure
#
# ==============================================================================

#' Get known enrollment file URLs
#'
#' Returns a named list mapping end years to their download URLs.
#' URLs are from the NDE Azure storage and may change when files are updated.
#'
#' @return Named list of URLs keyed by end_year
#' @keywords internal
get_enrollment_urls <- function() {
  # Note: 2025 data URL is currently unavailable from NDE
  # The original URL was removed/replaced by NDE in late 2025
  list(
    "2026" = "https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/suppressed_2025_2026_school_year_enrollment_counts_for_website_11_03_25_c9f4d44d51.xlsx",
    "2024" = "https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/2023_2024_school_year_validation_day_student_counts_2e68b10570.xlsx",
    "2023" = "https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/2022_2023_enrollment_numbers_2224fa62e5.xlsx",
    "2022" = "https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/2021_2022schoolyearcounts1021_0199ab6d3b.xlsx",
    "2021" = "https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/2020_2021_School_Year_Numberof_Students_f703bba063.xlsx",
    "2020" = "https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/20192020_Numberof_Student11142019_651a96a7a8.xlsx",
    "2019" = "https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/20182019_Numberof_Student102018_1ea97446e0.xlsx",
    "2018" = "https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/20172018_Numberofstudents_c45aa90c9a.xlsx",
    "2017" = "https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/20162017_Oct1_Enrol_Num_1a5ef511e2.xlsx",
    "2016" = "https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/2015_2016_SY_Student_Counts_a632177572.xlsx"
  )
}


#' List available enrollment data years
#'
#' Returns a vector of years for which enrollment data is available
#' from the Nevada Department of Education.
#'
#' Nevada provides enrollment data from 2015-16 (end year 2016) through
#' 2025-26 (end year 2026), except 2024-25 (end year 2025) which is
#' currently unavailable.
#'
#' @return Integer vector of available years
#' @export
#' @examples
#' \dontrun{
#' available_years <- list_enr_years()
#' }
list_enr_years <- function() {
  urls <- get_enrollment_urls()
  years <- as.integer(names(urls))
  sort(years)
}


#' Get the format type for a given year
#'
#' Nevada enrollment data comes in different formats by year:
#' - "legacy" (2016-2017): Separate sheets per district
#' - "intermediate" (2018-2020): Consolidated sheets, simpler column names
#' - "modern" (2021+): Full LEA codes, Master District codes, complete structure
#'
#' @param end_year School year end
#' @return Character string indicating format type
#' @keywords internal
get_format_type <- function(end_year) {
  if (end_year <= 2017) {
    "legacy"
  } else if (end_year <= 2020) {
    "intermediate"
  } else {
    "modern"
  }
}


#' Download raw enrollment data from NDE
#'
#' Downloads enrollment data from the Nevada Department of Education.
#' Data includes district and building level enrollment by grade and demographics.
#'
#' @param end_year School year end (2023-24 = 2024)
#' @return Raw data frame from NDE
#' @keywords internal
get_raw_enr <- function(end_year) {

  available_years <- list_enr_years()
  if (!end_year %in% available_years) {
    stop(paste(
      "Data for year", end_year, "is not available.",
      "Available years:", paste(range(available_years), collapse = " to ")
    ))
  }

  format_type <- get_format_type(end_year)

  switch(format_type,
    "modern" = get_raw_enr_modern(end_year),
    "intermediate" = get_raw_enr_intermediate(end_year),
    "legacy" = get_raw_enr_legacy(end_year)
  )
}


#' Download modern format NDE enrollment data (2021+)
#'
#' Downloads enrollment data with full LEA codes and school identifiers.
#' Modern format has sheets for school grade subgroups and school level totals.
#'
#' @param end_year School year end
#' @return Raw data frame with district and school enrollment
#' @keywords internal
get_raw_enr_modern <- function(end_year) {

  urls <- get_enrollment_urls()
  url <- urls[[as.character(end_year)]]

  if (is.null(url)) {
    stop(paste("No URL found for year", end_year))
  }

  # Download file
  tname <- tempfile(pattern = "nv_enr", tmpdir = tempdir(), fileext = ".xlsx")

  result <- tryCatch({
    # Set longer timeout for NDE Azure storage
    old_timeout <- getOption("timeout")
    options(timeout = 300)
    on.exit(options(timeout = old_timeout), add = TRUE)

    downloader::download(url, dest = tname, mode = "wb", quiet = TRUE)

    # Check if file is valid
    file_info <- file.info(tname)
    if (file_info$size < 10000) {
      stop("Downloaded file too small, likely error page")
    }

    # Read school level totals (has demographics and special populations)
    school_totals <- tryCatch({
      readxl::read_excel(tname, sheet = "School Level Totals")
    }, error = function(e) {
      warning("Could not read 'School Level Totals' sheet for year ", end_year, ": ", e$message)
      NULL
    })

    # Read school grade subgroups (has grade-level detail)
    school_grades <- tryCatch({
      readxl::read_excel(tname, sheet = "School Grade Subgroups Data")
    }, error = function(e) {
      warning("Could not read 'School Grade Subgroups Data' sheet for year ", end_year, ": ", e$message)
      NULL
    })

    # Read district data - prefer District Level Totals first, then subgroups
    district_data <- tryCatch({
      sheets <- readxl::excel_sheets(tname)

      # Try District Level Totals first (has complete data)
      dist_totals_sheet <- grep("^District Level Totals$", sheets, value = TRUE, ignore.case = TRUE)
      if (length(dist_totals_sheet) > 0) {
        readxl::read_excel(tname, sheet = dist_totals_sheet[1])
      } else {
        # Fallback to subgroups sheets
        dist_sheet <- grep("District.*Subgroups|District.*Grade", sheets, value = TRUE, ignore.case = TRUE)
        if (length(dist_sheet) > 0) {
          readxl::read_excel(tname, sheet = dist_sheet[1])
        } else {
          NULL
        }
      }
    }, error = function(e) {
      warning("Could not read district data sheet for year ", end_year, ": ", e$message)
      NULL
    })

    list(
      school_totals = school_totals,
      school_grades = school_grades,
      district_data = district_data,
      format = "modern"
    )
  }, error = function(e) {
    stop(paste(
      "Could not download enrollment data for year", end_year, ":",
      e$message
    ))
  })

  # Clean up temp file
  if (file.exists(tname)) unlink(tname)

  result$end_year <- end_year
  result
}


#' Download intermediate format NDE enrollment data (2018-2020)
#'
#' Downloads enrollment data with consolidated sheets.
#' Column names are simpler than modern format.
#'
#' @param end_year School year end
#' @return Raw data frame
#' @keywords internal
get_raw_enr_intermediate <- function(end_year) {

  urls <- get_enrollment_urls()
  url <- urls[[as.character(end_year)]]

  if (is.null(url)) {
    stop(paste("No URL found for year", end_year))
  }

  tname <- tempfile(pattern = "nv_enr", tmpdir = tempdir(), fileext = ".xlsx")

  result <- tryCatch({
    # Set longer timeout for NDE Azure storage
    old_timeout <- getOption("timeout")
    options(timeout = 300)
    on.exit(options(timeout = old_timeout), add = TRUE)

    downloader::download(url, dest = tname, mode = "wb", quiet = TRUE)

    file_info <- file.info(tname)
    if (file_info$size < 10000) {
      stop("Downloaded file too small, likely error page")
    }

    sheets <- readxl::excel_sheets(tname)

    # School grade details
    school_grades <- tryCatch({
      grade_sheet <- grep("School.*Grade|Schools.*Grade", sheets, value = TRUE, ignore.case = TRUE)
      if (length(grade_sheet) > 0) {
        readxl::read_excel(tname, sheet = grade_sheet[1])
      } else {
        NULL
      }
    }, error = function(e) {
      warning("Could not read school grade sheet for year ", end_year, ": ", e$message)
      NULL
    })

    # School totals
    school_totals <- tryCatch({
      total_sheet <- grep("^School Totals$|^School Level", sheets, value = TRUE, ignore.case = TRUE)
      if (length(total_sheet) > 0) {
        readxl::read_excel(tname, sheet = total_sheet[1])
      } else {
        NULL
      }
    }, error = function(e) {
      warning("Could not read school totals sheet for year ", end_year, ": ", e$message)
      NULL
    })

    # District totals
    district_data <- tryCatch({
      dist_sheet <- grep("District", sheets, value = TRUE, ignore.case = TRUE)
      if (length(dist_sheet) > 0) {
        readxl::read_excel(tname, sheet = dist_sheet[1])
      } else {
        NULL
      }
    }, error = function(e) {
      warning("Could not read district data sheet for year ", end_year, ": ", e$message)
      NULL
    })

    list(
      school_totals = school_totals,
      school_grades = school_grades,
      district_data = district_data,
      format = "intermediate"
    )
  }, error = function(e) {
    stop(paste(
      "Could not download enrollment data for year", end_year, ":",
      e$message
    ))
  })

  if (file.exists(tname)) unlink(tname)

  result$end_year <- end_year
  result
}


#' Download legacy format NDE enrollment data (2016-2017)
#'
#' Downloads enrollment data with separate sheets per district.
#' Requires combining data from multiple sheets.
#'
#' @param end_year School year end
#' @return Raw data frame
#' @keywords internal
get_raw_enr_legacy <- function(end_year) {

  urls <- get_enrollment_urls()
  url <- urls[[as.character(end_year)]]

  if (is.null(url)) {
    stop(paste("No URL found for year", end_year))
  }

  tname <- tempfile(pattern = "nv_enr", tmpdir = tempdir(), fileext = ".xlsx")

  result <- tryCatch({
    # Set longer timeout for NDE Azure storage
    old_timeout <- getOption("timeout")
    options(timeout = 300)
    on.exit(options(timeout = old_timeout), add = TRUE)

    downloader::download(url, dest = tname, mode = "wb", quiet = TRUE)

    file_info <- file.info(tname)
    if (file_info$size < 10000) {
      stop("Downloaded file too small, likely error page")
    }

    sheets <- readxl::excel_sheets(tname)

    # State-level data
    state_data <- tryCatch({
      state_sheet <- grep("State.*Grade|State_Grade", sheets, value = TRUE, ignore.case = TRUE)
      if (length(state_sheet) > 0) {
        readxl::read_excel(tname, sheet = state_sheet[1])
      } else {
        NULL
      }
    }, error = function(e) {
      warning("Could not read state data sheet for year ", end_year, ": ", e$message)
      NULL
    })

    # District race/ethnicity by grade (consolidated sheet)
    district_grade_data <- tryCatch({
      dist_sheet <- grep("District.*Race|District_Race", sheets, value = TRUE, ignore.case = TRUE)
      if (length(dist_sheet) > 0) {
        readxl::read_excel(tname, sheet = dist_sheet[1], skip = 1)
      } else {
        NULL
      }
    }, error = function(e) {
      warning("Could not read district race data sheet for year ", end_year, ": ", e$message)
      NULL
    })

    # Note: Legacy format has individual district sheets (CCSD, WCSD, etc.)
    # These would need to be combined for complete school-level data
    # For simplicity, we return available consolidated data

    list(
      state_data = state_data,
      district_data = district_grade_data,
      school_grades = NULL,
      school_totals = NULL,
      format = "legacy"
    )
  }, error = function(e) {
    stop(paste(
      "Could not download enrollment data for year", end_year, ":",
      e$message
    ))
  })

  if (file.exists(tname)) unlink(tname)

  result$end_year <- end_year
  result
}


#' Import local enrollment Excel files
#'
#' Imports enrollment data from locally downloaded Excel files.
#' Use this when automatic downloads fail.
#'
#' @param file_path Path to enrollment Excel file
#' @param end_year School year end (e.g., 2024 for 2023-24)
#' @return Raw data list with enrollment data
#' @export
#' @examples
#' \dontrun{
#' # After downloading file from NDE portal:
#' enr_raw <- import_local_enrollment(
#'   file_path = "~/Downloads/2024_2025_school_year_validation_day_student_counts.xlsx",
#'   end_year = 2025
#' )
#' }
import_local_enrollment <- function(file_path, end_year) {

  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }

  format_type <- get_format_type(end_year)
  sheets <- readxl::excel_sheets(file_path)

  if (format_type == "modern") {
    school_totals <- tryCatch({
      readxl::read_excel(file_path, sheet = "School Level Totals")
    }, error = function(e) {
      warning("Could not read 'School Level Totals' from ", file_path, ": ", e$message)
      NULL
    })

    school_grades <- tryCatch({
      readxl::read_excel(file_path, sheet = "School Grade Subgroups Data")
    }, error = function(e) {
      warning("Could not read 'School Grade Subgroups Data' from ", file_path, ": ", e$message)
      NULL
    })

    # Prefer District Level Totals sheet
    district_data <- tryCatch({
      dist_totals_sheet <- grep("^District Level Totals$", sheets, value = TRUE, ignore.case = TRUE)
      if (length(dist_totals_sheet) > 0) {
        readxl::read_excel(file_path, sheet = dist_totals_sheet[1])
      } else {
        dist_sheet <- grep("District.*Subgroups|District.*Grade", sheets, value = TRUE, ignore.case = TRUE)
        if (length(dist_sheet) > 0) {
          readxl::read_excel(file_path, sheet = dist_sheet[1])
        } else {
          NULL
        }
      }
    }, error = function(e) {
      warning("Could not read district data from ", file_path, ": ", e$message)
      NULL
    })

    result <- list(
      school_totals = school_totals,
      school_grades = school_grades,
      district_data = district_data,
      format = "modern",
      end_year = end_year
    )
  } else {
    # For non-modern formats, try to read available sheets
    school_grades <- tryCatch({
      grade_sheet <- grep("School.*Grade|Grade", sheets, value = TRUE, ignore.case = TRUE)
      if (length(grade_sheet) > 0) {
        readxl::read_excel(file_path, sheet = grade_sheet[1])
      } else {
        NULL
      }
    }, error = function(e) {
      warning("Could not read school grade sheet from ", file_path, ": ", e$message)
      NULL
    })

    school_totals <- tryCatch({
      total_sheet <- grep("School.*Total|Totals", sheets, value = TRUE, ignore.case = TRUE)
      if (length(total_sheet) > 0) {
        readxl::read_excel(file_path, sheet = total_sheet[1])
      } else {
        NULL
      }
    }, error = function(e) {
      warning("Could not read school totals sheet from ", file_path, ": ", e$message)
      NULL
    })

    district_data <- tryCatch({
      dist_sheet <- grep("District", sheets, value = TRUE, ignore.case = TRUE)
      if (length(dist_sheet) > 0) {
        readxl::read_excel(file_path, sheet = dist_sheet[1])
      } else {
        NULL
      }
    }, error = function(e) {
      warning("Could not read district data sheet from ", file_path, ": ", e$message)
      NULL
    })

    result <- list(
      school_totals = school_totals,
      school_grades = school_grades,
      district_data = district_data,
      format = format_type,
      end_year = end_year
    )
  }

  message(paste("Loaded enrollment data for", end_year))
  result
}
