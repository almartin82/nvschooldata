# ==============================================================================
# School Directory Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading school directory data from the
# Nevada Department of Education (NDE) website.
#
# Data source: https://doe.nv.gov/school-and-district-information
#
# ==============================================================================

#' Fetch Nevada school directory data
#'
#' Downloads and processes school directory data from the Nevada Department
#' of Education. This includes all public schools with contact information
#' and administrator names.
#'
#' @param end_year Currently unused. The directory data represents current
#'   schools and is not year-specific. Included for API consistency with
#'   other fetch functions.
#' @param tidy If TRUE (default), returns data in a standardized format with
#'   consistent column names. If FALSE, returns raw column names from NDE.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from NDE.
#' @return A tibble with school directory data. Columns include:
#'   \itemize{
#'     \item \code{state_school_id}: NDE state school code
#'     \item \code{state_district_id}: NDE master district code
#'     \item \code{school_name}: School name
#'     \item \code{school_type}: Type of school (e.g., "RG: Regular", "AL: Alternative")
#'     \item \code{grades_served}: Grade levels offered (low to high)
#'     \item \code{address}: Mailing address
#'     \item \code{city}: City
#'     \item \code{state}: State (always "NV")
#'     \item \code{zip}: ZIP code
#'     \item \code{phone}: Phone number
#'     \item \code{principal_name}: Principal name
#'     \item \code{principal_email}: Principal email address
#'     \item \code{charter_status}: Charter indicator (YES/NO)
#'     \item \code{operational_status}: Operational status
#'     \item \code{website}: School website URL
#'   }
#' @details
#' The directory data is downloaded as an Excel file from the NDE website.
#' This data represents the current state of Nevada schools and is updated
#' periodically by NDE.
#'
#' Note: The directory file contains only school-level data. District
#' information is derived from the Master District Code.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get school directory data
#' dir_data <- fetch_directory()
#'
#' # Get raw format (original NDE column names)
#' dir_raw <- fetch_directory(tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' dir_fresh <- fetch_directory(use_cache = FALSE)
#'
#' # Filter to active schools only
#' library(dplyr)
#' active_schools <- dir_data |>
#'   filter(operational_status == "OP: Open")
#'
#' # Find all charter schools
#' charters <- dir_data |>
#'   filter(charter_status == "YES")
#' }
fetch_directory <- function(end_year = NULL, tidy = TRUE, use_cache = TRUE) {

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "directory_tidy" else "directory_raw"

  # Check cache first
  if (use_cache && cache_exists_directory(cache_type)) {
    message("Using cached school directory data")
    return(read_cache_directory(cache_type))
  }

  # Get raw data from NDE
  raw <- get_raw_directory()

  # Process to standard schema
  if (tidy) {
    result <- process_directory(raw)
  } else {
    result <- raw
  }

  # Cache the result
  if (use_cache) {
    write_cache_directory(result, cache_type)
  }

  result
}


#' Get raw school directory data from NDE
#'
#' Downloads the raw school directory Excel file from the Nevada
#' Department of Education website.
#'
#' @return Raw data frame as downloaded from NDE
#' @keywords internal
get_raw_directory <- function() {

  # Build download URL
  url <- build_directory_url()

  message("Downloading school directory data from NDE...")

  # Download file to temp location
  tname <- tempfile(pattern = "nde_directory", tmpdir = tempdir(), fileext = ".xlsx")

  # Set longer timeout for large files
  old_timeout <- getOption("timeout")
  options(timeout = 300)  # 5 minutes

  tryCatch({
    downloader::download(url, dest = tname, mode = "wb", quiet = TRUE)
  }, error = function(e) {
    options(timeout = old_timeout)
    stop(paste("Failed to download school directory data from NDE:", e$message))
  })

  options(timeout = old_timeout)

  # Check if download was successful (file should be reasonably large)
  file_info <- file.info(tname)
  if (file_info$size < 50000) {
    stop("Download failed - file too small, may be error page")
  }

  message(paste("Downloaded", round(file_info$size / 1024, 1), "KB file"))

  # Read Excel file - skip first 3 rows which are header info
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read Excel files. Install it with install.packages('readxl')")
  }

  df <- readxl::read_excel(
    tname,
    skip = 3,
    col_types = "text",  # Read all as text to preserve leading zeros
    .name_repair = "unique"
  )

  message(paste("Loaded", nrow(df), "records"))

  # Clean up temp file
  if (file.exists(tname)) unlink(tname)

  # Convert to tibble for consistency
  dplyr::as_tibble(df)
}


#' Build NDE school directory download URL
#'
#' Constructs the download URL for the school directory Excel file.
#'
#' @return URL string
#' @keywords internal
build_directory_url <- function() {
  # NDE school directory download URL
  # Updated annually - check https://doe.nv.gov/school-and-district-information for current file
  "https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/school_directory_9b69a05740.xlsx"
}


#' Process raw school directory data to standard schema
#'
#' Takes raw school directory data from NDE and standardizes column names,
#' types, and adds derived columns.
#'
#' @param raw_data Raw data frame from get_raw_directory()
#' @return Processed data frame with standard schema
#' @keywords internal
process_directory <- function(raw_data) {

  cols <- names(raw_data)

  # Helper to find columns with flexible matching
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build the standardized result data frame
  n_rows <- nrow(raw_data)
  result <- dplyr::tibble(.rows = n_rows)

  # State School ID
  school_id_col <- find_col(c("^State School Code$", "^State.?School.?Code$"))
  if (!is.null(school_id_col)) {
    result$state_school_id <- trimws(raw_data[[school_id_col]])
  }

  # State District ID (Master District Code)
  dist_id_col <- find_col(c("^Master District Code$", "^Master.?District.?Code$"))
  if (!is.null(dist_id_col)) {
    result$state_district_id <- sprintf("%02s", trimws(raw_data[[dist_id_col]]))
    result$state_district_id <- gsub(" ", "0", result$state_district_id)
  }

  # School Name
  name_col <- find_col(c("^Name$", "^School.?Name$"))
  if (!is.null(name_col)) {
    result$school_name <- trimws(raw_data[[name_col]])
  }

  # District Name - not in raw data, would need lookup
  # Nevada directory doesn't include district names directly
  result$district_name <- NA_character_

  # School Type
  type_col <- find_col(c("^School Type$", "^School.?Type$"))
  if (!is.null(type_col)) {
    result$school_type <- trimws(raw_data[[type_col]])
  }

  # Grades Served - combine low and high grade

  low_grade_col <- find_col(c("^Grade Levels Offered - Low Grade$", "Low.?Grade$"))
  high_grade_col <- find_col(c("^Grade Levels Offered - High Grade$", "High.?Grade$"))
  if (!is.null(low_grade_col) && !is.null(high_grade_col)) {
    low_grade <- trimws(raw_data[[low_grade_col]])
    high_grade <- trimws(raw_data[[high_grade_col]])
    result$grades_served <- ifelse(
      low_grade == high_grade,
      low_grade,
      paste(low_grade, high_grade, sep = " - ")
    )
  }

  # Address (mailing address)
  addr_col <- find_col(c("^Address$", "^Mailing.?Address$"))
  if (!is.null(addr_col)) {
    result$address <- trimws(raw_data[[addr_col]])
  }

  # City
  city_col <- find_col(c("^City$", "^Mailing.?City$"))
  if (!is.null(city_col)) {
    result$city <- trimws(raw_data[[city_col]])
  }

  # State
  state_col <- find_col(c("^State$", "^Mailing.?State$"))
  if (!is.null(state_col)) {
    result$state <- trimws(raw_data[[state_col]])
  } else {
    result$state <- "NV"
  }

  # Zip
  zip_col <- find_col(c("^Zip$", "^Mailing.?Zip$"))
  if (!is.null(zip_col)) {
    result$zip <- trimws(raw_data[[zip_col]])
  }

  # Phone
  phone_col <- find_col(c("^Phone$", "^Telephone$"))
  if (!is.null(phone_col)) {
    result$phone <- trimws(raw_data[[phone_col]])
  }

  # Principal Name
  principal_col <- find_col(c("^Principal Name$", "^Principal.?Name$"))
  if (!is.null(principal_col)) {
    result$principal_name <- trimws(raw_data[[principal_col]])
  }

  # Principal Email
  principal_email_col <- find_col(c("^Principal Email$", "^Principal.?Email$"))
  if (!is.null(principal_email_col)) {
    result$principal_email <- trimws(raw_data[[principal_email_col]])
  }

  # Charter Status
  charter_col <- find_col(c("^Charter Status$", "^Charter.?Status$"))
  if (!is.null(charter_col)) {
    charter_vals <- trimws(raw_data[[charter_col]])
    # Standardize to YES/NO
    result$charter_status <- ifelse(
      grepl("^YES", charter_vals, ignore.case = TRUE),
      "YES",
      ifelse(grepl("^NO", charter_vals, ignore.case = TRUE), "NO", charter_vals)
    )
  }

  # Operational Status
  status_col <- find_col(c("^Operational Status$", "^Operational.?Status$"))
  if (!is.null(status_col)) {
    result$operational_status <- trimws(raw_data[[status_col]])
  }

  # Website URL
  url_col <- find_col(c("^URL$", "^Website$", "^Web$"))
  if (!is.null(url_col)) {
    result$website <- trimws(raw_data[[url_col]])
  }

  # Physical address (if different from mailing)
  phys_addr_col <- find_col(c("^Physical Address$", "^Physical.?Address$"))
  if (!is.null(phys_addr_col)) {
    result$physical_address <- trimws(raw_data[[phys_addr_col]])
  }

  phys_city_col <- find_col(c("^Physical City$", "^Physical.?City$"))
  if (!is.null(phys_city_col)) {
    result$physical_city <- trimws(raw_data[[phys_city_col]])
  }

  phys_state_col <- find_col(c("^Physical State$", "^Physical.?State$"))
  if (!is.null(phys_state_col)) {
    result$physical_state <- trimws(raw_data[[phys_state_col]])
  }

  phys_zip_col <- find_col(c("^Physical Zip$", "^Physical.?Zip$"))
  if (!is.null(phys_zip_col)) {
    result$physical_zip <- trimws(raw_data[[phys_zip_col]])
  }

  # Title 1 Status
  title1_col <- find_col(c("^Title 1$", "^Title.?1$"))
  if (!is.null(title1_col)) {
    result$title_1 <- trimws(raw_data[[title1_col]])
  }

  # School Level (Elementary, Middle, High, etc.)
  level_col <- find_col(c("^School Level$", "^School.?Level$"))
  if (!is.null(level_col)) {
    result$school_level <- trimws(raw_data[[level_col]])
  }

  # Locale (urban, suburban, rural, etc.)
  locale_col <- find_col(c("^Locale$"))
  if (!is.null(locale_col)) {
    result$locale <- trimws(raw_data[[locale_col]])
  }

  # Magnet Status
  magnet_col <- find_col(c("^Magnet Status$", "^Magnet.?Status$"))
  if (!is.null(magnet_col)) {
    result$magnet_status <- trimws(raw_data[[magnet_col]])
  }

  # Virtual Status
  virtual_col <- find_col(c("^Virtual$"))
  if (!is.null(virtual_col)) {
    result$virtual <- trimws(raw_data[[virtual_col]])
  }

  # Reorder columns for consistency
  preferred_order <- c(
    "state_school_id", "state_district_id",
    "school_name", "district_name",
    "school_type", "school_level", "grades_served",
    "address", "city", "state", "zip",
    "physical_address", "physical_city", "physical_state", "physical_zip",
    "phone",
    "principal_name", "principal_email",
    "charter_status", "operational_status",
    "website",
    "title_1", "locale", "magnet_status", "virtual"
  )

  existing_cols <- preferred_order[preferred_order %in% names(result)]
  other_cols <- setdiff(names(result), preferred_order)

  result <- result |>
    dplyr::select(dplyr::all_of(c(existing_cols, other_cols)))

  result
}


# ==============================================================================
# Directory-specific cache functions
# ==============================================================================

#' Build cache file path for directory data
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return File path string
#' @keywords internal
build_cache_path_directory <- function(cache_type) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, paste0(cache_type, ".rds"))
}


#' Check if cached directory data exists
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @param max_age Maximum age in days (default 30). Set to Inf to ignore age.
#' @return Logical indicating if valid cache exists
#' @keywords internal
cache_exists_directory <- function(cache_type, max_age = 30) {
  cache_path <- build_cache_path_directory(cache_type)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  # Check age
  file_info <- file.info(cache_path)
  age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  age_days <= max_age
}


#' Read directory data from cache
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return Cached data frame
#' @keywords internal
read_cache_directory <- function(cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  readRDS(cache_path)
}


#' Write directory data to cache
#'
#' @param data Data frame to cache
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return Invisibly returns the cache path
#' @keywords internal
write_cache_directory <- function(data, cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  cache_dir <- dirname(cache_path)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  saveRDS(data, cache_path)
  invisible(cache_path)
}


#' Clear school directory cache
#'
#' Removes cached school directory data files.
#'
#' @return Invisibly returns the number of files removed
#' @export
#' @examples
#' \dontrun{
#' # Clear cached directory data
#' clear_directory_cache()
#' }
clear_directory_cache <- function() {
  cache_dir <- get_cache_dir()

  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist")
    return(invisible(0))
  }

  files <- list.files(cache_dir, pattern = "^directory_", full.names = TRUE)

  if (length(files) > 0) {
    file.remove(files)
    message(paste("Removed", length(files), "cached directory file(s)"))
  } else {
    message("No cached directory files to remove")
  }

  invisible(length(files))
}
