# ==============================================================================
# Exhaustive Directory Tests for nvschooldata
# ==============================================================================
#
# Tests fetch_directory() and related functions with all parameter combinations.
# All expected values are pinned from real Nevada DOE data.
#
# ==============================================================================

library(testthat)
library(dplyr)

# ==============================================================================
# fetch_directory() — tidy format (default)
# ==============================================================================

test_that("fetch_directory tidy returns correct dimensions", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  expect_true(is.data.frame(dir_data))
  expect_equal(nrow(dir_data), 800)
  expect_equal(ncol(dir_data), 25)
})

test_that("fetch_directory tidy is a tibble", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  expect_s3_class(dir_data, "tbl_df")
})

test_that("fetch_directory tidy has correct column names", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  expected_cols <- c(
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
  expect_equal(names(dir_data), expected_cols)
})

test_that("fetch_directory tidy all columns are character type", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  for (col in names(dir_data)) {
    expect_type(dir_data[[col]], "character")
  }
})

test_that("fetch_directory tidy state column is always NV", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  expect_equal(unique(dir_data$state), "NV")
})


# ==============================================================================
# fetch_directory() — school_type values
# ==============================================================================

test_that("fetch_directory has correct school_type values", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  school_types <- sort(unique(dir_data$school_type))
  expected_types <- sort(c(
    "AD: Adult", "AL: Alternative", "CO: Correctional",
    "CTE: Career and Technical", "EL: Early Learning",
    "JVC: Juvenile Correctional", "RG: Regular",
    "RP: Reportable Program", "SE: Special Education"
  ))
  expect_equal(school_types, expected_types)
})


# ==============================================================================
# fetch_directory() — school_level values
# ==============================================================================

test_that("fetch_directory has correct school_level values", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  school_levels <- sort(unique(dir_data$school_level))
  expected_levels <- sort(c(
    "AD: Adult HS", "ES: Elementary School",
    "HS: High School", "MS: Middle School",
    "OC: Other Combination", "PK: PreK"
  ))
  expect_equal(school_levels, expected_levels)
})


# ==============================================================================
# fetch_directory() — charter_status values
# ==============================================================================

test_that("fetch_directory has correct charter_status values", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  charter_vals <- sort(unique(dir_data$charter_status))
  expected_vals <- sort(c("DCH: District Charter", "NO", "SSC: SPCSA"))
  expect_equal(charter_vals, expected_vals)
})


# ==============================================================================
# fetch_directory() — operational_status values
# ==============================================================================

test_that("fetch_directory has correct operational_status values", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  op_vals <- sort(unique(dir_data$operational_status))
  expected_vals <- sort(c("Inactive", "Open"))
  expect_equal(op_vals, expected_vals)
})


# ==============================================================================
# fetch_directory() — title_1 values
# ==============================================================================

test_that("fetch_directory has correct title_1 values", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  title1_vals <- sort(unique(dir_data$title_1))
  expected_vals <- sort(c(
    "NOTTITLE1ELIG: Not a Title I School",
    "SWELIGNOPROG: Schoolwide Eligible - No Program",
    "SWELIGSWPROG: Schoolwide Program"
  ))
  expect_equal(title1_vals, expected_vals)
})


# ==============================================================================
# fetch_directory() — locale values
# ==============================================================================

test_that("fetch_directory has correct locale values", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  locale_vals <- sort(unique(dir_data$locale))
  expected_vals <- sort(c(
    "01: Rural - Distant", "02: Town - Fringe",
    "03: City - Midsize", "04: City - Large",
    "05: Rural - Remote", "06: Rural - Fringe",
    "07: Town - Distant", "08: Suburb - Large",
    "09: City - Small", "10: Town - Remote", "N: No"
  ))
  expect_equal(locale_vals, expected_vals)
})


# ==============================================================================
# fetch_directory() — virtual values
# ==============================================================================

test_that("fetch_directory has correct virtual values", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  virtual_vals <- sort(unique(dir_data$virtual))
  expected_vals <- sort(c(
    "01: Exclusively virtual",
    "02: Primarily virtual",
    "03: Supplemental virtual",
    "04: No virtual instruction"
  ))
  expect_equal(virtual_vals, expected_vals)
})


# ==============================================================================
# fetch_directory() — magnet_status values
# ==============================================================================

test_that("fetch_directory has correct magnet_status values", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  magnet_vals <- sort(unique(dir_data$magnet_status))
  expected_vals <- sort(c("MAGNO: Magnet No", "MAGYES: Magnet Yes"))
  expect_equal(magnet_vals, expected_vals)
})


# ==============================================================================
# fetch_directory() — district count
# ==============================================================================

test_that("fetch_directory has correct number of unique districts", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  n_districts <- length(unique(dir_data$state_district_id))
  expect_equal(n_districts, 71)
})


# ==============================================================================
# fetch_directory() — raw format (tidy=FALSE)
# ==============================================================================

test_that("fetch_directory raw format returns more columns", {
  dir_raw <- nvschooldata::fetch_directory(tidy = FALSE, use_cache = TRUE)

  expect_true(is.data.frame(dir_raw))
  expect_s3_class(dir_raw, "tbl_df")
  expect_equal(nrow(dir_raw), 800)
  expect_equal(ncol(dir_raw), 54)
})

test_that("fetch_directory raw format has original NDE column names", {
  dir_raw <- nvschooldata::fetch_directory(tidy = FALSE, use_cache = TRUE)

  # Check some expected raw column names
  expect_true("Master District Code" %in% names(dir_raw))
  expect_true("State School Code" %in% names(dir_raw))
  expect_true("Name" %in% names(dir_raw))
  expect_true("School Type" %in% names(dir_raw))
  expect_true("Charter Status" %in% names(dir_raw))
  expect_true("Operational Status" %in% names(dir_raw))
  expect_true("Phone" %in% names(dir_raw))
})

test_that("fetch_directory raw and tidy have same row count", {
  dir_tidy <- nvschooldata::fetch_directory(tidy = TRUE, use_cache = TRUE)
  dir_raw <- nvschooldata::fetch_directory(tidy = FALSE, use_cache = TRUE)

  expect_equal(nrow(dir_tidy), nrow(dir_raw))
})


# ==============================================================================
# fetch_directory() — end_year parameter (unused but accepted)
# ==============================================================================

test_that("fetch_directory accepts end_year parameter without error", {
  dir_data <- nvschooldata::fetch_directory(end_year = 2026, use_cache = TRUE)

  expect_true(is.data.frame(dir_data))
  expect_equal(nrow(dir_data), 800)
})

test_that("fetch_directory result same with or without end_year", {
  dir_default <- nvschooldata::fetch_directory(use_cache = TRUE)
  dir_with_year <- nvschooldata::fetch_directory(end_year = 2024, use_cache = TRUE)

  expect_equal(nrow(dir_default), nrow(dir_with_year))
  expect_equal(names(dir_default), names(dir_with_year))
})


# ==============================================================================
# clear_directory_cache()
# ==============================================================================

test_that("clear_directory_cache returns invisibly", {
  # This test just verifies the function runs without error
  result <- nvschooldata::clear_directory_cache()

  # Result should be numeric (count of removed files)
  expect_true(is.numeric(result))
})


# ==============================================================================
# Directory data quality checks
# ==============================================================================

test_that("fetch_directory has no completely empty required columns", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  required_cols <- c(
    "state_school_id", "state_district_id", "school_name",
    "school_type", "charter_status", "operational_status"
  )

  for (col in required_cols) {
    non_na <- sum(!is.na(dir_data[[col]]) & dir_data[[col]] != "")
    expect_gt(non_na, 0,
              label = paste("Column", col, "should have non-empty values"))
  }
})

test_that("fetch_directory school_name has no empty values", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  expect_true(all(!is.na(dir_data$school_name)))
  expect_true(all(nchar(dir_data$school_name) > 0))
})

test_that("fetch_directory city column has Las Vegas as most common", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)

  city_counts <- sort(table(dir_data$city), decreasing = TRUE)
  expect_equal(names(city_counts)[1], "Las Vegas")
})
