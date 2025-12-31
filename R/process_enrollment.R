# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw NDE enrollment data into a
# clean, standardized format.
#
# Nevada uses several identifier systems:
# - Local Education Agency (LEA) Code: District identifier
# - Master District Code: Alternative district identifier
# - School Code: School identifier within a district
#
# ==============================================================================

#' Convert to numeric, handling suppression markers
#'
#' NDE uses various markers for suppressed data (*, <10, -, etc.)
#' and may use commas in large numbers.
#'
#' @param x Vector to convert
#' @return Numeric vector with NA for non-numeric values
#' @keywords internal
safe_numeric <- function(x) {
  if (is.numeric(x)) return(x)

  # Remove commas and whitespace
  x <- gsub(",", "", x)
  x <- trimws(x)

  # Handle common suppression markers
  x <- gsub("^\\*$", NA_character_, x)
  x <- gsub("^<.*$", NA_character_, x)
  x <- gsub("^N/A$", NA_character_, x, ignore.case = TRUE)
  x <- gsub("^-$", NA_character_, x)
  x <- gsub("^\\s*$", NA_character_, x)

  suppressWarnings(as.numeric(x))
}


#' Process raw NDE enrollment data
#'
#' Transforms raw NDE enrollment data into a standardized format with
#' consistent column names and data types.
#'
#' @param raw_list Raw data list from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_list, end_year) {

  format_type <- raw_list$format

  switch(format_type,
    "modern" = process_enr_modern(raw_list, end_year),
    "intermediate" = process_enr_intermediate(raw_list, end_year),
    "legacy" = process_enr_legacy(raw_list, end_year)
  )
}


#' Process modern format NDE data (2021+)
#'
#' Processes enrollment data from NDE with LEA codes and full structure.
#'
#' @param raw_list Raw data list
#' @param end_year School year end
#' @return Processed data frame
#' @keywords internal
process_enr_modern <- function(raw_list, end_year) {

  results <- list()

  # Process school level totals
  if (!is.null(raw_list$school_totals)) {
    df <- raw_list$school_totals
    results$school_totals <- process_school_totals(df, end_year)
  }

  # Process school grade data
  if (!is.null(raw_list$school_grades)) {
    df <- raw_list$school_grades
    results$school_grades <- process_school_grades(df, end_year)
  }

  # Process district data
  if (!is.null(raw_list$district_data)) {
    df <- raw_list$district_data
    results$district_data <- process_district_data(df, end_year)
  }

  # Combine results
  if (length(results) > 0) {
    dplyr::bind_rows(results)
  } else {
    data.frame()
  }
}


#' Process school totals sheet
#'
#' @param df Raw data frame
#' @param end_year School year end
#' @return Processed data frame
#' @keywords internal
process_school_totals <- function(df, end_year) {

  cols <- names(df)

  # Helper to find column by pattern
  find_col <- function(pattern) {
    matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
    if (length(matched) > 0) matched[1] else NULL
  }

  result <- data.frame(
    end_year = rep(end_year, nrow(df)),
    stringsAsFactors = FALSE
  )

  # === IDENTIFIERS ===

  # LEA Code (district)
  lea_col <- find_col("^Local.*Agency.*Code$|^LEA.*Code$")
  if (!is.null(lea_col)) {
    result$lea_code <- sprintf("%02d", as.integer(df[[lea_col]]))
  }

  # LEA Name
  lea_name_col <- find_col("^Local.*Agency.*Name$|^LEA.*Name$")
  if (!is.null(lea_name_col)) {
    result$lea_name <- trimws(df[[lea_name_col]])
  }

  # Master District Code
  master_code_col <- find_col("^Master.*District.*Code$")
  if (!is.null(master_code_col)) {
    result$district_code <- sprintf("%02d", as.integer(df[[master_code_col]]))
  }

  # Master District Name
  master_name_col <- find_col("^Master.*District.*Name$")
  if (!is.null(master_name_col)) {
    result$district_name <- trimws(df[[master_name_col]])
  }

  # School Code
  school_code_col <- find_col("^School.*Code$")
  if (!is.null(school_code_col)) {
    result$school_code <- sprintf("%04d", as.integer(df[[school_code_col]]))
  }

  # School Name
  school_name_col <- find_col("^School.*Name$")
  if (!is.null(school_name_col)) {
    result$school_name <- trimws(df[[school_name_col]])
  }

  result$entity_type <- "School"
  result$grade_level <- "TOTAL"

  # === ENROLLMENT TOTALS ===

  total_col <- find_col("^Total$|^Grand.*Total$")
  if (!is.null(total_col)) {
    result$enrollment_total <- safe_numeric(df[[total_col]])
  }

  # === GENDER ===

  female_col <- find_col("^Female$")
  if (!is.null(female_col)) {
    result$female <- safe_numeric(df[[female_col]])
  }

  male_col <- find_col("^Male$")
  if (!is.null(male_col)) {
    result$male <- safe_numeric(df[[male_col]])
  }

  # === DEMOGRAPHICS - Race/Ethnicity ===

  demo_map <- list(
    "white" = "^White$",
    "black" = "^Black|African.*American$",
    "hispanic" = "^Hispanic|Latino$",
    "asian" = "^Asian$",
    "pacific_islander" = "^Native.*Hawaiian|Pacific.*Islander$",
    "native_american" = "^American.*Indian|Alaska.*Native$",
    "multiracial" = "^Two.*more.*races$|^Multi"
  )

  for (new_name in names(demo_map)) {
    pattern <- demo_map[[new_name]]
    demo_col <- find_col(pattern)
    if (!is.null(demo_col)) {
      result[[new_name]] <- safe_numeric(df[[demo_col]])
    }
  }

  # === SPECIAL POPULATIONS ===

  special_map <- list(
    "frl" = "^FRL$",
    "iep" = "^IEP$",
    "el" = "^EL$",
    "migrant" = "^MIG$|^Migrant$",
    "foster" = "^Foster$",
    "military" = "^Military$",
    "homeless" = "^Homeless$"
  )

  for (new_name in names(special_map)) {
    pattern <- special_map[[new_name]]
    special_col <- find_col(pattern)
    if (!is.null(special_col)) {
      result[[new_name]] <- safe_numeric(df[[special_col]])
    }
  }

  result
}


#' Process school grade data sheet
#'
#' @param df Raw data frame
#' @param end_year School year end
#' @return Processed data frame
#' @keywords internal
process_school_grades <- function(df, end_year) {

  cols <- names(df)

  find_col <- function(pattern) {
    matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
    if (length(matched) > 0) matched[1] else NULL
  }

  result <- data.frame(
    end_year = rep(end_year, nrow(df)),
    stringsAsFactors = FALSE
  )

  # === IDENTIFIERS ===

  lea_col <- find_col("^Local.*Agency.*Code$|^LEA.*Code$")
  if (!is.null(lea_col)) {
    result$lea_code <- sprintf("%02d", as.integer(df[[lea_col]]))
  }

  lea_name_col <- find_col("^Local.*Agency.*Name$|^LEA.*Name$")
  if (!is.null(lea_name_col)) {
    result$lea_name <- trimws(df[[lea_name_col]])
  }

  master_code_col <- find_col("^Master.*District.*Code$")
  if (!is.null(master_code_col)) {
    result$district_code <- sprintf("%02d", as.integer(df[[master_code_col]]))
  }

  master_name_col <- find_col("^Master.*District.*Name$")
  if (!is.null(master_name_col)) {
    result$district_name <- trimws(df[[master_name_col]])
  }

  school_code_col <- find_col("^School.*Code$")
  if (!is.null(school_code_col)) {
    result$school_code <- sprintf("%04d", as.integer(df[[school_code_col]]))
  }

  school_name_col <- find_col("^School.*Name$")
  if (!is.null(school_name_col)) {
    result$school_name <- trimws(df[[school_name_col]])
  }

  result$entity_type <- "School"

  # Grade level
  grade_col <- find_col("^Grade.*Level$|^Grade$")
  if (!is.null(grade_col)) {
    result$grade_level <- standardize_grade(df[[grade_col]])
  }

  # Enrollment total
  total_col <- find_col("^Total$")
  if (!is.null(total_col)) {
    result$enrollment_total <- safe_numeric(df[[total_col]])
  }

  # Gender
  female_col <- find_col("^Female$")
  if (!is.null(female_col)) {
    result$female <- safe_numeric(df[[female_col]])
  }

  male_col <- find_col("^Male$")
  if (!is.null(male_col)) {
    result$male <- safe_numeric(df[[male_col]])
  }

  # Demographics
  demo_map <- list(
    "white" = "^White$",
    "black" = "^Black|African.*American$",
    "hispanic" = "^Hispanic|Latino$",
    "asian" = "^Asian$",
    "pacific_islander" = "^Native.*Hawaiian|Pacific.*Islander$",
    "native_american" = "^American.*Indian|Alaska.*Native$",
    "multiracial" = "^Two.*more.*races$|^Multi"
  )

  for (new_name in names(demo_map)) {
    pattern <- demo_map[[new_name]]
    demo_col <- find_col(pattern)
    if (!is.null(demo_col)) {
      result[[new_name]] <- safe_numeric(df[[demo_col]])
    }
  }

  result
}


#' Process district data sheet
#'
#' @param df Raw data frame
#' @param end_year School year end
#' @return Processed data frame
#' @keywords internal
process_district_data <- function(df, end_year) {

  cols <- names(df)

  find_col <- function(pattern) {
    matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
    if (length(matched) > 0) matched[1] else NULL
  }

  result <- data.frame(
    end_year = rep(end_year, nrow(df)),
    stringsAsFactors = FALSE
  )

  # === IDENTIFIERS ===

  lea_col <- find_col("^Local.*Agency.*Code$|^LEA.*Code$")
  if (!is.null(lea_col)) {
    result$lea_code <- sprintf("%02d", as.integer(df[[lea_col]]))
  }

  lea_name_col <- find_col("^Local.*Agency.*Name$|^LEA.*Name$")
  if (!is.null(lea_name_col)) {
    result$lea_name <- trimws(df[[lea_name_col]])
  }

  master_code_col <- find_col("^Master.*District.*Code$")
  if (!is.null(master_code_col)) {
    result$district_code <- sprintf("%02d", as.integer(df[[master_code_col]]))
  }

  master_name_col <- find_col("^Master.*District.*Name$")
  if (!is.null(master_name_col)) {
    result$district_name <- trimws(df[[master_name_col]])
  }

  result$school_code <- NA_character_
  result$school_name <- NA_character_
  result$entity_type <- "District"

  # Grade level (if present)
  grade_col <- find_col("^Grade.*Level$|^Grade$")
  if (!is.null(grade_col)) {
    result$grade_level <- standardize_grade(df[[grade_col]])
  } else {
    result$grade_level <- "TOTAL"
  }

  # Enrollment
  total_col <- find_col("^Total$|^Totals$")
  if (!is.null(total_col)) {
    result$enrollment_total <- safe_numeric(df[[total_col]])
  }

  # Gender
  female_col <- find_col("^Female$")
  if (!is.null(female_col)) {
    result$female <- safe_numeric(df[[female_col]])
  }

  male_col <- find_col("^Male$")
  if (!is.null(male_col)) {
    result$male <- safe_numeric(df[[male_col]])
  }

  # Demographics
  demo_map <- list(
    "white" = "^White$",
    "black" = "^Black|African.*American$",
    "hispanic" = "^Hispanic|Latino$",
    "asian" = "^Asian$",
    "pacific_islander" = "^Native.*Hawaiian|Pacific.*Islander$",
    "native_american" = "^American.*Indian|Alaska.*Native$",
    "multiracial" = "^Two.*more.*races$|^Multi"
  )

  for (new_name in names(demo_map)) {
    pattern <- demo_map[[new_name]]
    demo_col <- find_col(pattern)
    if (!is.null(demo_col)) {
      result[[new_name]] <- safe_numeric(df[[demo_col]])
    }
  }

  # Special populations
  special_map <- list(
    "frl" = "^FRL$",
    "iep" = "^IEP$",
    "el" = "^EL$",
    "migrant" = "^MIG$|^Migrant$",
    "foster" = "^Foster$",
    "military" = "^Military$",
    "homeless" = "^Homeless$"
  )

  for (new_name in names(special_map)) {
    pattern <- special_map[[new_name]]
    special_col <- find_col(pattern)
    if (!is.null(special_col)) {
      result[[new_name]] <- safe_numeric(df[[special_col]])
    }
  }

  result
}


#' Standardize grade level codes
#'
#' Converts various grade level formats to standard codes.
#'
#' @param grades Vector of grade levels
#' @return Standardized grade codes
#' @keywords internal
standardize_grade <- function(grades) {
  grades <- toupper(trimws(as.character(grades)))

  # Map common variations
  grades <- gsub("^KG$|^KINDERGARTEN$|^0K$|^K$", "K", grades)
  grades <- gsub("^PK$|^PRE-K$|^PREK$", "PK", grades)
  grades <- gsub("^UG$|^UNGRADED$", "UG", grades)
  grades <- gsub("^AD$|^ADULT$", "AD", grades)
  grades <- gsub("^13$", "UG", grades)  # Nevada uses 13 for ungraded sometimes

  # Pad single digit grades (1-9)
  grades <- gsub("^([1-9])$", "0\\1", grades)

  grades
}


#' Process intermediate format NDE data (2018-2020)
#'
#' @param raw_list Raw data list
#' @param end_year School year end
#' @return Processed data frame
#' @keywords internal
process_enr_intermediate <- function(raw_list, end_year) {

  results <- list()

  # Process school grade data
  if (!is.null(raw_list$school_grades)) {
    df <- raw_list$school_grades
    cols <- names(df)

    find_col <- function(pattern) {
      matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) matched[1] else NULL
    }

    result <- data.frame(
      end_year = rep(end_year, nrow(df)),
      stringsAsFactors = FALSE
    )

    # Identifiers vary by year
    lea_col <- find_col("^Local.*Agency.*Code$|^LEA")
    if (!is.null(lea_col)) {
      result$lea_code <- sprintf("%02d", as.integer(df[[lea_col]]))
    }

    dist_code_col <- find_col("^Master.*District.*Code$|^District.*Code$")
    if (!is.null(dist_code_col)) {
      result$district_code <- sprintf("%02d", as.integer(df[[dist_code_col]]))
    }

    dist_name_col <- find_col("^Master.*District.*Name$|^District$")
    if (!is.null(dist_name_col)) {
      result$district_name <- trimws(df[[dist_name_col]])
    }

    school_code_col <- find_col("^School.*Code$")
    if (!is.null(school_code_col)) {
      result$school_code <- sprintf("%04d", as.integer(df[[school_code_col]]))
    }

    school_name_col <- find_col("^School.*Name$")
    if (!is.null(school_name_col)) {
      result$school_name <- trimws(df[[school_name_col]])
    }

    result$entity_type <- "School"

    grade_col <- find_col("^Grade$")
    if (!is.null(grade_col)) {
      result$grade_level <- standardize_grade(df[[grade_col]])
    }

    total_col <- find_col("^Total$|^Totals$")
    if (!is.null(total_col)) {
      result$enrollment_total <- safe_numeric(df[[total_col]])
    }

    # Gender
    female_col <- find_col("^Female$")
    if (!is.null(female_col)) {
      result$female <- safe_numeric(df[[female_col]])
    }

    male_col <- find_col("^Male$")
    if (!is.null(male_col)) {
      result$male <- safe_numeric(df[[male_col]])
    }

    # Demographics
    demo_cols <- list(
      "white" = "^White$",
      "black" = "^Black$",
      "hispanic" = "^Hispanic$",
      "asian" = "^Asian$",
      "pacific_islander" = "^Native.*Hawaiian|Pacific",
      "native_american" = "^American.*Indian|Alaska",
      "multiracial" = "^Two.*more|Multi"
    )

    for (new_name in names(demo_cols)) {
      pattern <- demo_cols[[new_name]]
      demo_col <- find_col(pattern)
      if (!is.null(demo_col)) {
        result[[new_name]] <- safe_numeric(df[[demo_col]])
      }
    }

    results$school_grades <- result
  }

  # Process district data
  if (!is.null(raw_list$district_data)) {
    df <- raw_list$district_data
    cols <- names(df)

    find_col <- function(pattern) {
      matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) matched[1] else NULL
    }

    result <- data.frame(
      end_year = rep(end_year, nrow(df)),
      stringsAsFactors = FALSE
    )

    lea_col <- find_col("^Local.*Agency.*Code$|^LEA|^District.*Code$")
    if (!is.null(lea_col)) {
      result$lea_code <- sprintf("%02d", as.integer(df[[lea_col]]))
    }

    dist_code_col <- find_col("^Master.*District.*Code$")
    if (!is.null(dist_code_col)) {
      result$district_code <- sprintf("%02d", as.integer(df[[dist_code_col]]))
    }

    dist_name_col <- find_col("^Local.*Agency.*Name$|^Master.*District.*Name$|^District$")
    if (!is.null(dist_name_col)) {
      result$district_name <- trimws(df[[dist_name_col]])
    }

    result$school_code <- NA_character_
    result$school_name <- NA_character_
    result$entity_type <- "District"
    result$grade_level <- "TOTAL"

    total_col <- find_col("^Total$|^Totals$")
    if (!is.null(total_col)) {
      result$enrollment_total <- safe_numeric(df[[total_col]])
    }

    # Gender
    female_col <- find_col("^Female$")
    if (!is.null(female_col)) {
      result$female <- safe_numeric(df[[female_col]])
    }

    male_col <- find_col("^Male$")
    if (!is.null(male_col)) {
      result$male <- safe_numeric(df[[male_col]])
    }

    # Demographics
    demo_cols <- list(
      "white" = "^White$",
      "black" = "^Black$",
      "hispanic" = "^Hispanic$",
      "asian" = "^Asian$",
      "pacific_islander" = "^Native.*Hawaiian|Pacific",
      "native_american" = "^American.*Indian|Alaska",
      "multiracial" = "^Two.*more|Multi"
    )

    for (new_name in names(demo_cols)) {
      pattern <- demo_cols[[new_name]]
      demo_col <- find_col(pattern)
      if (!is.null(demo_col)) {
        result[[new_name]] <- safe_numeric(df[[demo_col]])
      }
    }

    results$district_data <- result
  }

  if (length(results) > 0) {
    dplyr::bind_rows(results)
  } else {
    data.frame()
  }
}


#' Process legacy format NDE data (2016-2017)
#'
#' @param raw_list Raw data list
#' @param end_year School year end
#' @return Processed data frame
#' @keywords internal
process_enr_legacy <- function(raw_list, end_year) {

  # Legacy format has limited consolidated data
  # Return district-level data if available

  if (!is.null(raw_list$district_data)) {
    df <- raw_list$district_data
    cols <- names(df)

    # Legacy format columns are different
    # District_Race_Ethnicity_Grade sheet has columns like:
    # District Name, Gender (F/M rows), Ethnicity rows, Grade columns

    # This format would require more complex parsing
    # For now, return a simplified version

    warning(paste(
      "Legacy format (", end_year, ") has limited structure.",
      "Consider using modern format years (2021+) for full data."
    ))

    data.frame(
      end_year = end_year,
      entity_type = "State",
      grade_level = "TOTAL",
      enrollment_total = NA_real_,
      note = "Legacy format - limited data extraction"
    )
  } else {
    data.frame()
  }
}
