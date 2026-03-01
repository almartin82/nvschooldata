# ==============================================================================
# Exhaustive Typology & Data Quality Tests for nvschooldata
# ==============================================================================
#
# Tests data structure, column types, naming standards, edge cases,
# fidelity checks, and cache functions.
#
# ==============================================================================

library(testthat)
library(dplyr)

# ==============================================================================
# Naming Standards — subgroup names (cross-state consistency)
# ==============================================================================

test_that("tidy enrollment uses standard subgroup names", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  subgroups <- unique(tidy26$subgroup)

  # Standard names MUST be present (modern format)
  expect_true("total_enrollment" %in% subgroups)
  expect_true("white" %in% subgroups)
  expect_true("black" %in% subgroups)
  expect_true("hispanic" %in% subgroups)
  expect_true("asian" %in% subgroups)
  expect_true("native_american" %in% subgroups)
  expect_true("pacific_islander" %in% subgroups)
  expect_true("multiracial" %in% subgroups)
  expect_true("female" %in% subgroups)
  expect_true("male" %in% subgroups)
  expect_true("free_reduced_lunch" %in% subgroups)
  expect_true("special_ed" %in% subgroups)
  expect_true("lep" %in% subgroups)

  # Non-standard names MUST NOT be present
  expect_false("total" %in% subgroups)
  expect_false("low_income" %in% subgroups)
  expect_false("economically_disadvantaged" %in% subgroups)
  expect_false("frl" %in% subgroups)
  expect_false("iep" %in% subgroups)
  expect_false("disability" %in% subgroups)
  expect_false("el" %in% subgroups)
  expect_false("ell" %in% subgroups)
  expect_false("english_learner" %in% subgroups)
  expect_false("american_indian" %in% subgroups)
  expect_false("two_or_more" %in% subgroups)
})


# ==============================================================================
# Naming Standards — grade levels (UPPERCASE, standardized)
# ==============================================================================

test_that("grade levels are all uppercase and standardized", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  grade_levels <- unique(tidy26$grade_level)

  # All must be uppercase
  expect_true(all(grade_levels == toupper(grade_levels)))

  # Standard names present
  expect_true("PK" %in% grade_levels)
  expect_true("K" %in% grade_levels)
  expect_true("01" %in% grade_levels)
  expect_true("12" %in% grade_levels)
  expect_true("TOTAL" %in% grade_levels)

  # Non-standard names absent
  expect_false("KG" %in% grade_levels)
  expect_false("KINDERGARTEN" %in% grade_levels)
  expect_false("0K" %in% grade_levels)
  expect_false("PRE-K" %in% grade_levels)
  expect_false("PREK" %in% grade_levels)
  expect_false("UNGRADED" %in% grade_levels)
  expect_false("13" %in% grade_levels)  # Nevada sometimes uses 13 for ungraded

  # Numeric grades are zero-padded
  for (g in 1:9) {
    if (sprintf("%02d", g) %in% grade_levels) {
      expect_false(as.character(g) %in% grade_levels,
                   label = paste("Grade", g, "should be zero-padded"))
    }
  }
})


# ==============================================================================
# Naming Standards — entity flags
# ==============================================================================

test_that("entity flags follow naming standards", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  expect_true("is_state" %in% names(tidy26))
  expect_true("is_district" %in% names(tidy26))
  expect_true("is_school" %in% names(tidy26))
  expect_true("is_charter" %in% names(tidy26))
})

test_that("entity flags are mutually exclusive for state/district/school", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  # No row should be both district and school
  overlap <- tidy26 %>%
    filter(is_district & is_school)
  expect_equal(nrow(overlap), 0)

  # No row should be both state and district
  overlap2 <- tidy26 %>%
    filter(is_state & is_district)
  expect_equal(nrow(overlap2), 0)

  # No row should be both state and school
  overlap3 <- tidy26 %>%
    filter(is_state & is_school)
  expect_equal(nrow(overlap3), 0)
})

test_that("entity_type matches boolean flags", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  dist_rows <- tidy26 %>% filter(entity_type == "District")
  expect_true(all(dist_rows$is_district))
  expect_true(all(!dist_rows$is_school))

  school_rows <- tidy26 %>% filter(entity_type == "School")
  expect_true(all(school_rows$is_school))
  expect_true(all(!school_rows$is_district))
})


# ==============================================================================
# Data Quality — no Inf, NaN, or negative enrollment
# ==============================================================================

test_that("no Inf values in tidy enrollment", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  expect_false(any(is.infinite(tidy26$n_students), na.rm = TRUE))
  expect_false(any(is.infinite(tidy26$pct), na.rm = TRUE))
})

test_that("no NaN values in tidy enrollment", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  expect_false(any(is.nan(tidy26$n_students), na.rm = TRUE))
  expect_false(any(is.nan(tidy26$pct), na.rm = TRUE))
})

test_that("no negative enrollment counts", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  expect_true(all(tidy26$n_students >= 0, na.rm = TRUE))
})

test_that("no Inf or NaN in wide format", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)

  numeric_cols <- names(wide26)[sapply(wide26, is.numeric)]
  for (col in numeric_cols) {
    expect_false(any(is.infinite(wide26[[col]]), na.rm = TRUE),
                 label = paste("No Inf in", col))
    expect_false(any(is.nan(wide26[[col]]), na.rm = TRUE),
                 label = paste("No NaN in", col))
  }
})

test_that("no negative values in wide format numeric columns", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)

  count_cols <- c("enrollment_total", "female", "male", "white", "black",
                  "hispanic", "asian", "pacific_islander", "native_american",
                  "multiracial")
  for (col in count_cols) {
    expect_true(all(wide26[[col]] >= 0, na.rm = TRUE),
                label = paste("No negatives in", col))
  }
})


# ==============================================================================
# Data Quality — n_students column type
# ==============================================================================

test_that("n_students is numeric in tidy format", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  expect_type(tidy26$n_students, "double")
})

test_that("pct is numeric in tidy format", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  expect_type(tidy26$pct, "double")
})


# ==============================================================================
# Fidelity — wide and tidy totals match exactly
# ==============================================================================

test_that("fidelity: wide and tidy enrollment_total sums match for 2026", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  wide_sum <- sum(wide26$enrollment_total, na.rm = TRUE)
  tidy_sum <- tidy26 %>%
    filter(subgroup == "total_enrollment") %>%
    summarize(s = sum(n_students, na.rm = TRUE)) %>%
    pull(s)

  expect_equal(wide_sum, tidy_sum)
})

test_that("fidelity: district-level wide and tidy totals match for 2026", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  wide_dist <- wide26 %>%
    filter(entity_type == "District", grade_level == "TOTAL") %>%
    summarize(s = sum(enrollment_total, na.rm = TRUE)) %>%
    pull(s)

  tidy_dist <- tidy26 %>%
    filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    summarize(s = sum(n_students, na.rm = TRUE)) %>%
    pull(s)

  expect_equal(wide_dist, tidy_dist)
})

test_that("fidelity: wide demographics sum to enrollment_total per row", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)

  demo_cols <- c("white", "black", "hispanic", "asian",
                 "pacific_islander", "native_american", "multiracial")
  demo_cols <- demo_cols[demo_cols %in% names(wide26)]

  # For rows where all demographic cols are present (non-NA)
  complete_rows <- wide26 %>%
    filter(!is.na(enrollment_total)) %>%
    filter(if_all(all_of(demo_cols), ~ !is.na(.)))

  if (nrow(complete_rows) > 0) {
    demo_sum <- rowSums(complete_rows[, demo_cols], na.rm = TRUE)
    # Demographics should sum to <= enrollment_total (some may have unknown race)
    expect_true(all(demo_sum <= complete_rows$enrollment_total + 1))  # +1 for rounding
  }
})

test_that("fidelity: wide gender sum roughly equals enrollment_total", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)

  gender_rows <- wide26 %>%
    filter(!is.na(female) & !is.na(male) & !is.na(enrollment_total))

  if (nrow(gender_rows) > 0) {
    gender_sum <- gender_rows$female + gender_rows$male
    # Most rows should match closely; allow discrepancy due to suppressed data
    # (NDE suppresses counts <10, which can cause gender sum != total)
    pct_close <- mean(abs(gender_sum - gender_rows$enrollment_total) <= 5)
    expect_true(pct_close > 0.99)  # 99%+ of rows should match within 5
  }
})


# ==============================================================================
# Fidelity — tidy subgroup pct consistency
# ==============================================================================

test_that("pct for total_enrollment is always 1.0", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  total_rows <- tidy26 %>% filter(subgroup == "total_enrollment")
  expect_true(all(total_rows$pct == 1.0))
})

test_that("pct for non-total subgroups is between 0 and 1", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  non_total <- tidy26 %>% filter(subgroup != "total_enrollment")
  expect_true(all(non_total$pct >= 0, na.rm = TRUE))
  expect_true(all(non_total$pct <= 1, na.rm = TRUE))
})

test_that("pct = n_students / enrollment_total for tidy data", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  # Pick a specific row to verify pct calculation
  washoe <- tidy26 %>%
    filter(district_code == "16", is_district,
           grade_level == "TOTAL", subgroup == "hispanic")

  expect_equal(nrow(washoe), 1)
  # pct should be n_students / total_enrollment for washoe
  washoe_total <- tidy26 %>%
    filter(district_code == "16", is_district,
           grade_level == "TOTAL", subgroup == "total_enrollment") %>%
    pull(n_students)

  expected_pct <- washoe$n_students / washoe_total
  expect_equal(washoe$pct, expected_pct, tolerance = 1e-10)
})


# ==============================================================================
# No duplicate rows per group
# ==============================================================================

test_that("no duplicate district-level rows per subgroup per grade", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  dupes <- tidy26 %>%
    filter(is_district) %>%
    count(end_year, district_code, grade_level, subgroup) %>%
    filter(n > 1)

  expect_equal(nrow(dupes), 0)
})

test_that("no duplicate school-level rows per subgroup per grade", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  dupes <- tidy26 %>%
    filter(is_school) %>%
    count(end_year, school_code, grade_level, subgroup) %>%
    filter(n > 1)

  expect_equal(nrow(dupes), 0)
})


# ==============================================================================
# Cross-year consistency
# ==============================================================================

test_that("Nevada state enrollment is in reasonable range across years", {
  state_enr <- nvschooldata::get_state_enrollment(c(2021, 2022, 2023, 2024, 2026),
                                                   use_cache = TRUE)

  # All years should have enrollment between 400k and 550k
  expect_true(all(state_enr$total_enrollment > 400000))
  expect_true(all(state_enr$total_enrollment < 550000))
})

test_that("Clark County is always the largest district", {
  for (year in c(2024, 2026)) {
    tidy <- nvschooldata::fetch_enr(year, tidy = TRUE, use_cache = TRUE)

    dist_totals <- tidy %>%
      filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
      arrange(desc(n_students))

    expect_equal(dist_totals$district_code[1], "02",
                 label = paste("Clark County should be largest in", year))
  }
})

test_that("Washoe County is always the second largest district", {
  for (year in c(2024, 2026)) {
    tidy <- nvschooldata::fetch_enr(year, tidy = TRUE, use_cache = TRUE)

    dist_totals <- tidy %>%
      filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
      arrange(desc(n_students))

    expect_equal(dist_totals$district_code[2], "16",
                 label = paste("Washoe County should be second in", year))
  }
})


# ==============================================================================
# Cache functions
# ==============================================================================

test_that("cache_status returns a data.frame", {
  result <- nvschooldata::cache_status()

  expect_true(is.data.frame(result))
})

test_that("cache_status has expected columns", {
  result <- nvschooldata::cache_status()

  expect_true("year" %in% names(result))
  expect_true("type" %in% names(result))
  expect_true("size_mb" %in% names(result))
  expect_true("age_days" %in% names(result))
})

test_that("cache_status shows cached data after fetch", {
  # Ensure we have some cached data
  nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  result <- nvschooldata::cache_status()

  expect_true(nrow(result) > 0)
  expect_true(any(result$year == 2026, na.rm = TRUE))
})

test_that("clear_enr_cache returns count of removed files", {
  # First fetch to ensure cache exists
  nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  # Clear a specific year
  result <- nvschooldata::clear_enr_cache(2026)
  expect_true(is.numeric(result))
})


# ==============================================================================
# Edge cases
# ==============================================================================

test_that("SPCSA charter identification is consistent", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  # All SPCSA entities should be charter
  spcsa <- tidy26 %>%
    filter(grepl("SPCSA", lea_name, ignore.case = TRUE) |
           grepl("Charter", lea_name, ignore.case = TRUE))

  if (nrow(spcsa) > 0) {
    expect_true(all(spcsa$is_charter))
  }
})

test_that("district codes are zero-padded with consistent format", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  dist_codes <- unique(tidy26$district_code)
  dist_codes <- dist_codes[!is.na(dist_codes)]

  # District codes should be at least 2 characters (zero-padded)
  # NDE uses 2-char codes for traditional districts, 3-char for charter districts
  expect_true(all(nchar(dist_codes) >= 2))
  expect_true(all(nchar(dist_codes) <= 3))
  expect_true(all(grepl("^\\d+$", dist_codes)))  # All numeric
})

test_that("lea codes are zero-padded to 2 characters", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  lea_codes <- unique(tidy26$lea_code)
  lea_codes <- lea_codes[!is.na(lea_codes)]

  expect_true(all(nchar(lea_codes) == 2))
})

test_that("school codes are zero-padded with consistent format", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  school_codes <- tidy26 %>%
    filter(is_school) %>%
    pull(school_code) %>%
    unique()

  school_codes <- school_codes[!is.na(school_codes)]
  # School codes are at least 4 characters; some NDE charter schools use 5-6 digits
  expect_true(all(nchar(school_codes) >= 4))
  expect_true(all(nchar(school_codes) <= 6))
  expect_true(all(grepl("^\\d+$", school_codes)))  # All numeric
})


# ==============================================================================
# Column type consistency across years
# ==============================================================================

test_that("column types are consistent across modern format years", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)
  tidy24 <- nvschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Same columns
  expect_equal(names(tidy26), names(tidy24))

  # Same types for key columns
  expect_type(tidy26$end_year, typeof(tidy24$end_year))
  expect_type(tidy26$lea_code, typeof(tidy24$lea_code))
  expect_type(tidy26$n_students, typeof(tidy24$n_students))
  expect_type(tidy26$pct, typeof(tidy24$pct))
  expect_type(tidy26$is_district, typeof(tidy24$is_district))
  expect_type(tidy26$is_school, typeof(tidy24$is_school))
  expect_type(tidy26$is_charter, typeof(tidy24$is_charter))
})


# ==============================================================================
# Intermediate format (2019) structure matches
# ==============================================================================

test_that("intermediate format has same key columns as modern", {
  tidy19 <- nvschooldata::fetch_enr(2019, tidy = TRUE, use_cache = TRUE)

  # Must have these columns
  key_cols <- c(
    "end_year", "district_code", "school_code", "school_name",
    "entity_type", "grade_level", "subgroup", "n_students", "pct",
    "is_district", "is_school", "is_charter"
  )

  for (col in key_cols) {
    expect_true(col %in% names(tidy19),
                label = paste("Column", col, "should be in 2019 tidy data"))
  }
})


# ==============================================================================
# fetch_enr_range()
# ==============================================================================

test_that("fetch_enr_range returns combined data for valid range", {
  range_data <- nvschooldata::fetch_enr_range(2023, 2024, tidy = TRUE, use_cache = TRUE)

  expect_true(is.data.frame(range_data))
  expect_equal(sort(unique(range_data$end_year)), c(2023, 2024))
  expect_true(nrow(range_data) > 0)
})

test_that("fetch_enr_range skips unavailable years with warning", {
  # 2024-2026 includes 2025 which is unavailable
  expect_warning(
    range_data <- nvschooldata::fetch_enr_range(2024, 2026, tidy = TRUE, use_cache = TRUE),
    "Could not fetch data for 2025"
  )

  # Should still have data for 2024 and 2026
  expect_true(is.data.frame(range_data))
  years_present <- sort(unique(range_data$end_year))
  expect_true(2024 %in% years_present)
  expect_true(2026 %in% years_present)
  expect_false(2025 %in% years_present)
})


# ==============================================================================
# One observation per group per period
# ==============================================================================

test_that("one district observation per subgroup per grade per year", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  dupes <- tidy26 %>%
    filter(is_district) %>%
    group_by(end_year, district_code, grade_level, subgroup) %>%
    filter(n() > 1)

  expect_equal(nrow(dupes), 0)
})

test_that("one school observation per subgroup per grade per year", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  dupes <- tidy26 %>%
    filter(is_school) %>%
    group_by(end_year, school_code, grade_level, subgroup) %>%
    filter(n() > 1)

  expect_equal(nrow(dupes), 0)
})


# ==============================================================================
# Tidy enrollment preserves counts (no rounding)
# ==============================================================================

test_that("tidy enrollment counts are integers (no fractional values)", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  # n_students should be whole numbers
  non_na <- tidy26$n_students[!is.na(tidy26$n_students)]
  expect_true(all(non_na == floor(non_na)))
})

test_that("wide format counts are integers (no fractional values)", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)

  count_cols <- c("enrollment_total", "female", "male", "white", "black",
                  "hispanic", "asian", "pacific_islander", "native_american",
                  "multiracial")

  for (col in count_cols) {
    non_na <- wide26[[col]][!is.na(wide26[[col]])]
    expect_true(all(non_na == floor(non_na)),
                label = paste(col, "should be whole numbers"))
  }
})


# ==============================================================================
# Directory data consistency with enrollment data
# ==============================================================================

test_that("directory school count is in same order of magnitude as enrollment", {
  dir_data <- nvschooldata::fetch_directory(use_cache = TRUE)
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  dir_count <- nrow(dir_data)
  enr_school_count <- tidy26 %>%
    filter(is_school, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    nrow()

  # Directory should have similar number of schools (within 2x)
  expect_true(dir_count > enr_school_count * 0.5)
  expect_true(dir_count < enr_school_count * 2)
})
