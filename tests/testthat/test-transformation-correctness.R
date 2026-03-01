# ==============================================================================
# Transformation Correctness Tests for nvschooldata
# ==============================================================================
#
# These tests verify the transformation pipeline produces correct, consistent
# output. Every expected value is derived from real NDE data via fetch_enr().
#
# Test Categories:
#  1. Suppression handling (safe_numeric)
#  2. ID formatting (lea_code, district_code, school_code)
#  3. Grade standardization (standardize_grade)
#  4. Subgroup values
#  5. Pivot fidelity (wide vs tidy)
#  6. Percentage calculations
#  7. Aggregation (district sum, grade-level sum)
#  8. Entity flags (is_state, is_district, is_school, is_charter)
#  9. Per-year known values
# 10. Cross-year consistency
# 11. Grade aggregation (enr_grade_aggs)
# 12. Data uniqueness (no duplicates)
#
# ==============================================================================

library(testthat)

# ---- helpers ----------------------------------------------------------------

get_tidy <- function(yr) {

  nvschooldata::fetch_enr(yr, tidy = TRUE, use_cache = TRUE)
}

get_wide <- function(yr) {
  nvschooldata::fetch_enr(yr, tidy = FALSE, use_cache = TRUE)
}

# ==============================================================================
# 1. Suppression Handling (safe_numeric)
# ==============================================================================

test_that("safe_numeric converts suppression markers to NA", {
  safe <- nvschooldata:::safe_numeric

  # Standard suppression markers used by NDE

  expect_true(is.na(safe("*")))
  expect_true(is.na(safe("<10")))
  expect_true(is.na(safe("N/A")))
  expect_true(is.na(safe("-")))
  expect_true(is.na(safe("")))
  expect_true(is.na(safe("  ")))
})

test_that("safe_numeric converts valid numbers correctly", {
  safe <- nvschooldata:::safe_numeric

  expect_equal(safe("42"), 42)
  expect_equal(safe("1,234"), 1234)
  expect_equal(safe("  100  "), 100)
  expect_equal(safe("0"), 0)
})

test_that("safe_numeric passes through already-numeric values", {
  safe <- nvschooldata:::safe_numeric

  expect_equal(safe(42), 42)
  expect_equal(safe(0), 0)
  expect_equal(safe(c(1, 2, 3)), c(1, 2, 3))
})

test_that("suppressed values become NA in wide data, not zero", {
  wide <- get_wide(2024)

  # Special populations have many NAs from suppression in 2024

  # free_reduced_lunch has 4413 NAs out of 5169 rows
  frl_nas <- sum(is.na(wide$free_reduced_lunch))
  expect_gt(frl_nas, 4000)

  # enrollment_total should have 0 NAs
  expect_equal(sum(is.na(wide$enrollment_total)), 0)
})

test_that("tidy data has no NA n_students (filtered by tidy_enr)", {
  tidy <- get_tidy(2024)

  expect_equal(sum(is.na(tidy$n_students)), 0)
})

# ==============================================================================
# 2. ID Formatting
# ==============================================================================

test_that("lea_code is zero-padded two-digit character", {
  tidy <- get_tidy(2024)

  expect_type(tidy$lea_code, "character")
  # All should be exactly 2 characters
  expect_true(all(nchar(tidy$lea_code) == 2))
  # Known lea_codes
  expect_true("02" %in% tidy$lea_code)  # Clark County
  expect_true("16" %in% tidy$lea_code)  # Washoe County
  expect_true("04" %in% tidy$lea_code)  # Elko County
  expect_true("18" %in% tidy$lea_code)  # SPCSA charters
})

test_that("district_code is zero-padded character", {
  tidy <- get_tidy(2024)

  expect_type(tidy$district_code, "character")
  # District codes should exist and be character
  expect_true("02" %in% tidy$district_code)  # Clark County
  expect_true("16" %in% tidy$district_code)  # Washoe County
})

test_that("school_code is character and NA for district rows", {
  tidy <- get_tidy(2024)

  expect_type(tidy$school_code, "character")
  # District rows should have NA school_code
  district_rows <- tidy[tidy$is_district, ]
  expect_true(all(is.na(district_rows$school_code)))

  # School rows should have non-NA school_code
  school_rows <- tidy[tidy$is_school, ]
  expect_true(all(!is.na(school_rows$school_code)))
})

test_that("school_name is NA for district rows", {
  tidy <- get_tidy(2024)

  district_rows <- tidy[tidy$is_district, ]
  expect_true(all(is.na(district_rows$school_name)))
})

# ==============================================================================
# 3. Grade Standardization
# ==============================================================================

test_that("standardize_grade normalizes kindergarten variants", {
  std <- nvschooldata:::standardize_grade

  expect_equal(std("KG"), "K")
  expect_equal(std("KINDERGARTEN"), "K")
  expect_equal(std("0K"), "K")
  expect_equal(std("K"), "K")
})

test_that("standardize_grade normalizes pre-K variants", {
  std <- nvschooldata:::standardize_grade

  expect_equal(std("PK"), "PK")
  expect_equal(std("PRE-K"), "PK")
  expect_equal(std("PREK"), "PK")
})

test_that("standardize_grade normalizes ungraded and adult", {
  std <- nvschooldata:::standardize_grade

  expect_equal(std("UG"), "UG")
  expect_equal(std("UNGRADED"), "UG")
  expect_equal(std("13"), "UG")  # NV uses 13 for ungraded
  expect_equal(std("AD"), "AD")
  expect_equal(std("ADULT"), "AD")
})

test_that("standardize_grade pads single-digit grades", {
  std <- nvschooldata:::standardize_grade

  expect_equal(std("1"), "01")
  expect_equal(std("9"), "09")
  # Already-padded should stay the same
  expect_equal(std("01"), "01")
  expect_equal(std("12"), "12")
})

test_that("tidy data contains only standard grade levels", {
  tidy <- get_tidy(2024)

  valid_grades <- c("PK", "K", "01", "02", "03", "04", "05", "06", "07", "08",
                     "09", "10", "11", "12", "UG", "AD", "TOTAL")
  actual_grades <- unique(tidy$grade_level)

  expect_true(all(actual_grades %in% valid_grades),
    info = paste("Unexpected grades:", paste(setdiff(actual_grades, valid_grades), collapse = ", "))
  )

  # No raw format leaks
  raw_formats <- c("KG", "KINDERGARTEN", "PRE-K", "PREK", "0K", "UNGRADED", "ADULT")
  expect_false(any(actual_grades %in% raw_formats))
})

test_that("district rows only have TOTAL grade_level", {
  tidy <- get_tidy(2024)

  district_grades <- unique(tidy$grade_level[tidy$is_district])
  expect_equal(district_grades, "TOTAL")
})

test_that("school rows include per-grade and TOTAL rows", {
  tidy <- get_tidy(2024)

  school_grades <- unique(tidy$grade_level[tidy$is_school])
  expect_true("TOTAL" %in% school_grades)
  expect_true("K" %in% school_grades)
  expect_true("01" %in% school_grades)
  expect_true("12" %in% school_grades)
})

# ==============================================================================
# 4. Subgroup Values
# ==============================================================================

test_that("modern format (2024) has all 17 subgroups", {
  tidy <- get_tidy(2024)

  expected_subgroups <- c(
    "total_enrollment", "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "female", "male",
    "free_reduced_lunch", "special_ed", "lep",
    "migrant", "foster", "military", "homeless"
  )

  actual_subgroups <- sort(unique(tidy$subgroup))
  expect_equal(actual_subgroups, sort(expected_subgroups))
})

test_that("intermediate format (2019) has race/gender but not special populations", {
  tidy <- get_tidy(2019)

  actual_subgroups <- sort(unique(tidy$subgroup))

  # Should have race and gender
  expect_true("total_enrollment" %in% actual_subgroups)
  expect_true("white" %in% actual_subgroups)
  expect_true("black" %in% actual_subgroups)
  expect_true("hispanic" %in% actual_subgroups)
  expect_true("female" %in% actual_subgroups)
  expect_true("male" %in% actual_subgroups)

  # Should NOT have special populations in intermediate format
  expect_false("free_reduced_lunch" %in% actual_subgroups)
  expect_false("special_ed" %in% actual_subgroups)
  expect_false("lep" %in% actual_subgroups)
})

test_that("subgroup names follow cross-state standards", {
  tidy <- get_tidy(2024)
  subs <- unique(tidy$subgroup)

  # Must NOT have non-standard names

  non_standard <- c("total", "low_income", "economically_disadvantaged", "frl",
                     "iep", "disability", "el", "ell", "english_learner",
                     "american_indian", "two_or_more")
  expect_false(any(non_standard %in% subs),
    info = paste("Non-standard subgroup found:", paste(intersect(non_standard, subs), collapse = ", "))
  )

  # Must have standard names
  expect_true("total_enrollment" %in% subs)
  expect_true("econ_disadv" %in% subs || "free_reduced_lunch" %in% subs)
  expect_true("special_ed" %in% subs)
  expect_true("lep" %in% subs)
  expect_true("native_american" %in% subs)
  expect_true("multiracial" %in% subs)
})

# ==============================================================================
# 5. Pivot Fidelity (Wide vs Tidy)
# ==============================================================================

test_that("tidy total_enrollment matches wide enrollment_total for CCSD", {
  wide <- get_wide(2024)
  tidy <- get_tidy(2024)

  # CCSD district TOTAL
  wide_val <- wide$enrollment_total[
    wide$lea_code == "02" & wide$entity_type == "District" & wide$grade_level == "TOTAL"
  ]
  tidy_val <- tidy$n_students[
    tidy$lea_code == "02" & tidy$is_district & tidy$grade_level == "TOTAL" &
    tidy$subgroup == "total_enrollment"
  ]

  expect_equal(tidy_val, wide_val)
  expect_equal(tidy_val, 309397)
})

test_that("tidy race counts match wide columns for CCSD", {
  wide <- get_wide(2024)
  tidy <- get_tidy(2024)

  ccsd_wide <- wide[wide$lea_code == "02" & wide$entity_type == "District" &
                     wide$grade_level == "TOTAL", ]
  ccsd_tidy <- tidy[tidy$lea_code == "02" & tidy$is_district &
                     tidy$grade_level == "TOTAL", ]

  for (sub in c("white", "black", "hispanic", "asian", "pacific_islander",
                "native_american", "multiracial")) {
    tidy_val <- ccsd_tidy$n_students[ccsd_tidy$subgroup == sub]
    wide_val <- ccsd_wide[[sub]]
    expect_equal(tidy_val, wide_val, info = paste("Subgroup:", sub))
  }
})

test_that("tidy gender counts match wide columns for CCSD", {
  wide <- get_wide(2024)
  tidy <- get_tidy(2024)

  ccsd_wide <- wide[wide$lea_code == "02" & wide$entity_type == "District" &
                     wide$grade_level == "TOTAL", ]
  ccsd_tidy <- tidy[tidy$lea_code == "02" & tidy$is_district &
                     tidy$grade_level == "TOTAL", ]

  expect_equal(
    ccsd_tidy$n_students[ccsd_tidy$subgroup == "female"],
    ccsd_wide$female
  )
  expect_equal(
    ccsd_tidy$n_students[ccsd_tidy$subgroup == "male"],
    ccsd_wide$male
  )
})

test_that("tidy special population counts match wide columns for CCSD", {
  wide <- get_wide(2024)
  tidy <- get_tidy(2024)

  ccsd_wide <- wide[wide$lea_code == "02" & wide$entity_type == "District" &
                     wide$grade_level == "TOTAL", ]
  ccsd_tidy <- tidy[tidy$lea_code == "02" & tidy$is_district &
                     tidy$grade_level == "TOTAL", ]

  expect_equal(
    ccsd_tidy$n_students[ccsd_tidy$subgroup == "free_reduced_lunch"],
    ccsd_wide$free_reduced_lunch
  )
  expect_equal(
    ccsd_tidy$n_students[ccsd_tidy$subgroup == "special_ed"],
    ccsd_wide$special_ed
  )
  expect_equal(
    ccsd_tidy$n_students[ccsd_tidy$subgroup == "lep"],
    ccsd_wide$lep
  )
})

test_that("school-level tidy values match wide for a CCSD school", {
  wide <- get_wide(2024)
  tidy <- get_tidy(2024)

  # Use district-level CCSD data for robust pivot fidelity (already covered above).
  # For school-level, verify that tidy total_enrollment values appear in wide
  # enrollment_total for at least one matching row.
  # Note: school_totals and school_grades sheets may both contribute TOTAL rows,
  # so we match on first occurrence.

  # Pick first CCSD school from wide with a non-NA TOTAL row
  ccsd_school_wide <- wide[wide$lea_code == "02" & wide$entity_type == "School" &
                            wide$grade_level == "TOTAL" &
                            !is.na(wide$enrollment_total), ]
  first_school <- ccsd_school_wide[1, ]
  sc <- first_school$school_code

  # Get matching tidy row
  tidy_match <- tidy[tidy$lea_code == "02" & tidy$school_code == sc &
                      tidy$grade_level == "TOTAL" &
                      tidy$subgroup == "total_enrollment", ]

  # At least one row should exist
  expect_gt(nrow(tidy_match), 0)

  # The enrollment value should match (take first of each)
  expect_equal(tidy_match$n_students[1], first_school$enrollment_total)
})

test_that("school-level grade-specific tidy values match wide", {
  wide <- get_wide(2024)
  tidy <- get_tidy(2024)

  # Pick a CCSD school with K grade data
  ccsd_k_wide <- wide[wide$lea_code == "02" & wide$entity_type == "School" &
                       wide$grade_level == "K" & !is.na(wide$enrollment_total), ]
  first_k <- ccsd_k_wide[1, ]
  sc <- first_k$school_code

  # Get matching tidy row for K grade total_enrollment
  tidy_k <- tidy[tidy$lea_code == "02" & tidy$school_code == sc &
                  tidy$grade_level == "K" & tidy$subgroup == "total_enrollment", ]

  expect_gt(nrow(tidy_k), 0)
  expect_equal(tidy_k$n_students[1], first_k$enrollment_total)

  # Also check a gender column
  if (!is.na(first_k$female)) {
    tidy_k_fem <- tidy[tidy$lea_code == "02" & tidy$school_code == sc &
                        tidy$grade_level == "K" & tidy$subgroup == "female", ]
    expect_gt(nrow(tidy_k_fem), 0)
    expect_equal(tidy_k_fem$n_students[1], first_k$female)
  }
})

# ==============================================================================
# 6. Percentage Calculations
# ==============================================================================

test_that("total_enrollment pct is always 1.0", {
  tidy <- get_tidy(2024)

  te_rows <- tidy[tidy$subgroup == "total_enrollment", ]
  expect_true(all(te_rows$pct == 1.0))
})

test_that("subgroup pct equals n_students / enrollment_total", {
  wide <- get_wide(2024)
  tidy <- get_tidy(2024)

  # CCSD white percentage
  ccsd_tidy_white <- tidy[tidy$lea_code == "02" & tidy$is_district &
    tidy$grade_level == "TOTAL" & tidy$subgroup == "white", ]
  expected_pct <- 61459 / 309397

  expect_equal(ccsd_tidy_white$pct, expected_pct, tolerance = 1e-10)
})

test_that("pct values are between 0 and 1 for non-special populations", {
  tidy <- get_tidy(2024)

  race_gender <- tidy[tidy$subgroup %in% c("white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial", "female", "male"), ]

  expect_true(all(race_gender$pct >= 0, na.rm = TRUE))
  expect_true(all(race_gender$pct <= 1, na.rm = TRUE))
})

test_that("special population pct can exceed 1.0 (FRL > total is valid)", {

  # free_reduced_lunch can be close to or exceed total for some entities
  # This test just ensures pct is computed correctly even when > 1
  tidy <- get_tidy(2024)

  frl <- tidy[tidy$subgroup == "free_reduced_lunch" & !is.na(tidy$pct), ]
  # All pct should be non-negative
  expect_true(all(frl$pct >= 0))
})

# ==============================================================================
# 7. Aggregation
# ==============================================================================

test_that("sum of district total_enrollment equals state total (2024)", {
  tidy <- get_tidy(2024)

  state_total <- sum(
    tidy$n_students[tidy$is_district & tidy$subgroup == "total_enrollment" &
                    tidy$grade_level == "TOTAL"],
    na.rm = TRUE
  )
  expect_equal(state_total, 485570)
})

test_that("school-level grade sums match TOTAL for CCSD schools", {
  tidy <- get_tidy(2024)

  # Sum of per-grade total_enrollment across all CCSD schools
  grade_sum <- sum(
    tidy$n_students[tidy$lea_code == "02" & tidy$is_school &
      tidy$subgroup == "total_enrollment" & tidy$grade_level != "TOTAL"],
    na.rm = TRUE
  )

  total_sum <- sum(
    tidy$n_students[tidy$lea_code == "02" & tidy$is_school &
      tidy$subgroup == "total_enrollment" & tidy$grade_level == "TOTAL"],
    na.rm = TRUE
  )

  expect_equal(grade_sum, total_sum)
  expect_equal(total_sum, 309397)
})

test_that("race subgroup sum is close to total (within suppression margin)", {
  tidy <- get_tidy(2024)

  # CCSD district, TOTAL grade
  race_subs <- c("white", "black", "hispanic", "asian", "pacific_islander",
                 "native_american", "multiracial")
  race_sum <- sum(
    tidy$n_students[tidy$lea_code == "02" & tidy$is_district &
      tidy$grade_level == "TOTAL" & tidy$subgroup %in% race_subs],
    na.rm = TRUE
  )
  total_val <- tidy$n_students[tidy$lea_code == "02" & tidy$is_district &
    tidy$grade_level == "TOTAL" & tidy$subgroup == "total_enrollment"]

  # Race sum: 309394, total: 309397, diff of 3 due to suppression
  expect_equal(race_sum, 309394)
  expect_true(abs(total_val - race_sum) < 100,
    info = paste("Race sum diff:", total_val - race_sum))
})

test_that("number of districts is reasonable (2024)", {
  tidy <- get_tidy(2024)

  n_districts <- length(unique(
    tidy$district_code[tidy$is_district & tidy$subgroup == "total_enrollment" &
                       tidy$grade_level == "TOTAL"]
  ))

  # 2024 has 63 district-level records
  expect_equal(
    sum(tidy$is_district & tidy$subgroup == "total_enrollment" &
        tidy$grade_level == "TOTAL"),
    63
  )
})

# ==============================================================================
# 8. Entity Flags
# ==============================================================================

test_that("is_state is always FALSE (NV data has no state-level rows)", {
  tidy <- get_tidy(2024)

  expect_false(any(tidy$is_state))
})

test_that("is_district and is_school are mutually exclusive", {
  tidy <- get_tidy(2024)

  # No row should have both TRUE
  both_true <- tidy$is_district & tidy$is_school
  expect_false(any(both_true))

  # Every row should have at least one TRUE
  neither <- !tidy$is_district & !tidy$is_school
  expect_false(any(neither))
})

test_that("is_district corresponds to entity_type == District", {
  tidy <- get_tidy(2024)

  expect_true(all(tidy$is_district == (tidy$entity_type == "District")))
})

test_that("is_school corresponds to entity_type == School", {
  tidy <- get_tidy(2024)

  expect_true(all(tidy$is_school == (tidy$entity_type == "School")))
})

test_that("is_charter TRUE only for SPCSA (lea_code 18)", {
  tidy <- get_tidy(2024)

  charter_rows <- tidy[tidy$is_charter, ]

  # All charter rows should be in lea_code 18 (SPCSA)
  expect_true(all(charter_rows$lea_code == "18"))

  # SPCSA should exist
  expect_gt(nrow(charter_rows), 0)
})

test_that("is_charter FALSE for traditional districts", {
  tidy <- get_tidy(2024)

  ccsd <- tidy[tidy$lea_code == "02", ]
  expect_false(any(ccsd$is_charter))

  washoe <- tidy[tidy$lea_code == "16", ]
  expect_false(any(washoe$is_charter))
})

test_that("aggregation_flag is campus for schools, district for districts", {
  tidy <- get_tidy(2024)

  expect_true(all(tidy$aggregation_flag[tidy$is_school] == "campus"))
  expect_true(all(tidy$aggregation_flag[tidy$is_district] == "district"))
})

# ==============================================================================
# 9. Per-Year Known Values
# ==============================================================================

test_that("2024 CCSD enrollment is 309,397", {
  tidy <- get_tidy(2024)

  ccsd <- tidy$n_students[
    tidy$lea_code == "02" & tidy$is_district & tidy$grade_level == "TOTAL" &
    tidy$subgroup == "total_enrollment"
  ]
  expect_equal(ccsd, 309397)
})

test_that("2024 Washoe enrollment is 64,755", {
  tidy <- get_tidy(2024)

  washoe <- tidy$n_students[
    tidy$lea_code == "16" & tidy$is_district & tidy$grade_level == "TOTAL" &
    tidy$subgroup == "total_enrollment"
  ]
  expect_equal(washoe, 64755)
})

test_that("2024 CCSD demographics match known values", {
  tidy <- get_tidy(2024)

  ccsd <- tidy[tidy$lea_code == "02" & tidy$is_district &
               tidy$grade_level == "TOTAL", ]

  expect_equal(ccsd$n_students[ccsd$subgroup == "white"], 61459)
  expect_equal(ccsd$n_students[ccsd$subgroup == "black"], 49790)
  expect_equal(ccsd$n_students[ccsd$subgroup == "hispanic"], 149478)
  expect_equal(ccsd$n_students[ccsd$subgroup == "asian"], 18414)
  expect_equal(ccsd$n_students[ccsd$subgroup == "pacific_islander"], 4929)
  expect_equal(ccsd$n_students[ccsd$subgroup == "native_american"], 1025)
  expect_equal(ccsd$n_students[ccsd$subgroup == "multiracial"], 24299)
  expect_equal(ccsd$n_students[ccsd$subgroup == "female"], 150449)
  expect_equal(ccsd$n_students[ccsd$subgroup == "male"], 158843)
})

test_that("2024 CCSD special populations match known values", {
  tidy <- get_tidy(2024)

  ccsd <- tidy[tidy$lea_code == "02" & tidy$is_district &
               tidy$grade_level == "TOTAL", ]

  expect_equal(ccsd$n_students[ccsd$subgroup == "free_reduced_lunch"], 296267)
  expect_equal(ccsd$n_students[ccsd$subgroup == "special_ed"], 42585)
  expect_equal(ccsd$n_students[ccsd$subgroup == "lep"], 48278)
})

test_that("2024 CCSD district-level known values match", {
  tidy <- get_tidy(2024)

  # Use district-level data which has exactly 1 row per subgroup
  # (no school_totals/school_grades duplication issue)
  ccsd <- tidy[tidy$lea_code == "02" & tidy$is_district &
               tidy$grade_level == "TOTAL", ]

  get_val <- function(sub) {
    val <- ccsd$n_students[ccsd$subgroup == sub]
    expect_length(val, 1)
    val
  }

  expect_equal(get_val("total_enrollment"), 309397)
  expect_equal(get_val("female"), 150449)
  expect_equal(get_val("male"), 158843)
  expect_equal(get_val("white"), 61459)
  expect_equal(get_val("black"), 49790)
  expect_equal(get_val("hispanic"), 149478)
  expect_equal(get_val("asian"), 18414)
  expect_equal(get_val("pacific_islander"), 4929)
  expect_equal(get_val("native_american"), 1025)
  expect_equal(get_val("multiracial"), 24299)
  expect_equal(get_val("free_reduced_lunch"), 296267)
  expect_equal(get_val("special_ed"), 42585)
  expect_equal(get_val("lep"), 48278)
})

test_that("2024 Washoe County district known values match", {
  tidy <- get_tidy(2024)

  washoe <- tidy[tidy$lea_code == "16" & tidy$is_district &
                  tidy$grade_level == "TOTAL", ]

  get_val <- function(sub) {
    val <- washoe$n_students[washoe$subgroup == sub]
    expect_length(val, 1)
    val
  }

  expect_equal(get_val("total_enrollment"), 64755)
  expect_equal(get_val("female"), 31180)
  expect_equal(get_val("male"), 33547)
})

test_that("2026 CCSD enrollment is 291,587", {
  tidy <- get_tidy(2026)

  ccsd <- tidy$n_students[
    tidy$lea_code == "02" & tidy$is_district & tidy$grade_level == "TOTAL" &
    tidy$subgroup == "total_enrollment"
  ]
  expect_equal(ccsd, 291587)
})

test_that("2026 state total enrollment is 473,657", {
  tidy <- get_tidy(2026)

  state_total <- sum(
    tidy$n_students[tidy$is_district & tidy$subgroup == "total_enrollment" &
                    tidy$grade_level == "TOTAL"],
    na.rm = TRUE
  )
  expect_equal(state_total, 473657)
})

test_that("2026 CCSD demographics match known values", {
  tidy <- get_tidy(2026)

  ccsd <- tidy[tidy$lea_code == "02" & tidy$is_district &
               tidy$grade_level == "TOTAL", ]

  expect_equal(ccsd$n_students[ccsd$subgroup == "white"], 54411)
  expect_equal(ccsd$n_students[ccsd$subgroup == "black"], 47438)
  expect_equal(ccsd$n_students[ccsd$subgroup == "hispanic"], 142657)
  expect_equal(ccsd$n_students[ccsd$subgroup == "asian"], 17998)
  expect_equal(ccsd$n_students[ccsd$subgroup == "female"], 141358)
  expect_equal(ccsd$n_students[ccsd$subgroup == "male"], 150156)
})

test_that("2023 state total is 489,597", {
  tidy <- get_tidy(2023)

  state_total <- sum(
    tidy$n_students[tidy$is_district & tidy$subgroup == "total_enrollment" &
                    tidy$grade_level == "TOTAL"],
    na.rm = TRUE
  )
  expect_equal(state_total, 489597)
})

test_that("2023 CCSD enrollment is 314,372", {
  tidy <- get_tidy(2023)

  ccsd <- tidy$n_students[
    tidy$lea_code == "02" & tidy$is_district & tidy$grade_level == "TOTAL" &
    tidy$subgroup == "total_enrollment"
  ]
  expect_equal(ccsd, 314372)
})

test_that("2019 (intermediate format) state total is 498,616", {
  tidy <- get_tidy(2019)

  state_total <- sum(
    tidy$n_students[tidy$is_district & tidy$subgroup == "total_enrollment" &
                    tidy$grade_level == "TOTAL"],
    na.rm = TRUE
  )
  expect_equal(state_total, 498616)
})

test_that("2019 (intermediate format) CCSD enrollment is 335,333", {
  tidy <- get_tidy(2019)

  ccsd <- tidy$n_students[
    tidy$lea_code == "02" & tidy$is_district & tidy$grade_level == "TOTAL" &
    tidy$subgroup == "total_enrollment"
  ]
  expect_equal(ccsd, 335333)
})

# ==============================================================================
# 10. Cross-Year Consistency
# ==============================================================================

test_that("list_enr_years returns expected years (excluding 2025)", {
  years <- nvschooldata::list_enr_years()

  expect_true(2016 %in% years)
  expect_true(2024 %in% years)
  expect_true(2026 %in% years)
  expect_false(2025 %in% years)  # 2025 is unavailable from NDE
})

test_that("tidy schema is consistent across modern format years", {
  tidy_2022 <- get_tidy(2022)
  tidy_2024 <- get_tidy(2024)

  # Same column names
  expect_equal(sort(names(tidy_2022)), sort(names(tidy_2024)))
})

test_that("end_year column matches requested year", {
  tidy_2024 <- get_tidy(2024)
  expect_true(all(tidy_2024$end_year == 2024))

  tidy_2023 <- get_tidy(2023)
  expect_true(all(tidy_2023$end_year == 2023))
})

test_that("CCSD is present in every modern format year", {
  for (yr in c(2021, 2022, 2023, 2024, 2026)) {
    tidy <- get_tidy(yr)
    ccsd_rows <- tidy[tidy$lea_code == "02" & tidy$is_district &
      tidy$subgroup == "total_enrollment" & tidy$grade_level == "TOTAL", ]
    expect_gt(nrow(ccsd_rows), 0, label = paste("CCSD rows in year", yr))
    expect_gt(ccsd_rows$n_students, 200000,
      label = paste("CCSD enrollment in year", yr))
  }
})

test_that("state enrollment is stable across years (not 2x inflation)", {
  totals <- vapply(c(2021, 2022, 2023, 2024, 2026), function(yr) {
    tidy <- get_tidy(yr)
    sum(tidy$n_students[tidy$is_district & tidy$subgroup == "total_enrollment" &
                        tidy$grade_level == "TOTAL"], na.rm = TRUE)
  }, numeric(1))

  # Nevada state enrollment should be between 400k and 600k for these years
  expect_true(all(totals > 400000))
  expect_true(all(totals < 600000))

  # No year should be 2x another (inflation bug check)
  for (i in seq_along(totals)) {
    for (j in seq_along(totals)) {
      ratio <- totals[i] / totals[j]
      expect_true(ratio > 0.8 && ratio < 1.25,
        info = paste("Year ratio out of range:", totals[i], "/", totals[j]))
    }
  }
})

test_that("number of districts grows slowly across years", {
  counts <- vapply(c(2021, 2022, 2023, 2024, 2026), function(yr) {
    tidy <- get_tidy(yr)
    sum(tidy$is_district & tidy$subgroup == "total_enrollment" &
        tidy$grade_level == "TOTAL")
  }, integer(1))

  # Known values
  expect_equal(counts[1], 56L)  # 2021
  expect_equal(counts[2], 57L)  # 2022
  expect_equal(counts[3], 62L)  # 2023
  expect_equal(counts[4], 63L)  # 2024
  expect_equal(counts[5], 70L)  # 2026

  # Districts should not double year-over-year
  for (i in 2:length(counts)) {
    expect_true(counts[i] / counts[i - 1] < 1.5,
      info = paste("District count jumped too much between years"))
  }
})

# ==============================================================================
# 11. Grade Aggregation (enr_grade_aggs)
# ==============================================================================

test_that("enr_grade_aggs produces K8, HS, K12 grade levels", {
  tidy <- get_tidy(2024)
  aggs <- nvschooldata::enr_grade_aggs(tidy)

  expect_true(all(c("K8", "HS", "K12") %in% unique(aggs$grade_level)))
  expect_equal(sort(unique(aggs$grade_level)), c("HS", "K12", "K8"))
})

test_that("K8 + HS equals K12 for school-level data", {
  tidy <- get_tidy(2024)
  aggs <- nvschooldata::enr_grade_aggs(tidy)

  # CCSD school-level aggregation
  ccsd_k8 <- sum(
    aggs$n_students[aggs$lea_code == "02" & aggs$is_school & aggs$grade_level == "K8"],
    na.rm = TRUE
  )
  ccsd_hs <- sum(
    aggs$n_students[aggs$lea_code == "02" & aggs$is_school & aggs$grade_level == "HS"],
    na.rm = TRUE
  )
  ccsd_k12 <- sum(
    aggs$n_students[aggs$lea_code == "02" & aggs$is_school & aggs$grade_level == "K12"],
    na.rm = TRUE
  )

  expect_equal(ccsd_k8 + ccsd_hs, ccsd_k12)
  expect_equal(ccsd_k12, 292947)
})

test_that("grade aggs only use total_enrollment subgroup", {
  tidy <- get_tidy(2024)
  aggs <- nvschooldata::enr_grade_aggs(tidy)

  expect_true(all(aggs$subgroup == "total_enrollment"))
})

# ==============================================================================
# 12. Data Uniqueness (No Duplicates)
# ==============================================================================

test_that("no duplicate rows per entity/grade/subgroup in tidy 2024", {
  tidy <- get_tidy(2024)

  dupes <- tidy |>
    dplyr::count(lea_code, district_code, school_code, entity_type,
                 grade_level, subgroup) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(dupes), 0,
    info = paste("Found", nrow(dupes), "duplicate entity/grade/subgroup combos"))
})

test_that("no duplicate rows per entity/grade/subgroup in tidy 2026", {
  tidy <- get_tidy(2026)

  dupes <- tidy |>
    dplyr::count(lea_code, district_code, school_code, entity_type,
                 grade_level, subgroup) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(dupes), 0,
    info = paste("Found", nrow(dupes), "duplicate entity/grade/subgroup combos"))
})

test_that("no duplicate rows in wide format (one row per entity/grade)", {
  wide <- get_wide(2024)

  dupes <- wide |>
    dplyr::count(lea_code, district_code, school_code, entity_type, grade_level) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(dupes), 0,
    info = paste("Found", nrow(dupes), "duplicate entity/grade combos in wide"))
})

# ==============================================================================
# 13. Data Quality Guards
# ==============================================================================

test_that("no Inf or NaN in numeric columns", {
  tidy <- get_tidy(2024)

  for (col in c("n_students", "pct", "end_year")) {
    vals <- tidy[[col]]
    if (is.numeric(vals)) {
      expect_false(any(is.infinite(vals), na.rm = TRUE),
        info = paste("Inf found in", col))
      expect_false(any(is.nan(vals), na.rm = TRUE),
        info = paste("NaN found in", col))
    }
  }
})

test_that("n_students is non-negative", {
  tidy <- get_tidy(2024)

  expect_true(all(tidy$n_students >= 0, na.rm = TRUE))
})

test_that("wide enrollment_total is non-negative", {
  wide <- get_wide(2024)

  expect_true(all(wide$enrollment_total >= 0, na.rm = TRUE))
})

test_that("lea_name and district_name are non-empty for districts", {
  tidy <- get_tidy(2024)

  dist_rows <- tidy[tidy$is_district, ]
  expect_true(all(!is.na(dist_rows$lea_name)))
  expect_true(all(nchar(dist_rows$lea_name) > 0))
  expect_true(all(!is.na(dist_rows$district_name)))
  expect_true(all(nchar(dist_rows$district_name) > 0))
})
