# ==============================================================================
# Exhaustive Enrollment Tests for nvschooldata
# ==============================================================================
#
# Tests EVERY exported enrollment function with EVERY parameter combination.
# All expected values are pinned from real Nevada DOE data.
#
# ==============================================================================

library(testthat)
library(dplyr)

# ==============================================================================
# list_enr_years()
# ==============================================================================

test_that("list_enr_years returns correct available years", {
  years <- nvschooldata::list_enr_years()

  expect_type(years, "integer")
  expect_true(2016 %in% years)
  expect_true(2026 %in% years)
  expect_false(2025 %in% years)  # 2024-25 unavailable from NDE
  expect_equal(min(years), 2016L)
  expect_equal(max(years), 2026L)
  expect_equal(length(years), 10L)
  expect_equal(years, c(2016L, 2017L, 2018L, 2019L, 2020L, 2021L, 2022L, 2023L, 2024L, 2026L))
})


# ==============================================================================
# get_available_years()
# ==============================================================================

test_that("get_available_years returns correct structure", {
  yrs <- nvschooldata::get_available_years()

  expect_type(yrs, "list")
  expect_true("min_year" %in% names(yrs))
  expect_true("max_year" %in% names(yrs))
  expect_true("description" %in% names(yrs))
  expect_equal(yrs$min_year, 2016)
  expect_equal(yrs$max_year, 2026)
  expect_true(nchar(yrs$description) > 50)
  expect_true(grepl("Nevada", yrs$description))
})


# ==============================================================================
# fetch_enr() — 2026 modern format, tidy=TRUE
# ==============================================================================

test_that("fetch_enr 2026 tidy returns correct dimensions", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  expect_true(is.data.frame(tidy26))
  expect_equal(nrow(tidy26), 54762)
  expect_equal(ncol(tidy26), 17)
  expect_equal(unique(tidy26$end_year), 2026)
})

test_that("fetch_enr 2026 tidy has correct column names", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  expected_cols <- c(
    "end_year", "lea_code", "lea_name",
    "district_code", "district_name",
    "school_code", "school_name",
    "entity_type", "grade_level",
    "subgroup", "n_students", "pct",
    "is_state", "is_district", "is_school",
    "aggregation_flag", "is_charter"
  )
  expect_equal(names(tidy26), expected_cols)
})

test_that("fetch_enr 2026 tidy has correct entity types", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  entity_types <- sort(unique(tidy26$entity_type))
  expect_equal(entity_types, c("District", "School"))
})

test_that("fetch_enr 2026 tidy has all 17 subgroups", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  subgroups <- sort(unique(tidy26$subgroup))
  expected_subgroups <- sort(c(
    "total_enrollment", "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "female", "male",
    "free_reduced_lunch", "special_ed", "lep",
    "migrant", "foster", "military", "homeless"
  ))
  expect_equal(subgroups, expected_subgroups)
})

test_that("fetch_enr 2026 tidy has all expected grade levels", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  grade_levels <- sort(unique(tidy26$grade_level))
  expected_grades <- sort(c(
    "PK", "K", "01", "02", "03", "04", "05", "06",
    "07", "08", "09", "10", "11", "12", "AD", "UG", "TOTAL"
  ))
  expect_equal(grade_levels, expected_grades)
})

test_that("fetch_enr 2026 tidy state total enrollment is pinned", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  state_total <- tidy26 %>%
    filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)

  expect_equal(state_total, 473657)
})

test_that("fetch_enr 2026 tidy Clark County enrollment is pinned", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  clark <- tidy26 %>%
    filter(district_code == "02", is_district,
           subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(clark), 1)
  expect_equal(clark$n_students, 291587)
  expect_equal(clark$lea_code, "02")
  expect_equal(clark$lea_name, "Clark County School District")
  expect_equal(clark$district_name, "Clark County School District")
})

test_that("fetch_enr 2026 tidy Washoe County enrollment is pinned", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  washoe <- tidy26 %>%
    filter(district_code == "16", is_district,
           subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(washoe), 1)
  expect_equal(washoe$n_students, 63655)
  expect_equal(washoe$lea_name, "Washoe County School District")
})

test_that("fetch_enr 2026 tidy state demographics are pinned", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  state_demo <- tidy26 %>%
    filter(is_district, grade_level == "TOTAL") %>%
    group_by(subgroup) %>%
    summarize(total = sum(n_students, na.rm = TRUE), .groups = "drop")

  get_total <- function(sg) state_demo %>% filter(subgroup == sg) %>% pull(total)

  expect_equal(get_total("total_enrollment"), 473657)
  expect_equal(get_total("hispanic"), 217320)
  expect_equal(get_total("white"), 122852)
  expect_equal(get_total("black"), 58830)
  expect_equal(get_total("asian"), 27170)
  expect_equal(get_total("multiracial"), 37516)
  expect_equal(get_total("pacific_islander"), 6622)
  expect_equal(get_total("native_american"), 3347)
  expect_equal(get_total("female"), 230511)
  expect_equal(get_total("male"), 243022)
  expect_equal(get_total("free_reduced_lunch"), 389604)
  expect_equal(get_total("special_ed"), 70021)
  expect_equal(get_total("lep"), 65846)
  expect_equal(get_total("military"), 10710)
  expect_equal(get_total("homeless"), 9665)
  expect_equal(get_total("foster"), 2359)
  expect_equal(get_total("migrant"), 24)
})

test_that("fetch_enr 2026 tidy district count is correct", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  n_districts <- tidy26 %>%
    filter(is_district) %>%
    pull(district_code) %>%
    unique() %>%
    length()

  expect_equal(n_districts, 70)
})

test_that("fetch_enr 2026 tidy school count is correct", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  n_schools <- tidy26 %>%
    filter(is_school, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    nrow()

  expect_equal(n_schools, 747)
})

test_that("fetch_enr 2026 tidy charter detection works", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  charter_schools <- tidy26 %>%
    filter(is_charter, is_school, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    nrow()

  expect_equal(charter_schools, 90)
})

test_that("fetch_enr 2026 tidy Clark schools count is pinned", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  clark_schools <- tidy26 %>%
    filter(district_code == "02", is_school,
           subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    nrow()

  expect_equal(clark_schools, 369)
})

test_that("fetch_enr 2026 tidy Washoe schools count is pinned", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  washoe_schools <- tidy26 %>%
    filter(district_code == "16", is_school,
           subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    nrow()

  expect_equal(washoe_schools, 117)
})

test_that("fetch_enr 2026 tidy pct is correct for total_enrollment", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  total_rows <- tidy26 %>% filter(subgroup == "total_enrollment")
  expect_true(all(total_rows$pct == 1.0))
})

test_that("fetch_enr 2026 tidy pct is in valid range for non-total subgroups", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  non_total <- tidy26 %>% filter(subgroup != "total_enrollment")
  expect_true(all(non_total$pct >= 0 & non_total$pct <= 1, na.rm = TRUE))
})

test_that("fetch_enr 2026 tidy boolean flags are correct types", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  expect_type(tidy26$is_state, "logical")
  expect_type(tidy26$is_district, "logical")
  expect_type(tidy26$is_school, "logical")
  expect_type(tidy26$is_charter, "logical")
})

test_that("fetch_enr 2026 tidy boolean flag counts are pinned", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  expect_equal(sum(tidy26$is_state, na.rm = TRUE), 0)
  expect_equal(sum(tidy26$is_district, na.rm = TRUE), 957)
  expect_equal(sum(tidy26$is_school, na.rm = TRUE), 53805)
  expect_equal(sum(tidy26$is_charter, na.rm = TRUE), 8748)
})

test_that("fetch_enr 2026 tidy aggregation_flag values are correct", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  agg_flags <- sort(unique(tidy26$aggregation_flag))
  expect_equal(agg_flags, c("campus", "district"))
})

test_that("fetch_enr 2026 tidy Washoe demographics are pinned", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  washoe <- tidy26 %>%
    filter(district_code == "16", is_district, grade_level == "TOTAL")

  get_n <- function(sg) washoe %>% filter(subgroup == sg) %>% pull(n_students)

  expect_equal(get_n("total_enrollment"), 63655)
  expect_equal(get_n("white"), 24839)
  expect_equal(get_n("black"), 1806)
  expect_equal(get_n("hispanic"), 28488)
  expect_equal(get_n("asian"), 2774)
  expect_equal(get_n("native_american"), 622)
  expect_equal(get_n("pacific_islander"), 1036)
  expect_equal(get_n("multiracial"), 4090)
  expect_equal(get_n("female"), 30579)
  expect_equal(get_n("male"), 33049)
  expect_equal(get_n("free_reduced_lunch"), 39010)
  expect_equal(get_n("special_ed"), 10537)
  expect_equal(get_n("lep"), 9229)
  expect_equal(get_n("foster"), 265)
  expect_equal(get_n("military"), 664)
  expect_equal(get_n("homeless"), 906)
})

test_that("fetch_enr 2026 tidy per-grade school totals are pinned", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  grade_totals <- tidy26 %>%
    filter(is_school, subgroup == "total_enrollment", grade_level != "TOTAL") %>%
    group_by(grade_level) %>%
    summarize(total = sum(n_students, na.rm = TRUE), .groups = "drop")

  get_total <- function(gl) grade_totals %>% filter(grade_level == gl) %>% pull(total)

  expect_equal(get_total("PK"), 13852)
  expect_equal(get_total("K"), 30490)
  expect_equal(get_total("01"), 31486)
  expect_equal(get_total("02"), 30265)
  expect_equal(get_total("03"), 33417)
  expect_equal(get_total("04"), 35890)
  expect_equal(get_total("05"), 34847)
  expect_equal(get_total("06"), 35814)
  expect_equal(get_total("07"), 36276)
  expect_equal(get_total("08"), 36577)
  expect_equal(get_total("09"), 37251)
  expect_equal(get_total("10"), 37010)
  expect_equal(get_total("11"), 37217)
  expect_equal(get_total("12"), 37730)
  expect_equal(get_total("AD"), 4381)
  expect_equal(get_total("UG"), 1154)
})


# ==============================================================================
# fetch_enr() — 2026 wide format (tidy=FALSE)
# ==============================================================================

test_that("fetch_enr 2026 wide returns correct dimensions", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)

  expect_true(is.data.frame(wide26))
  expect_equal(nrow(wide26), 5195)
  expect_equal(ncol(wide26), 26)
})

test_that("fetch_enr 2026 wide has correct column names", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)

  expected_cols <- c(
    "end_year", "lea_code", "lea_name",
    "district_code", "district_name",
    "school_code", "school_name",
    "entity_type", "grade_level",
    "enrollment_total", "female", "male",
    "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "free_reduced_lunch", "special_ed", "lep",
    "migrant", "foster", "military", "homeless"
  )
  expect_equal(names(wide26), expected_cols)
})

test_that("fetch_enr 2026 wide column types are correct", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)

  expect_type(wide26$end_year, "double")
  expect_type(wide26$lea_code, "character")
  expect_type(wide26$lea_name, "character")
  expect_type(wide26$district_code, "character")
  expect_type(wide26$district_name, "character")
  expect_type(wide26$school_code, "character")
  expect_type(wide26$school_name, "character")
  expect_type(wide26$entity_type, "character")
  expect_type(wide26$grade_level, "character")
  expect_type(wide26$enrollment_total, "double")
  expect_type(wide26$female, "double")
  expect_type(wide26$male, "double")
  expect_type(wide26$white, "double")
  expect_type(wide26$black, "double")
  expect_type(wide26$hispanic, "double")
  expect_type(wide26$asian, "double")
  expect_type(wide26$pacific_islander, "double")
  expect_type(wide26$native_american, "double")
  expect_type(wide26$multiracial, "double")
})

test_that("fetch_enr 2026 wide district total matches tidy", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  wide_total <- wide26 %>%
    filter(entity_type == "District", grade_level == "TOTAL") %>%
    summarize(total = sum(enrollment_total, na.rm = TRUE)) %>%
    pull(total)

  tidy_total <- tidy26 %>%
    filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)

  expect_equal(wide_total, tidy_total)
  expect_equal(wide_total, 473657)
})

test_that("fetch_enr 2026 wide and tidy enrollment sums match", {
  wide26 <- nvschooldata::fetch_enr(2026, tidy = FALSE, use_cache = TRUE)
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  wide_sum <- sum(wide26$enrollment_total, na.rm = TRUE)
  tidy_sum <- tidy26 %>%
    filter(subgroup == "total_enrollment") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)

  expect_equal(wide_sum, tidy_sum)
  expect_equal(wide_sum, 1420971)
})


# ==============================================================================
# fetch_enr() — 2024 modern format
# ==============================================================================

test_that("fetch_enr 2024 tidy returns correct data", {
  tidy24 <- nvschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_true(is.data.frame(tidy24))
  expect_equal(nrow(tidy24), 54369)
  expect_equal(unique(tidy24$end_year), 2024)

  state_total <- tidy24 %>%
    filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)

  expect_equal(state_total, 485570)
})

test_that("fetch_enr 2024 Clark County enrollment is pinned", {
  tidy24 <- nvschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  clark24 <- tidy24 %>%
    filter(district_code == "02", is_district,
           subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(clark24$n_students, 309397)
})


# ==============================================================================
# fetch_enr() — 2023, 2022, 2021 modern format
# ==============================================================================

test_that("fetch_enr 2023 state total is pinned", {
  tidy23 <- nvschooldata::fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  expect_equal(nrow(tidy23), 49579)
  expect_equal(unique(tidy23$end_year), 2023)

  state_total <- tidy23 %>%
    filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)

  expect_equal(state_total, 489597)
})

test_that("fetch_enr 2022 state total is pinned", {
  tidy22 <- nvschooldata::fetch_enr(2022, tidy = TRUE, use_cache = TRUE)

  expect_equal(unique(tidy22$end_year), 2022)
  expect_equal(nrow(tidy22), 39417)

  state_total <- tidy22 %>%
    filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)

  expect_equal(state_total, 492338)
})

test_that("fetch_enr 2021 state total is pinned", {
  tidy21 <- nvschooldata::fetch_enr(2021, tidy = TRUE, use_cache = TRUE)

  expect_equal(unique(tidy21$end_year), 2021)
  expect_equal(nrow(tidy21), 38849)

  state_total <- tidy21 %>%
    filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)

  expect_equal(state_total, 486633)
})


# ==============================================================================
# fetch_enr() — 2019 intermediate format
# ==============================================================================

test_that("fetch_enr 2019 intermediate format returns correct structure", {
  tidy19 <- nvschooldata::fetch_enr(2019, tidy = TRUE, use_cache = TRUE)

  expect_true(is.data.frame(tidy19))
  expect_equal(nrow(tidy19), 33941)
  expect_equal(unique(tidy19$end_year), 2019)
  expect_equal(ncol(tidy19), 16)
})

test_that("fetch_enr 2019 has reduced subgroup set (no special populations)", {
  tidy19 <- nvschooldata::fetch_enr(2019, tidy = TRUE, use_cache = TRUE)

  subgroups <- sort(unique(tidy19$subgroup))
  expected <- sort(c(
    "total_enrollment", "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "female", "male"
  ))
  expect_equal(subgroups, expected)

  # Special populations not in 2019
  expect_false("free_reduced_lunch" %in% subgroups)
  expect_false("special_ed" %in% subgroups)
  expect_false("lep" %in% subgroups)
})

test_that("fetch_enr 2019 state total is pinned", {
  tidy19 <- nvschooldata::fetch_enr(2019, tidy = TRUE, use_cache = TRUE)

  state_total <- tidy19 %>%
    filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)

  expect_equal(state_total, 498616)
})

test_that("fetch_enr 2019 has expected grade levels", {
  tidy19 <- nvschooldata::fetch_enr(2019, tidy = TRUE, use_cache = TRUE)

  grade_levels <- sort(unique(tidy19$grade_level))
  expected_grades <- sort(c(
    "PK", "K", "01", "02", "03", "04", "05", "06",
    "07", "08", "09", "10", "11", "12", "AD", "UG", "TOTAL"
  ))
  expect_equal(grade_levels, expected_grades)
})


# ==============================================================================
# fetch_enr() — Error handling
# ==============================================================================

test_that("fetch_enr errors on unavailable year 2025", {
  expect_error(
    nvschooldata::fetch_enr(2025, use_cache = TRUE),
    "end_year must be between"
  )
})

test_that("fetch_enr errors on year 2000 (out of range)", {
  expect_error(
    nvschooldata::fetch_enr(2000, use_cache = TRUE),
    "end_year must be between"
  )
})

test_that("fetch_enr errors on year 2030 (future)", {
  expect_error(
    nvschooldata::fetch_enr(2030, use_cache = TRUE),
    "end_year must be between"
  )
})


# ==============================================================================
# fetch_enr_multi()
# ==============================================================================

test_that("fetch_enr_multi returns combined data for two years", {
  multi <- nvschooldata::fetch_enr_multi(c(2024, 2026), tidy = TRUE, use_cache = TRUE)

  expect_true(is.data.frame(multi))
  expect_equal(sort(unique(multi$end_year)), c(2024, 2026))
  expect_equal(nrow(multi), 54369 + 54762)  # 2024 + 2026 row counts
})

test_that("fetch_enr_multi preserves year-specific data", {
  multi <- nvschooldata::fetch_enr_multi(c(2024, 2026), tidy = TRUE, use_cache = TRUE)

  # 2024 state total
  state_24 <- multi %>%
    filter(end_year == 2024, is_district, subgroup == "total_enrollment",
           grade_level == "TOTAL") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)
  expect_equal(state_24, 485570)

  # 2026 state total
  state_26 <- multi %>%
    filter(end_year == 2026, is_district, subgroup == "total_enrollment",
           grade_level == "TOTAL") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)
  expect_equal(state_26, 473657)
})

test_that("fetch_enr_multi errors on invalid year in vector", {
  expect_error(
    nvschooldata::fetch_enr_multi(c(2024, 2025), use_cache = TRUE),
    "Invalid years: 2025"
  )
})

test_that("fetch_enr_multi with tidy=FALSE returns wide format", {
  multi_wide <- nvschooldata::fetch_enr_multi(c(2024, 2026), tidy = FALSE, use_cache = TRUE)

  expect_true("enrollment_total" %in% names(multi_wide))
  expect_false("subgroup" %in% names(multi_wide))
  expect_equal(sort(unique(multi_wide$end_year)), c(2024, 2026))
})


# ==============================================================================
# get_state_enrollment()
# ==============================================================================

test_that("get_state_enrollment returns correct structure for multiple years", {
  state_enr <- nvschooldata::get_state_enrollment(c(2024, 2026), use_cache = TRUE)

  expect_true(is.data.frame(state_enr))
  expect_equal(nrow(state_enr), 2)
  expect_true(all(c("end_year", "n_districts", "total_enrollment") %in% names(state_enr)))
})

test_that("get_state_enrollment values are pinned", {
  state_enr <- nvschooldata::get_state_enrollment(c(2024, 2026), use_cache = TRUE)

  row_24 <- state_enr %>% filter(end_year == 2024)
  expect_equal(row_24$total_enrollment, 485570)
  expect_equal(row_24$n_districts, 63)

  row_26 <- state_enr %>% filter(end_year == 2026)
  expect_equal(row_26$total_enrollment, 473657)
  expect_equal(row_26$n_districts, 70)
})

test_that("get_state_enrollment for single year works", {
  state_26 <- nvschooldata::get_state_enrollment(2026, use_cache = TRUE)

  expect_equal(nrow(state_26), 1)
  expect_equal(state_26$end_year, 2026)
  expect_equal(state_26$total_enrollment, 473657)
  expect_equal(state_26$n_districts, 70)
})


# ==============================================================================
# filter_district()
# ==============================================================================

test_that("filter_district returns Clark County data with schools", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)
  clark <- nvschooldata::filter_district(tidy26, "02")

  expect_true(nrow(clark) > 0)
  expect_equal(nrow(clark), 26837)
  expect_true(all(clark$district_code == "02" | clark$lea_code == "02"))
})

test_that("filter_district returns Clark County district-only data", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)
  clark_dist <- nvschooldata::filter_district(tidy26, "02", include_schools = FALSE)

  expect_equal(nrow(clark_dist), 16)
  expect_true(all(clark_dist$entity_type == "District"))
})

test_that("filter_district accepts numeric code", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)
  clark <- nvschooldata::filter_district(tidy26, 2)

  expect_true(nrow(clark) > 0)
  expect_equal(nrow(clark), 26837)
})


# ==============================================================================
# filter_county()
# ==============================================================================

test_that("filter_county returns Clark County data", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)
  clark <- nvschooldata::filter_county(tidy26, "Clark")

  expect_true(nrow(clark) > 0)
  expect_equal(nrow(clark), 26837)
})

test_that("filter_county is case-insensitive", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)
  clark_lower <- nvschooldata::filter_county(tidy26, "clark")
  clark_upper <- nvschooldata::filter_county(tidy26, "CLARK")

  expect_equal(nrow(clark_lower), nrow(clark_upper))
})


# ==============================================================================
# enr_grade_aggs()
# ==============================================================================

test_that("enr_grade_aggs returns K8, HS, K12 grade levels", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)
  aggs <- nvschooldata::enr_grade_aggs(tidy26)

  expect_true(is.data.frame(aggs))
  expect_equal(sort(unique(aggs$grade_level)), c("HS", "K12", "K8"))
  expect_equal(nrow(aggs), 1503)
})

test_that("enr_grade_aggs school-level totals are pinned", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)
  aggs <- nvschooldata::enr_grade_aggs(tidy26)

  k8_total <- aggs %>%
    filter(is_school, grade_level == "K8") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)
  expect_equal(k8_total, 305062)

  hs_total <- aggs %>%
    filter(is_school, grade_level == "HS") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)
  expect_equal(hs_total, 149208)

  k12_total <- aggs %>%
    filter(is_school, grade_level == "K12") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)
  expect_equal(k12_total, 454270)
})

test_that("enr_grade_aggs K12 = K8 + HS", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)
  aggs <- nvschooldata::enr_grade_aggs(tidy26)

  # School level
  school_aggs <- aggs %>% filter(is_school)

  k8 <- school_aggs %>%
    filter(grade_level == "K8") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)

  hs <- school_aggs %>%
    filter(grade_level == "HS") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)

  k12 <- school_aggs %>%
    filter(grade_level == "K12") %>%
    summarize(total = sum(n_students, na.rm = TRUE)) %>%
    pull(total)

  expect_equal(k8 + hs, k12)
})


# ==============================================================================
# import_local_enrollment() error handling
# ==============================================================================

test_that("import_local_enrollment errors on nonexistent file", {
  expect_error(
    nvschooldata::import_local_enrollment("/nonexistent/file.xlsx", 2024),
    "File not found"
  )
})


# ==============================================================================
# CHARTER school identification via SPCSA
# ==============================================================================

test_that("charter schools are identified via SPCSA in lea_name", {
  tidy26 <- nvschooldata::fetch_enr(2026, tidy = TRUE, use_cache = TRUE)

  spcsa_rows <- tidy26 %>%
    filter(grepl("SPCSA", lea_name, ignore.case = TRUE))

  expect_true(all(spcsa_rows$is_charter))
})
