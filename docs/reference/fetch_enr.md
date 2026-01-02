# Fetch Nevada enrollment data

Downloads and processes enrollment data from the Nevada Department of
Education. Data is available from the NDE website.

## Usage

``` r
fetch_enr(end_year, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  A school year. Year is the end of the academic year - eg 2024-25
  school year is year '2025'. Valid values are 2016 through 2026.

- tidy:

  If TRUE (default), returns data in long (tidy) format with subgroup
  column. If FALSE, returns wide format with one row per entity/grade.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from NDE.

## Value

Data frame with enrollment data. Tidy format includes columns:

- end_year: School year end (e.g., 2025 for 2024-25)

- lea_code: Local Education Agency code

- lea_name: LEA name

- district_code: Master District code

- district_name: District name

- school_code: School code (NA for district-level)

- school_name: School name (NA for district-level)

- entity_type: "District" or "School"

- grade_level: Grade level or "TOTAL"

- subgroup: Demographic or population subgroup

- n_students: Student count

- pct: Percentage of total enrollment

## Details

Nevada provides enrollment data from 2015-16 through the current year.
Data formats vary by year:

- 2016-2017: Legacy format (limited data extraction)

- 2018-2020: Intermediate format (consolidated data)

- 2021-2026: Modern format (full school/district data with demographics)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get 2025 enrollment data (2024-25 school year)
enr_2025 <- fetch_enr(2025)

# Get wide format (one row per entity/grade)
enr_wide <- fetch_enr(2025, tidy = FALSE)

# Force fresh download (ignore cache)
enr_fresh <- fetch_enr(2025, use_cache = FALSE)

# Get multiple years
enr_multi <- purrr::map_df(2021:2025, fetch_enr)
} # }
```
