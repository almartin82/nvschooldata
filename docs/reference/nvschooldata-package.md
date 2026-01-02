# nvschooldata: Fetch and Process Nevada School Data

The nvschooldata package provides functions to download and process
school data from the Nevada Department of Education (NDE).

## Main Functions

- [`fetch_enr`](https://almartin82.github.io/nvschooldata/reference/fetch_enr.md):
  Download enrollment data for a specific year

- [`fetch_enr_range`](https://almartin82.github.io/nvschooldata/reference/fetch_enr_range.md):
  Download enrollment data for multiple years

- [`get_state_enrollment`](https://almartin82.github.io/nvschooldata/reference/get_state_enrollment.md):
  Get statewide enrollment summary

## Data Processing

- [`tidy_enr`](https://almartin82.github.io/nvschooldata/reference/tidy_enr.md):
  Convert wide data to tidy (long) format

- [`id_enr_aggs`](https://almartin82.github.io/nvschooldata/reference/id_enr_aggs.md):
  Add aggregation level flags

- [`enr_grade_aggs`](https://almartin82.github.io/nvschooldata/reference/enr_grade_aggs.md):
  Create grade-level aggregates

## Filtering

- [`filter_district`](https://almartin82.github.io/nvschooldata/reference/filter_district.md):
  Filter by district code

- [`filter_county`](https://almartin82.github.io/nvschooldata/reference/filter_county.md):
  Filter by county name

## Cache Management

- [`cache_status`](https://almartin82.github.io/nvschooldata/reference/cache_status.md):
  Show cached data status

- [`clear_enr_cache`](https://almartin82.github.io/nvschooldata/reference/clear_enr_cache.md):
  Clear cached data

## Data Source

Data is downloaded from the Nevada Department of Education website at
<https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/accountability/data-requests/enrollment-for-nevada-public-schools>.

Nevada uses several identifier systems:

- Local Education Agency (LEA) Code: District identifier

- Master District Code: Alternative district identifier

- School Code: School identifier within a district

## See also

Useful links:

- <https://almartin82.github.io/nvschooldata>

- <https://github.com/almartin82/nvschooldata>

- Report bugs at <https://github.com/almartin82/nvschooldata/issues>

## Author

**Maintainer**: Al Martin <almartin@example.com>
