# Import local enrollment Excel files

Imports enrollment data from locally downloaded Excel files. Use this
when automatic downloads fail.

## Usage

``` r
import_local_enrollment(file_path, end_year)
```

## Arguments

- file_path:

  Path to enrollment Excel file

- end_year:

  School year end (e.g., 2024 for 2023-24)

## Value

Raw data list with enrollment data

## Examples

``` r
if (FALSE) { # \dontrun{
# After downloading file from NDE portal:
enr_raw <- import_local_enrollment(
  file_path = "~/Downloads/2024_2025_school_year_validation_day_student_counts.xlsx",
  end_year = 2025
)
} # }
```
