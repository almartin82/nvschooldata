# Fetch Nevada school directory data

Downloads and processes school directory data from the Nevada Department
of Education. This includes all public schools with contact information
and administrator names.

## Usage

``` r
fetch_directory(end_year = NULL, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  Currently unused. The directory data represents current schools and is
  not year-specific. Included for API consistency with other fetch
  functions.

- tidy:

  If TRUE (default), returns data in a standardized format with
  consistent column names. If FALSE, returns raw column names from NDE.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from NDE.

## Value

A tibble with school directory data. Columns include:

- `state_school_id`: NDE state school code

- `state_district_id`: NDE master district code

- `school_name`: School name

- `school_type`: Type of school (e.g., "RG: Regular", "AL: Alternative")

- `grades_served`: Grade levels offered (low to high)

- `address`: Mailing address

- `city`: City

- `state`: State (always "NV")

- `zip`: ZIP code

- `phone`: Phone number

- `principal_name`: Principal name

- `principal_email`: Principal email address

- `charter_status`: Charter indicator (YES/NO)

- `operational_status`: Operational status

- `website`: School website URL

## Details

The directory data is downloaded as an Excel file from the NDE website.
This data represents the current state of Nevada schools and is updated
periodically by NDE.

Note: The directory file contains only school-level data. District
information is derived from the Master District Code.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get school directory data
dir_data <- fetch_directory()

# Get raw format (original NDE column names)
dir_raw <- fetch_directory(tidy = FALSE)

# Force fresh download (ignore cache)
dir_fresh <- fetch_directory(use_cache = FALSE)

# Filter to active schools only
library(dplyr)
active_schools <- dir_data |>
  filter(operational_status == "OP: Open")

# Find all charter schools
charters <- dir_data |>
  filter(charter_status == "YES")
} # }
```
