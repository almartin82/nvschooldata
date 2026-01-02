# Filter enrollment data by district

Convenience function to filter enrollment data to a specific district
using the LEA code or district code.

## Usage

``` r
filter_district(df, code, include_schools = TRUE)
```

## Arguments

- df:

  Enrollment dataframe

- code:

  District code (LEA code or Master District Code)

- include_schools:

  If TRUE (default), include school-level data. If FALSE, only return
  district-level aggregates.

## Value

Filtered data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# Get Clark County School District (code 02)
ccsd <- fetch_enr(2025) |> filter_district("02")
} # }
```
