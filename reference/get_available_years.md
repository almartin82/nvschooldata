# Get available years for enrollment data

Returns information about the range of school years for which enrollment
data is available from the Nevada Department of Education.

## Usage

``` r
get_available_years()
```

## Value

A list with three elements:

- min_year:

  The earliest available school year end (e.g., 2016 = 2015-16)

- max_year:

  The most recent available school year end (e.g., 2026 = 2025-26)

- description:

  A human-readable description of data availability

## Examples

``` r
years <- get_available_years()
years$min_year
#> [1] 2016
years$max_year
#> [1] 2024
```
