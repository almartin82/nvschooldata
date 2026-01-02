# Fetch enrollment data for a range of years

Convenience function to download enrollment data for a range of years.
Results are combined into a single data frame.

## Usage

``` r
fetch_enr_range(start_year, end_year, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- start_year:

  First year to fetch

- end_year:

  Last year to fetch

- tidy:

  If TRUE (default), returns tidy format

- use_cache:

  If TRUE (default), uses cached data when available

## Value

Combined data frame with enrollment data for all requested years

## Examples

``` r
if (FALSE) { # \dontrun{
# Get 5 years of enrollment data
enr_history <- fetch_enr_range(2021, 2025)
} # }
```
