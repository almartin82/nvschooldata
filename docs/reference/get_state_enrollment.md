# Get Nevada statewide enrollment summary

Returns a summary of statewide enrollment totals by year.

## Usage

``` r
get_state_enrollment(end_year, use_cache = TRUE)
```

## Arguments

- end_year:

  School year end (or vector of years)

- use_cache:

  If TRUE (default), uses cached data

## Value

Data frame with statewide enrollment by year

## Examples

``` r
if (FALSE) { # \dontrun{
# Get statewide summary for 2025
state_summary <- get_state_enrollment(2025)

# Get 5-year trend
state_trend <- get_state_enrollment(2021:2025)
} # }
```
