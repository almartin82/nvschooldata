# Custom Enrollment Grade Level Aggregates

Creates aggregations for common grade groupings: K-8, 9-12 (HS), K-12.
Useful for comparing elementary and secondary enrollment patterns.

## Usage

``` r
enr_grade_aggs(df)
```

## Arguments

- df:

  A tidy enrollment df from tidy_enr

## Value

df of aggregated enrollment data with K8, HS, K12 grade levels

## Examples

``` r
if (FALSE) { # \dontrun{
tidy_data <- fetch_enr(2025)
grade_aggregates <- enr_grade_aggs(tidy_data)
} # }
```
