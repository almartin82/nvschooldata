# Identify enrollment aggregation levels

Adds boolean flags to identify state, district, and school level
records.

## Usage

``` r
id_enr_aggs(df)
```

## Arguments

- df:

  Enrollment dataframe, output of tidy_enr

## Value

data.frame with boolean aggregation flags

## Examples

``` r
if (FALSE) { # \dontrun{
tidy_data <- fetch_enr(2025)
with_flags <- id_enr_aggs(tidy_data)
} # }
```
