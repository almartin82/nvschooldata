# List available enrollment data years

Returns a vector of years for which enrollment data is available from
the Nevada Department of Education.

## Usage

``` r
list_enr_years()
```

## Value

Integer vector of available years

## Details

Nevada provides enrollment data from 2015-16 (end year 2016) through
2025-26 (end year 2026), except 2024-25 (end year 2025) which is
currently unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
available_years <- list_enr_years()
} # }
```
