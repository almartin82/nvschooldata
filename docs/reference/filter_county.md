# Filter enrollment data by county

Convenience function to filter enrollment data to a specific Nevada
county. Matches on district name containing the county name.

## Usage

``` r
filter_county(df, county_name)
```

## Arguments

- df:

  Enrollment dataframe

- county_name:

  Name of Nevada county (case-insensitive)

## Value

Filtered data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# Get Clark County schools
clark <- fetch_enr(2025) |> filter_county("Clark")
} # }
```
