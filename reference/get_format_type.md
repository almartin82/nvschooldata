# Get the format type for a given year

Nevada enrollment data comes in different formats by year:

- "legacy" (2016-2017): Separate sheets per district

- "intermediate" (2018-2020): Consolidated sheets, simpler column names

- "modern" (2021+): Full LEA codes, Master District codes, complete
  structure

## Usage

``` r
get_format_type(end_year)
```

## Arguments

- end_year:

  School year end

## Value

Character string indicating format type
