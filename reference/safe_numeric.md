# Convert to numeric, handling suppression markers

NDE uses various markers for suppressed data (\*, \<10, -, etc.) and may
use commas in large numbers.

## Usage

``` r
safe_numeric(x)
```

## Arguments

- x:

  Vector to convert

## Value

Numeric vector with NA for non-numeric values
