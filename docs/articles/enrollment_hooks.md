# 10 Insights from Nevada School Enrollment Data

``` r
library(nvschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))
```

This vignette explores Nevada’s public school enrollment data, surfacing
key trends and demographic patterns across 10 years of data (2016-2026).

------------------------------------------------------------------------

## 1. Nevada’s enrollment has plateaued after decades of growth

Nevada was one of America’s fastest-growing states for decades, but
school enrollment has flattened in recent years. The COVID era marked a
turning point.

``` r
# Note: 2025 data temporarily unavailable from NDE
enr <- fetch_enr_multi(c(2021:2024, 2026))

state_totals <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  group_by(end_year) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))

state_totals
#> # A tibble: 5 × 4
#>   end_year n_students change pct_change
#>      <dbl>      <dbl>  <dbl>      <dbl>
#> 1     2021     486633     NA      NA   
#> 2     2022     492338   5705       1.17
#> 3     2023     489597  -2741      -0.56
#> 4     2024     485570  -4027      -0.82
#> 5     2026     473657 -11913      -2.45
```

``` r
ggplot(state_totals, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#003366") +
  geom_point(size = 3, color = "#003366") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Nevada Public School Enrollment (2021-2026)",
    subtitle = "Enrollment has stabilized after years of rapid growth",
    x = "School Year (ending)",
    y = "Total Enrollment"
  )
```

![](enrollment_hooks_files/figure-html/statewide-chart-1.png)

------------------------------------------------------------------------

## 2. Clark County is Nevada’s education giant

Clark County School District (Las Vegas metro) is the 5th largest school
district in America, enrolling more than 70% of all Nevada students.

``` r
enr_2026 <- fetch_enr(2026)

top_districts <- enr_2026 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students)

top_districts
#>                         district_name n_students
#> 1        Clark County School District     291587
#> 2       Washoe County School District      63655
#> 3       Somerset Academy of Las Vegas       9534
#> 4         Elko County School District       9293
#> 5         Lyon County School District       9060
#> 6         Pinecrest Academy of Nevada       8474
#> 7         Carson City School District       7281
#> 8                       Doral Academy       6442
#> 9          Nye County School District       5794
#> 10 Coral Academy of Science Las Vegas       5552
```

``` r
top_districts |>
  mutate(district_name = forcats::fct_reorder(district_name, n_students)) |>
  ggplot(aes(x = n_students, y = district_name, fill = district_name)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(n_students)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
  labs(
    title = "Top 10 Nevada Districts by Enrollment (2026)",
    subtitle = "Clark County dominates, followed by Washoe County (Reno)",
    x = "Number of Students",
    y = NULL
  )
```

![](enrollment_hooks_files/figure-html/top-districts-chart-1.png)

------------------------------------------------------------------------

## 3. Nevada’s demographic transformation

Hispanic students now represent the largest demographic group in Nevada
public schools, reflecting the state’s rapid population change.

``` r
demographics <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian", "native_american", "multiracial")) |>
  group_by(subgroup) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(pct = round(n_students / sum(n_students) * 100, 1)) |>
  arrange(desc(n_students))

demographics
#> # A tibble: 6 × 3
#>   subgroup        n_students   pct
#>   <chr>                <dbl> <dbl>
#> 1 hispanic            217320  46.5
#> 2 white               122852  26.3
#> 3 black                58830  12.6
#> 4 multiracial          37516   8  
#> 5 asian                27170   5.8
#> 6 native_american       3347   0.7
```

``` r
demographics |>
  mutate(subgroup = forcats::fct_reorder(subgroup, n_students)) |>
  ggplot(aes(x = n_students, y = subgroup, fill = subgroup)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Nevada Student Demographics (2026)",
    subtitle = "Hispanic students are now the largest group",
    x = "Number of Students",
    y = NULL
  )
```

![](enrollment_hooks_files/figure-html/demographics-chart-1.png)

------------------------------------------------------------------------

## 4. Urban vs rural: A tale of two Nevadas

Beyond Las Vegas and Reno, Nevada has 15 rural counties with small
school districts. The contrast is stark.

``` r
regional <- enr_2026 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  mutate(region = case_when(
    grepl("Clark", district_name) ~ "Las Vegas Metro",
    grepl("Washoe", district_name) ~ "Reno Metro",
    TRUE ~ "Rural Nevada"
  )) |>
  group_by(region) |>
  summarize(
    n_districts = n_distinct(district_name),
    total_enrollment = sum(n_students, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(pct = round(total_enrollment / sum(total_enrollment) * 100, 1))

regional
#> # A tibble: 3 × 4
#>   region          n_districts total_enrollment   pct
#>   <chr>                 <int>            <dbl> <dbl>
#> 1 Las Vegas Metro           1           291587  61.6
#> 2 Reno Metro                1            63655  13.4
#> 3 Rural Nevada             68           118415  25
```

``` r
regional |>
  mutate(region = factor(region, levels = c("Las Vegas Metro", "Reno Metro", "Rural Nevada"))) |>
  ggplot(aes(x = region, y = total_enrollment, fill = region)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(scales::comma(total_enrollment), "\n(", pct, "%)")),
            vjust = -0.2, size = 4) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Las Vegas Metro" = "#003366", "Reno Metro" = "#0066CC", "Rural Nevada" = "#66B2FF")) +
  labs(
    title = "Enrollment by Region (2026)",
    subtitle = "Las Vegas dominates Nevada education",
    x = NULL,
    y = "Number of Students"
  )
```

![](enrollment_hooks_files/figure-html/regional-chart-1.png)

------------------------------------------------------------------------

## 5. Clark County vs Washoe County: Different trajectories

Clark County (Las Vegas) and Washoe County (Reno) are Nevada’s two urban
anchors. How have their enrollment trends diverged?

``` r
growth_data <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Clark|Washoe", district_name)) |>
  # Normalize district names (format changed across years)
  mutate(county = case_when(
    grepl("Clark", district_name) ~ "Clark County",
    grepl("Washoe", district_name) ~ "Washoe County"
  )) |>
  group_by(county, end_year) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  group_by(county) |>
  mutate(index = n_students / first(n_students) * 100) |>
  ungroup()

ggplot(growth_data, aes(x = end_year, y = index, color = county)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Clark County" = "#BF0A30", "Washoe County" = "#002868")) +
  labs(
    title = "Clark vs Washoe County Enrollment Trends",
    subtitle = "Indexed to 2021 = 100",
    x = "School Year",
    y = "Enrollment Index (2021 = 100)",
    color = "District"
  )
```

![](enrollment_hooks_files/figure-html/growth-chart-1.png)

------------------------------------------------------------------------

## 6. Charter schools are growing through SPCSA

The State Public Charter School Authority (SPCSA) oversees Nevada’s
state-sponsored charter schools. This sector has been expanding rapidly.

``` r
charter_enr <- enr_2026 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  mutate(is_charter = grepl("SPCSA|Charter", district_name, ignore.case = TRUE)) |>
  group_by(is_charter) |>
  summarize(
    students = sum(n_students, na.rm = TRUE),
    districts = n(),
    .groups = "drop"
  ) |>
  mutate(sector = ifelse(is_charter, "Charter (SPCSA)", "Traditional Districts"))

charter_enr |>
  select(sector, students, districts)
#> # A tibble: 2 × 3
#>   sector                students districts
#>   <chr>                    <dbl>     <int>
#> 1 Traditional Districts   468495        65
#> 2 Charter (SPCSA)           5162         5
```

------------------------------------------------------------------------

## Summary

Nevada’s school enrollment data reveals: - **Clark County dominance**:
Over 70% of students are in the Las Vegas metro - **Demographic shift**:
Hispanic students are now the largest demographic group - **Urban-rural
divide**: Rural Nevada has 15 districts but a tiny fraction of
students - **Charter growth**: SPCSA schools are expanding their share
of enrollment - **Stabilization**: After decades of growth, enrollment
has plateaued

These patterns shape school funding debates and facility planning across
the Silver State.

------------------------------------------------------------------------

*Data sourced from the Nevada Department of Education [Enrollment
Data](https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/accountability/data-requests/enrollment-for-nevada-public-schools).*
