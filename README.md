# nvschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/nvschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/nvschooldata/actions/workflows/R-CMD-check.yaml)
[![Python Tests](https://github.com/almartin82/nvschooldata/actions/workflows/python-test.yaml/badge.svg)](https://github.com/almartin82/nvschooldata/actions/workflows/python-test.yaml)
[![pkgdown](https://github.com/almartin82/nvschooldata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/almartin82/nvschooldata/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Fetch and analyze Nevada school enrollment data from [NVDOE](https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/accountability/data-requests/enrollment-for-nevada-public-schools) in R or Python.

## Why nvschooldata?

This package is part of the [State Schooldata Project](https://github.com/almartin82/njschooldata), which provides simple, consistent interfaces for accessing state-published school data. The original [njschooldata](https://github.com/almartin82/njschooldata) package for New Jersey inspired this 50-state effort to make state education data accessible without manual downloads or data wrangling.

**What's available:** 10 years of enrollment data (2016-2026) covering 470,000+ students across 70 district-level entities (19 traditional county districts and 51 charter schools). Demographic breakdowns include race/ethnicity, gender, and special populations (EL, FRL, IEP).

---

## 15 Insights from Nevada Enrollment Data

### 1. Nevada's enrollment has plateaued after decades of growth

Nevada was one of America's fastest-growing states for decades, but school enrollment has flattened in recent years. The COVID era marked a turning point.

```r
library(nvschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

# Note: 2025 data temporarily unavailable from NDE
enr <- fetch_enr_multi(c(2021:2024, 2026), use_cache = TRUE)

state_totals <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  group_by(end_year) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))

stopifnot(nrow(state_totals) > 0)
state_totals
#> # A tibble: 5 x 4
#>   end_year n_students change pct_change
#>      <dbl>      <dbl>  <dbl>      <dbl>
#> 1     2021     486633     NA      NA
#> 2     2022     492338   5705       1.17
#> 3     2023     489597  -2741      -0.56
#> 4     2024     485570  -4027      -0.82
#> 5     2026     473657 -11913      -2.45
```

![Nevada statewide enrollment trends](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/statewide-chart-1.png)

---

### 2. Clark County is Nevada's education giant

Clark County School District (Las Vegas metro) is the 5th largest school district in America. Charter networks like Somerset Academy and Pinecrest also rank among the state's largest entities.

```r
enr_2026 <- fetch_enr(2026, use_cache = TRUE)

top_districts <- enr_2026 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students)

stopifnot(nrow(top_districts) > 0)
top_districts
#> # A tibble: 10 x 2
#>    district_name                      n_students
#>    <chr>                                   <dbl>
#>  1 Clark County School District           291587
#>  2 Washoe County School District           63655
#>  3 Somerset Academy of Las Vegas            9534
#>  4 Elko County School District              9293
#>  5 Lyon County School District              9060
#>  6 Pinecrest Academy of Nevada              8474
#>  7 Carson City School District              7281
#>  8 Doral Academy                            6442
#>  9 Nye County School District               5794
#> 10 Coral Academy of Science Las Vegas       5552
```

![Top Nevada districts](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

---

### 3. Nevada's demographic transformation

Hispanic students now represent the largest demographic group in Nevada public schools at 46.5%, reflecting the state's rapid population change.

```r
demographics <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian", "native_american", "multiracial")) |>
  group_by(subgroup) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(pct = round(n_students / sum(n_students) * 100, 1)) |>
  arrange(desc(n_students))

stopifnot(nrow(demographics) > 0)
demographics
#> # A tibble: 6 x 3
#>   subgroup        n_students   pct
#>   <chr>                <dbl> <dbl>
#> 1 hispanic            217320  46.5
#> 2 white               122852  26.3
#> 3 black                58830  12.6
#> 4 multiracial          37516   8.0
#> 5 asian                27170   5.8
#> 6 native_american       3347   0.7
```

![Nevada student demographics](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/demographics-chart-1.png)

---

### 4. Urban vs rural: A tale of two Nevadas

Beyond Las Vegas and Reno, Nevada has dozens of entities classified as districts, including charter schools. The enrollment contrast is stark.

```r
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

stopifnot(nrow(regional) > 0)
regional
#> # A tibble: 3 x 4
#>   region          n_districts total_enrollment   pct
#>   <chr>                 <int>            <dbl> <dbl>
#> 1 Las Vegas Metro           1           291587  61.6
#> 2 Reno Metro                1            63655  13.4
#> 3 Rural Nevada             68           118415  25.0
```

![Enrollment by region](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/regional-chart-1.png)

---

### 5. Clark County vs Washoe County: Different trajectories

Clark County (Las Vegas) and Washoe County (Reno) are Nevada's two urban anchors. Clark has lost 8.7% of enrollment since 2021 while Washoe has been more stable, declining only 2.1%.

```r
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
  mutate(index = round(n_students / first(n_students) * 100, 1)) |>
  ungroup()

stopifnot(nrow(growth_data) > 0)
growth_data
#> # A tibble: 10 x 4
#>    county        end_year n_students index
#>    <chr>            <dbl>      <dbl> <dbl>
#>  1 Clark County      2021     319293 100
#>  2 Clark County      2022     320245 100.3
#>  3 Clark County      2023     314372  98.5
#>  4 Clark County      2024     309397  96.9
#>  5 Clark County      2026     291587  91.3
#>  6 Washoe County     2021      64988 100
#>  7 Washoe County     2022      66541 102.4
#>  8 Washoe County     2023      64990 100
#>  9 Washoe County     2024      64755  99.6
#> 10 Washoe County     2026      63655  97.9
```

![Clark vs Washoe enrollment trends](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/growth-chart-1.png)

---

### 6. Nearly 15% of Nevada students attend charter schools

Nevada's charter sector has grown significantly. Using the package's `is_charter` flag, 51 charter entities enroll over 70,000 students -- nearly 15% of all public school enrollment.

```r
charter_enr <- enr_2026 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  group_by(is_charter) |>
  summarize(
    students = sum(n_students, na.rm = TRUE),
    districts = n(),
    .groups = "drop"
  ) |>
  mutate(sector = ifelse(is_charter, "Charter (SPCSA)", "Traditional Districts"))

stopifnot(nrow(charter_enr) > 0)
charter_enr |>
  select(sector, students, districts)
#> # A tibble: 2 x 3
#>   sector                students districts
#>   <chr>                    <dbl>     <int>
#> 1 Traditional Districts   403123        19
#> 2 Charter (SPCSA)          70534        51
```

![Traditional vs charter enrollment](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/charter-chart-1.png)

---

### 7. Washoe County: Nevada's second city

Washoe County (Reno-Sparks) is Nevada's second-largest district. While smaller than Clark County, it educates over 60,000 students.

```r
washoe_data <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Washoe", district_name)) |>
  group_by(end_year) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))

stopifnot(nrow(washoe_data) > 0)
washoe_data
#> # A tibble: 5 x 4
#>   end_year n_students change pct_change
#>      <dbl>      <dbl>  <dbl>      <dbl>
#> 1     2021      64988     NA      NA
#> 2     2022      66541   1553       2.39
#> 3     2023      64990  -1551      -2.33
#> 4     2024      64755   -235      -0.36
#> 5     2026      63655  -1100      -1.70
```

![Washoe County enrollment](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/washoe-chart-1.png)

---

### 8. Grade-level enrollment patterns

Tracking enrollment by grade reveals where schools are growing or shrinking. Grade-level breakdowns are available at the school level; district rows report only totals.

```r
grade_data <- enr_2026 |>
  filter(is_school, subgroup == "total_enrollment",
         !grade_level %in% c("TOTAL", "UG", "AD")) |>
  group_by(grade_level) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(grade_level = factor(grade_level, levels = c("PK", "K", sprintf("%02d", 1:12))))

stopifnot(nrow(grade_data) > 0)
grade_data
#> # A tibble: 14 x 2
#>    grade_level n_students
#>    <fct>            <dbl>
#>  1 PK               13852
#>  2 K                30490
#>  3 01               31486
#>  4 02               30265
#>  5 03               33417
#>  6 04               35890
#>  7 05               34847
#>  8 06               35814
#>  9 07               36276
#> 10 08               36577
#> 11 09               37251
#> 12 10               37010
#> 13 11               37217
#> 14 12               37730
```

![Enrollment by grade level](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/grade-chart-1.png)

---

### 9. Gender enrollment balance

How does the male/female ratio vary across Nevada? Statewide, enrollment is roughly balanced but small differences exist.

```r
gender_data <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL",
         subgroup %in% c("male", "female")) |>
  group_by(subgroup) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(pct = round(n_students / sum(n_students) * 100, 1))

stopifnot(nrow(gender_data) > 0)
gender_data
#> # A tibble: 2 x 3
#>   subgroup n_students   pct
#>   <chr>         <dbl> <dbl>
#> 1 female       230511  48.7
#> 2 male         243022  51.3
```

![Enrollment by gender](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/gender-chart-1.png)

---

### 10. Special populations across districts

English Learners, students with IEPs, and Free/Reduced Lunch eligible students represent key populations for educational policy. How do the two largest districts compare?

```r
special_pops <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL",
         subgroup %in% c("frl", "iep", "el"),
         grepl("Clark|Washoe", district_name)) |>
  mutate(county = ifelse(grepl("Clark", district_name), "Clark County", "Washoe County")) |>
  group_by(county, subgroup) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop")

stopifnot(nrow(special_pops) > 0)
special_pops
#> # A tibble: 6 x 3
#>   county        subgroup n_students
#>   <chr>         <chr>         <dbl>
#> 1 Clark County  el            45993
#> 2 Clark County  frl          282969
#> 3 Clark County  iep           44484
#> 4 Washoe County el             9229
#> 5 Washoe County frl           39010
#> 6 Washoe County iep           10537
```

![Special populations by district](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/special-pops-chart-1.png)

---

### 11. Kindergarten enrollment as a leading indicator

Kindergarten enrollment often predicts future trends. School-level data shows K enrollment dipped to 28,931 in 2024 before recovering to 30,490 in 2026.

```r
k_data <- enr |>
  filter(is_school, subgroup == "total_enrollment", grade_level == "K") |>
  group_by(end_year) |>
  summarize(k_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(change = k_students - lag(k_students),
         pct_change = round(change / lag(k_students) * 100, 2))

stopifnot(nrow(k_data) > 0)
k_data
#> # A tibble: 5 x 4
#>   end_year k_students change pct_change
#>      <dbl>      <dbl>  <dbl>      <dbl>
#> 1     2021      31995     NA      NA
#> 2     2022      34641   2646       8.27
#> 3     2023      31951  -2690      -7.77
#> 4     2024      28931  -3020      -9.45
#> 5     2026      30490   1559       5.39
```

![Kindergarten enrollment trends](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/kindergarten-chart-1.png)

---

### 12. English Learners across Nevada

English Learner (EL) populations vary dramatically across Nevada's districts and charter schools. Clark County has the largest absolute number, but charter schools like Mater Academy have higher EL percentages.

```r
el_data <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL", subgroup == "el") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students, pct)

stopifnot(nrow(el_data) > 0)
el_data
#> # A tibble: 10 x 3
#>    district_name                  n_students     pct
#>    <chr>                               <dbl>   <dbl>
#>  1 Clark County School District        45993 0.15773
#>  2 Washoe County School District        9229 0.14498
#>  3 Mater Academy of Nevada              1816 0.34284
#>  4 Carson City School District           916 0.12581
#>  5 Elko County School District           774 0.08329
#>  6 Lyon County School District           637 0.07031
#>  7 Nye County School District            449 0.07749
#>  8 Somerset Academy of Las Vegas         405 0.04248
#>  9 CIVICA Academy                        403 0.28045
#> 10 Equipo Academy                        365 0.38462
```

![English Learner enrollment by district](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/el-chart-1.png)

---

### 13. Free/Reduced Lunch eligibility reveals economic disparities

FRL eligibility is often used as a proxy for economic disadvantage. Many charter schools report 100% FRL eligibility, while traditional county districts vary widely.

```r
frl_data <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL", subgroup == "frl") |>
  mutate(pct_display = pct * 100) |>
  arrange(desc(pct)) |>
  head(15) |>
  select(district_name, n_students, pct_display)

stopifnot(nrow(frl_data) > 0)
frl_data
#> # A tibble: 15 x 3
#>    district_name                              n_students pct_display
#>    <chr>                                           <dbl>       <dbl>
#>  1 Esmeralda County School District                   69         100
#>  2 Pershing County School District                   647         100
#>  3 Futuro Academy                                    484         100
#>  4 Mater Academy of Northern Nevada                  514         100
#>  5 Democracy Prep                                    927         100
#>  6 Sports Leadership and Management Academy         1988         100
#>  7 Equipo Academy                                    949         100
#>  8 Mater Academy of Nevada                          5297         100
#>  9 Rainbow Dreams Early Learning Academy             229         100
#> 10 The Delta Academy                                1315         100
#> 11 Innovations International Charter School          619         100
#> 12 Quest Academy                                     415         100
#> 13 FuturEdge Academy                                 318         100
#> 14 Southern Nevada Trades High School                250         100
#> 15 Vegas Vista Academy                               270         100
```

![FRL eligibility by district](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/frl-chart-1.png)

---

### 14. Nevada's smallest districts

While Clark County dominates headlines, Nevada has many tiny entities. Charter schools and rural districts alike can have fewer than 200 students.

```r
smallest <- enr_2026 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(n_students) |>
  head(10) |>
  select(district_name, n_students)

stopifnot(nrow(smallest) > 0)
smallest
#> # A tibble: 10 x 2
#>    district_name                                  n_students
#>    <chr>                                               <dbl>
#>  1 Nevada State High School II                            18
#>  2 Independence High School                               37
#>  3 Esmeralda County School District                       69
#>  4 Nevada Classical Academy Elko                          83
#>  5 Young Women's Leadership Academy of Las Vegas         108
#>  6 Learning Bridge                                       171
#>  7 Davidson Academy                                      171
#>  8 Do & Be Arts Academy of Excellence                    175
#>  9 Silver Sands Montessori                               188
#> 10 Honors Academy of Literature                          202
```

![Nevada's smallest districts](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/smallest-chart-1.png)

---

### 15. IEP students: Special education across Nevada

Students with Individualized Education Programs (IEPs) require specialized services. Lyon County leads in IEP rate at 17.0%, while some charter networks have lower rates.

```r
iep_data <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL", subgroup == "iep") |>
  mutate(pct_display = pct * 100) |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students, pct_display)

stopifnot(nrow(iep_data) > 0)
iep_data
#> # A tibble: 10 x 3
#>    district_name                  n_students pct_display
#>    <chr>                               <dbl>       <dbl>
#>  1 Clark County School District        44484       15.26
#>  2 Washoe County School District       10537       16.55
#>  3 Lyon County School District          1541       17.01
#>  4 Somerset Academy of Las Vegas        1258       13.19
#>  5 Elko County School District          1238       13.32
#>  6 Carson City School District           943       12.95
#>  7 Nye County School District            882       15.22
#>  8 Pinecrest Academy of Nevada           840        9.91
#>  9 Douglas County School District        692       14.58
#> 10 Doral Academy                         674       10.46
```

![IEP students by district](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/iep-chart-1.png)

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("almartin82/nvschooldata")
```

## Quick start

### R

```r
library(nvschooldata)
library(dplyr)

# Fetch one year
enr_2026 <- fetch_enr(2026)

# Fetch multiple years
enr_multi <- fetch_enr_multi(c(2021:2024, 2026))

# State totals
get_state_enrollment(c(2021:2024, 2026))

# Filter to a specific district
enr_2026 |>
  filter_district("02")  # Clark County

# Filter to Clark County schools
enr_2026 |>
  filter_county("Clark")

# District breakdown
enr_2026 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students))
```

### Python

```python
import pynvschooldata as nv

# Check available years
years = nv.get_available_years()
print(f"Data available from {years['min_year']} to {years['max_year']}")

# Fetch one year
df = nv.fetch_enr(2026)

# Fetch multiple years
df_multi = nv.fetch_enr_multi([2024, 2026])

# Filter to district totals
district_totals = df[
    (df['is_district'] == True) &
    (df['grade_level'] == 'TOTAL') &
    (df['subgroup'] == 'total_enrollment')
].sort_values('n_students', ascending=False)

print(district_totals[['district_name', 'n_students']])
```

## Data Notes

### Data Source

Nevada Department of Education: [Enrollment Data](https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/accountability/data-requests/enrollment-for-nevada-public-schools)

### Available Years

| Years | Format | Aggregation Levels | Demographics | Notes |
|-------|--------|-------------------|--------------|-------|
| **2021-2024, 2026** | Modern | District, School | Race, Gender, Special Populations | Full LEA codes, complete data |
| **2018-2020** | Intermediate | District, School | Race, Gender | Consolidated format |
| **2016-2017** | Legacy | Limited | Limited | Partial data extraction |

**Note:** 2025 data is currently unavailable from NDE. Use `list_enr_years()` to check current availability.

### Census Day

Data is collected on **October 1 "validation day"** each year. This is Nevada's official enrollment count date.

### Suppression Rules

Nevada does not suppress small counts in the publicly available enrollment files. All counts are reported as provided by districts.

### Nevada District Codes

| Code | District |
|------|----------|
| 02 | Clark County School District (Las Vegas) |
| 16 | Washoe County School District (Reno) |
| 13 | Carson City School District |
| 18 | State Public Charter School Authority (SPCSA) |

### Subgroup Availability by Year

- **Race/ethnicity and gender**: Available for 2018+
- **Special populations** (EL, FRL, IEP, Foster, Military, Homeless, Migrant): Available 2021+
- **Grade levels**: K-12 plus ungraded (UG) available for modern format years

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data in Python and R.

**Mothership package:** [njschooldata](https://github.com/almartin82/njschooldata) (the original)

**All 50 state packages:** [github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

[Andy Martin](https://github.com/almartin82) (almartin@gmail.com)

## License

MIT
