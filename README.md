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

**What's available:** Enrollment data (2021-2026) covering 470,000+ students across 17 county school districts, Carson City, and 51 charter school operators under the State Public Charter School Authority (SPCSA). Demographic breakdowns include race/ethnicity, gender, and special populations (EL, FRL, IEP, foster, military, homeless, migrant).

---

## 15 Insights from Nevada Enrollment Data

### 1. Nevada lost nearly 13,000 students between 2021 and 2026

Nevada was one of America's fastest-growing states for decades, but school enrollment has declined since 2022. The state shed 18,681 students from its 2022 peak to 2026.

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

### 2. Charter schools crack the top 10 in Nevada

Clark County dominates, but three charter networks (Somerset, Pinecrest, Doral) now enroll more students than most county school districts. Somerset Academy alone enrolls more than 14 of the 17 county districts.

```r
enr_2026 <- fetch_enr(2026, use_cache = TRUE)

top_districts <- enr_2026 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students, is_charter)

stopifnot(nrow(top_districts) == 10)
top_districts
#> # A tibble: 10 x 3
#>    district_name                      n_students is_charter
#>    <chr>                                   <dbl> <lgl>
#>  1 Clark County School District           291587 FALSE
#>  2 Washoe County School District           63655 FALSE
#>  3 Somerset Academy of Las Vegas            9534 TRUE
#>  4 Elko County School District              9293 FALSE
#>  5 Lyon County School District              9060 FALSE
#>  6 Pinecrest Academy of Nevada              8474 TRUE
#>  7 Carson City School District              7281 FALSE
#>  8 Doral Academy                            6442 TRUE
#>  9 Nye County School District               5794 FALSE
#> 10 Coral Academy of Science Las Vegas       5552 TRUE
```

![Top Nevada districts](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

---

### 3. Hispanic students are 46% of enrollment, up from 43% in 2021

Hispanic students are the largest demographic group in Nevada public schools at 45.9%, followed by white students at 25.9%. This reflects a continuing demographic shift -- Hispanic share has grown each year.

```r
demographics <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian",
                         "native_american", "multiracial", "pacific_islander")) |>
  group_by(subgroup) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(pct = round(n_students / sum(n_students) * 100, 1)) |>
  arrange(desc(n_students))

stopifnot(nrow(demographics) == 7)
demographics
#> # A tibble: 7 x 3
#>   subgroup         n_students   pct
#>   <chr>                 <dbl> <dbl>
#> 1 hispanic             217320  45.9
#> 2 white                122852  25.9
#> 3 black                 58830  12.4
#> 4 multiracial           37516   7.9
#> 5 asian                 27170   5.7
#> 6 pacific_islander       6622   1.4
#> 7 native_american        3347   0.7
```

![Nevada student demographics](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/demographics-chart-1.png)

---

### 4. Las Vegas Metro has 61.6% of all Nevada students

Beyond Las Vegas and Reno, the remaining 68 entities (15 rural county districts plus charter schools) account for 25% of enrollment.

```r
regional <- enr_2026 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  mutate(region = case_when(
    grepl("Clark", district_name) ~ "Las Vegas Metro",
    grepl("Washoe", district_name) ~ "Reno Metro",
    TRUE ~ "Rest of Nevada"
  )) |>
  group_by(region) |>
  summarize(
    n_entities = n_distinct(district_name),
    total_enrollment = sum(n_students, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(pct = round(total_enrollment / sum(total_enrollment) * 100, 1))

stopifnot(nrow(regional) == 3)
regional
#> # A tibble: 3 x 4
#>   region          n_entities total_enrollment   pct
#>   <chr>                <int>            <dbl> <dbl>
#> 1 Las Vegas Metro          1           291587  61.6
#> 2 Reno Metro               1            63655  13.4
#> 3 Rest of Nevada          68           118415  25.0
```

![Enrollment by region](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/regional-chart-1.png)

---

### 5. Clark County lost 27,706 students since 2021 while Washoe held steady

Clark County (Las Vegas) and Washoe County (Reno) are Nevada's two urban anchors. Clark dropped 8.7% from 2021 to 2026 while Washoe only dropped 2.1%.

```r
growth_data <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Clark|Washoe", district_name)) |>
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

### 6. Charter enrollment surged 33% while traditional districts lost 30,000 students

The State Public Charter School Authority (SPCSA) oversees Nevada's charter sector. Charters grew from 53,223 students in 2021 to 70,534 in 2026 -- a 33% increase -- while traditional districts lost 30,287 students in the same period.

```r
charter_trend <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  group_by(end_year, is_charter) |>
  summarize(students = sum(n_students, na.rm = TRUE),
            n_districts = n(), .groups = "drop") |>
  mutate(sector = ifelse(is_charter, "Charter", "Traditional"))

stopifnot(nrow(charter_trend) > 0)
charter_trend |> select(end_year, sector, students, n_districts)
#> # A tibble: 10 x 4
#>    end_year sector      students n_districts
#>       <dbl> <chr>          <dbl>       <int>
#>  1     2021 Traditional   433410          19
#>  2     2021 Charter        53223          37
#>  3     2022 Traditional   436923          19
#>  4     2022 Charter        55415          38
#>  5     2023 Traditional   429927          19
#>  6     2023 Charter        59670          43
#>  7     2024 Traditional   423687          19
#>  8     2024 Charter        61883          44
#>  9     2026 Traditional   403123          19
#> 10     2026 Charter        70534          51
```

![Charter vs traditional enrollment trends](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/charter-chart-1.png)

---

### 7. Washoe County lost 1,333 students over 5 years

Washoe County (Reno-Sparks) is Nevada's second-largest district. It peaked at 66,541 in 2022 before declining.

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

### 8. 9th grade has the most students; PK has the fewest

Tracking enrollment by grade reveals a classic pattern: PK is small (13,852), enrollment jumps at K (30,490), and peaks in high school around 9th grade (37,251). Grade-level data is available at the school level.

```r
grade_data <- enr_2026 |>
  filter(is_school, subgroup == "total_enrollment",
         !grade_level %in% c("TOTAL", "UG", "AD")) |>
  group_by(grade_level) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(grade_level = factor(grade_level, levels = c("PK", "K", sprintf("%02d", 1:12)))) |>
  filter(!is.na(grade_level)) |>
  arrange(grade_level)

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

### 9. Boys outnumber girls 51.3% to 48.7% statewide

The gender split across Nevada schools is close to even, with a slight male skew consistent with national patterns.

```r
gender_data <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL",
         subgroup %in% c("male", "female")) |>
  group_by(subgroup) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(pct = round(n_students / sum(n_students) * 100, 1))

stopifnot(nrow(gender_data) == 2)
gender_data
#> # A tibble: 2 x 3
#>   subgroup n_students   pct
#>   <chr>         <dbl> <dbl>
#> 1 female       230511  48.7
#> 2 male         243022  51.3
```

![Enrollment by gender](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/gender-chart-1.png)

---

### 10. Clark County has 5x more EL students than Washoe but similar FRL rates

English Learners, students with IEPs, and Free/Reduced Lunch eligible students represent key populations for educational policy. Clark County has ~46,000 EL students vs Washoe's ~9,200.

```r
special_pops <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL",
         subgroup %in% c("free_reduced_lunch", "special_ed", "lep"),
         grepl("Clark|Washoe", district_name)) |>
  mutate(county = ifelse(grepl("Clark", district_name), "Clark County", "Washoe County")) |>
  group_by(county, subgroup) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop")

stopifnot(nrow(special_pops) == 6)
special_pops
#> # A tibble: 6 x 3
#>   county        subgroup           n_students
#>   <chr>         <chr>                   <dbl>
#> 1 Clark County  free_reduced_lunch     282969
#> 2 Clark County  lep                     45993
#> 3 Clark County  special_ed              44484
#> 4 Washoe County free_reduced_lunch      39010
#> 5 Washoe County lep                      9229
#> 6 Washoe County special_ed              10537
```

![Special populations by district](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/special-pops-chart-1.png)

---

### 11. Kindergarten enrollment swung wildly: up 8% in 2022, down 9% in 2024

Kindergarten enrollment is a leading indicator of future trends. Nevada K enrollment hit 34,641 in 2022 then crashed to 28,931 in 2024 before recovering to 30,490 in 2026. Grade-level data is available at the school level.

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

### 12. Mater Academy has the highest EL concentration at 34%

English Learner populations vary dramatically. Clark County leads in absolute numbers (45,993), but several charter schools have much higher EL concentrations. Mater Academy of Nevada has 34.3% EL students.

```r
el_data <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL", subgroup == "lep") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students, pct)

stopifnot(nrow(el_data) == 10)
el_data
#> # A tibble: 10 x 3
#>    district_name                      n_students    pct
#>    <chr>                                   <dbl>  <dbl>
#>  1 Clark County School District            45993 0.158
#>  2 Washoe County School District            9229 0.145
#>  3 Mater Academy of Nevada                  1816 0.343
#>  4 Carson City School District               916 0.126
#>  5 Elko County School District               774 0.083
#>  6 Lyon County School District               637 0.070
#>  7 Nye County School District                449 0.077
#>  8 Somerset Academy of Las Vegas             405 0.042
#>  9 CIVICA Academy                            403 0.280
#> 10 Equipo Academy                            365 0.385
```

![English Learner enrollment by district](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/el-chart-1.png)

---

### 13. Fifteen districts report 100% FRL eligibility

FRL eligibility is a proxy for economic disadvantage. Many charter schools report 100% FRL, along with two rural counties (Esmeralda and Pershing). The variation across districts is enormous.

```r
frl_data <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL", subgroup == "free_reduced_lunch") |>
  mutate(pct_display = round(pct * 100, 1)) |>
  arrange(desc(pct)) |>
  head(15) |>
  select(district_name, n_students, pct_display)

stopifnot(nrow(frl_data) > 0)
frl_data
#> # A tibble: 15 x 3
#>    district_name                           n_students pct_display
#>    <chr>                                        <dbl>       <dbl>
#>  1 Esmeralda County School District                69       100
#>  2 Pershing County School District                647       100
#>  3 Futuro Academy                                 484       100
#>  4 Mater Academy of Northern Nevada               514       100
#>  5 Democracy Prep                                 927       100
#>  6 Sports Leadership and Management Academy      1988       100
#>  7 Equipo Academy                                 949       100
#>  8 Mater Academy of Nevada                       5297       100
#>  9 Rainbow Dreams Early Learning Academy          229       100
#> 10 The Delta Academy                             1315       100
#> 11 Innovations International Charter School       619       100
#> 12 Quest Academy                                  415       100
#> 13 FuturEdge Academy                              318       100
#> 14 Southern Nevada Trades High School             250       100
#> 15 Vegas Vista Academy                            270       100
```

![FRL eligibility by district](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/frl-chart-1.png)

---

### 14. Nevada's smallest LEAs are charter schools, not counties

While Esmeralda County (69 students) is the smallest county district, Nevada's smallest LEAs are micro-charters. Nevada State High School II has just 18 students.

```r
smallest <- enr_2026 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(n_students) |>
  head(10) |>
  select(district_name, n_students, is_charter)

stopifnot(nrow(smallest) == 10)
smallest
#> # A tibble: 10 x 3
#>    district_name                                  n_students is_charter
#>    <chr>                                               <dbl> <lgl>
#>  1 Nevada State High School II                            18 TRUE
#>  2 Independence High School                               37 FALSE
#>  3 Esmeralda County School District                       69 FALSE
#>  4 Nevada Classical Academy Elko                          83 TRUE
#>  5 Young Women's Leadership Academy of Las Vegas         108 TRUE
#>  6 Learning Bridge                                       171 TRUE
#>  7 Davidson Academy                                      171 FALSE
#>  8 Do & Be Arts Academy of Excellence                    175 TRUE
#>  9 Silver Sands Montessori                               188 TRUE
#> 10 Honors Academy of Literature                          202 TRUE
```

![Nevada's smallest districts](https://almartin82.github.io/nvschooldata/articles/enrollment_hooks_files/figure-html/smallest-chart-1.png)

---

### 15. Lyon County has the highest IEP rate among large districts at 17%

Students with IEPs require specialized services. Clark County has the most IEP students by count (44,484), but Lyon County has the highest rate among sizable districts at 17%.

```r
iep_data <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL", subgroup == "special_ed") |>
  mutate(pct_display = round(pct * 100, 1)) |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students, pct_display)

stopifnot(nrow(iep_data) == 10)
iep_data
#> # A tibble: 10 x 3
#>    district_name                  n_students pct_display
#>    <chr>                               <dbl>       <dbl>
#>  1 Clark County School District        44484        15.3
#>  2 Washoe County School District       10537        16.6
#>  3 Lyon County School District          1541        17.0
#>  4 Somerset Academy of Las Vegas        1258        13.2
#>  5 Elko County School District          1238        13.3
#>  6 Carson City School District           943        13.0
#>  7 Nye County School District            882        15.2
#>  8 Pinecrest Academy of Nevada           840         9.9
#>  9 Douglas County School District        692        14.6
#> 10 Doral Academy                         674        10.5
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

### Grade-Level Data Availability

Grade-level breakdowns (PK, K, 01-12) are available at the **school level** only. District-level records have `grade_level == "TOTAL"` only. To get grade-level aggregates across districts, filter on `is_school` and aggregate.

### Subgroup Availability by Year

- **Race/ethnicity and gender**: Available for 2018+
- **Special populations** (EL, FRL, IEP, Foster, Military, Homeless, Migrant): Available 2021+
- **Grade levels**: K-12 plus ungraded (UG) available at school level for modern format years

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data in Python and R.

**Mothership package:** [njschooldata](https://github.com/almartin82/njschooldata) (the original)

**All 50 state packages:** [github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

[Andy Martin](https://github.com/almartin82) (almartin@gmail.com)

## License

MIT
