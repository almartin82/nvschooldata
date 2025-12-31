# nvschooldata

Fetch and analyze Nevada public school enrollment data from the Nevada Department of Education.

## What can you find with nvschooldata?

**10 years of enrollment data (2016-2026).** Over 500,000 students across 17 school districts and the State Public Charter School Authority. Here are ten stories hiding in the numbers:

---

### 1. Clark County: A district the size of a state

Clark County School District (CCSD) enrolls more students than many entire states. It's the 5th largest school district in America.

```r
library(nvschooldata)
library(dplyr)

enr <- fetch_enr_range(2021, 2026)

enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  group_by(end_year) %>%
  mutate(pct_of_state = n_students / sum(n_students) * 100) %>%
  filter(grepl("Clark", district_name)) %>%
  select(end_year, district_name, n_students, pct_of_state)
```

Clark County consistently enrolls 70%+ of all Nevada students.

---

### 2. The Las Vegas metro explosion (and slowdown)

For decades, Las Vegas was one of America's fastest-growing metro areas. Has COVID changed that trajectory?

```r
enr <- fetch_enr_range(2021, 2026)

enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Clark", district_name)) %>%
  select(end_year, n_students) %>%
  mutate(change = n_students - lag(n_students),
         pct_change = change / lag(n_students) * 100)
```

---

### 3. Nevada's demographic transformation

Nevada has experienced rapid demographic change. Hispanic students now represent the largest group in Clark County schools.

```r
enr <- fetch_enr(2026)

enr %>%
  filter(is_district, grade_level == "TOTAL",
         subgroup %in% c("white", "hispanic", "black", "asian")) %>%
  filter(grepl("Clark", district_name)) %>%
  group_by(district_name) %>%
  mutate(total = sum(n_students),
         pct = n_students / total * 100) %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(pct))
```

---

### 4. COVID's enrollment shock

The pandemic hit Nevada schools hard. How did enrollment change across different grade levels?

```r
enr <- fetch_enr_range(2021, 2023)

enr %>%
  filter(is_district, subgroup == "total_enrollment",
         grade_level %in% c("K", "06", "09")) %>%
  group_by(end_year, grade_level) %>%
  summarize(total = sum(n_students, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = end_year, values_from = total)
```

Kindergarten typically shows the sharpest COVID-era declines.

---

### 5. Rural Nevada vs. urban Nevada

Beyond Las Vegas and Reno, Nevada has 15 rural counties with their own school districts. How do they compare?

```r
enr <- fetch_enr(2026)

enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  mutate(region = case_when(
    grepl("Clark", district_name) ~ "Las Vegas Metro",
    grepl("Washoe", district_name) ~ "Reno Metro",
    TRUE ~ "Rural Nevada"
  )) %>%
  group_by(region) %>%
  summarize(
    n_districts = n_distinct(district_name),
    total_enrollment = sum(n_students, na.rm = TRUE)
  )
```

---

### 6. Charter school growth: The rise of SPCSA

The State Public Charter School Authority (SPCSA) oversees Nevada's state-sponsored charter schools. How fast is this sector growing?

```r
enr <- fetch_enr_range(2021, 2026)

enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  mutate(is_spcsa = grepl("SPCSA|Charter", district_name, ignore.case = TRUE)) %>%
  group_by(end_year, is_spcsa) %>%
  summarize(total = sum(n_students, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = is_spcsa, values_from = total,
                     names_prefix = "spcsa_")
```

---

### 7. Washoe County: Nevada's second city

Washoe County (Reno-Sparks) is Nevada's second-largest district. How does it compare to Clark County trends?

```r
enr <- fetch_enr_range(2021, 2026)

enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Clark|Washoe", district_name)) %>%
  group_by(district_name) %>%
  mutate(index = n_students / first(n_students) * 100) %>%
  select(end_year, district_name, n_students, index)
```

---

### 8. Grade-level enrollment patterns

Tracking enrollment by grade reveals where schools are growing or shrinking. Kindergarten is often the canary in the coal mine.

```r
enr <- fetch_enr(2026)

grade_aggs <- enr_grade_aggs(enr)

grade_aggs %>%
  filter(is_district, grepl("Clark", district_name)) %>%
  select(grade_level, n_students)
```

Compare K-8 vs high school (9-12) enrollment to see where growth is happening.

---

### 9. Gender enrollment balance

How does the male/female ratio vary across Nevada's schools?

```r
enr <- fetch_enr(2026)

enr %>%
  filter(is_district, grade_level == "TOTAL",
         subgroup %in% c("male", "female")) %>%
  group_by(district_name) %>%
  mutate(total = sum(n_students),
         pct = n_students / total * 100) %>%
  filter(subgroup == "female") %>%
  select(district_name, n_students, pct) %>%
  arrange(desc(pct))
```

---

### 10. Special populations across districts

English Learners, students with IEPs, Free/Reduced Lunch eligible students: how do these populations vary by district?

```r
enr <- fetch_enr(2026)

enr %>%
  filter(is_district, grade_level == "TOTAL",
         subgroup %in% c("frl", "iep", "el")) %>%
  filter(grepl("Clark|Washoe", district_name)) %>%
  select(district_name, subgroup, n_students, pct) %>%
  tidyr::pivot_wider(names_from = subgroup, values_from = c(n_students, pct))
```

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("almartin82/nvschooldata")
```

## Quick start

```r
library(nvschooldata)
library(dplyr)

# Fetch one year
enr_2026 <- fetch_enr(2026)

# Fetch multiple years (2021-2026)
enr_recent <- fetch_enr_range(2021, 2026)

# State totals
get_state_enrollment(2021:2026)

# Filter to a specific district
enr_2026 %>%
  filter_district("02")  # Clark County

# Filter to Clark County schools
enr_2026 %>%
  filter_county("Clark")

# District breakdown
enr_2026 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students))

# Grade-level aggregates (K-8, 9-12)
enr_grade_aggs(enr_2026)
```

## Data availability

| Years | Format | Aggregation Levels | Demographics | Notes |
|-------|--------|-------------------|--------------|-------|
| **2021-2026** | Modern | District, School | Race, Gender, Special Populations | Full LEA codes, complete data |
| **2018-2020** | Intermediate | District, School | Race, Gender | Consolidated format |
| **2016-2017** | Legacy | Limited | Limited | Partial data extraction |

### Nevada district codes

| Code | District |
|------|----------|
| 02 | Clark County School District (Las Vegas) |
| 16 | Washoe County School District (Reno) |
| 13 | Carson City School District |
| 18 | State Public Charter School Authority (SPCSA) |

### What's available by year range

- **Subgroups**: Race/ethnicity and gender available for 2018+. Special populations (EL, FRL, IEP, Foster, Military, Homeless, Migrant) available 2021+.
- **Grade levels**: K-12 plus ungraded (UG) available for modern format years.
- **Aggregation**: District and school-level data available. State totals computed by package.

## Data source

Nevada Department of Education: [Enrollment Data](https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/accountability/data-requests/enrollment-for-nevada-public-schools)

Data is collected on October 1 "validation day" each year.

## Part of the 50 State Schooldata Family

This package is part of a family of R packages providing school enrollment data for all 50 US states. Each package fetches data directly from the state's Department of Education.

**See also:** [njschooldata](https://github.com/almartin82/njschooldata) - The original state schooldata package for New Jersey.

**All packages:** [github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

[Andy Martin](https://github.com/almartin82) (almartin@gmail.com)

## License

MIT
