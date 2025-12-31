# Claude Code Instructions for nvschooldata

## Project Context

This is an R package for fetching and processing Nevada school enrollment data from the Nevada Department of Education (NDE).

### Key Files

- `R/fetch_enrollment.R` - Main `fetch_enr()` function
- `R/get_raw_enrollment.R` - Downloads raw data from NDE
- `R/process_enrollment.R` - Transforms raw data to standard schema
- `R/tidy_enrollment.R` - Converts to long/tidy format
- `R/cache.R` - Local caching layer

### Data Sources

Data comes from NDE website:
https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/accountability/data-requests/enrollment-for-nevada-public-schools

Storage URL: https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/

Available year range: 2016-2026 (2015-16 through 2025-26 school years)
- 2016-2017: Legacy format (separate sheets per district, limited extraction)
- 2018-2020: Intermediate format (consolidated sheets, simpler column names)
- 2021-2026: Modern format (full LEA codes, school codes, demographics, special populations)

## Package Conventions
- Follow tidyverse style guide
- Use roxygen2 for documentation
- All exported functions should have examples
- Cache downloaded data to avoid repeated API calls

## Nevada-Specific Notes
- LEA Code: Local Education Agency identifier (district-level)
- Master District Code: Alternative district identifier
- School Code: School identifier within a district
- SPCSA: State Public Charter School Authority (charter schools)
- Data is collected on October 1 "validation day" each year
- Major districts: Clark County (02), Washoe County (16), Carson City (13)
