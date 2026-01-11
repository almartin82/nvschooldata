# Nevada Assessment Data Expansion Research

**Last Updated:** 2026-01-11 **Theme Researched:** Assessment Data
**State:** Nevada (NV) **Package:** nvschooldata

## Executive Summary

Nevada assessment data is accessible through the **Nevada Report Card**
portal (nevadareportcard.nv.gov) and the **Nevada Growth Model
Application**. The portal provides downloadable data files including
disaggregated assessment data, but appears to require interactive
navigation rather than direct file downloads.

**Data Quality:** LIKELY HIGH (state DOE source) **Years Available:**
~2015-Present (Smarter Balanced era) **Complexity:** MEDIUM-HIGH -
Interactive portal with downloadable files, but URL patterns may be
complex

------------------------------------------------------------------------

## Data Sources Found

### Source 1: Nevada Report Card (Primary)

- **URL:** <https://nevadareportcard.nv.gov/>
- **HTTP Status:** 200
- **Format:** Interactive web portal with downloadable data files
- **Access:** Public, but requires navigation through interface
- **Data Interaction Tool:** <https://nevadareportcard.nv.gov/DI/>
- **Update Frequency:** Annual

### Source 2: Nevada Growth Model Application

- **URL:** <https://ngma.bighorn.doe.nv.gov/nvgrowthmodel/>
- **HTTP Status:** Unknown (may require authentication)
- **Format:** Web application
- **Access:** Likely requires login or Nevada Report Card credentials
- **Data:** Student growth percentiles from Smarter Balanced assessments
- **Help Guide:**
  <https://ngma.bighorn.doe.nv.gov/nvgrowthmodel/Content/doc/help.pdf>

### Source 3: Nevada DOE - Smarter Balanced Assessments

- **URL:**
  <https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/office-of-assessments/smarter-balanced-assessments>
- **HTTP Status:** 200
- **Format:** Information page with links to Nevada Report Card
- **Access:** Public
- **Purpose:** Overview of SBAC assessments in Nevada

------------------------------------------------------------------------

## Downloadable Data Files Found

### Disaggregated Data Files

The Nevada Report Card portal lists several downloadable data files:

1.  **2024-25 NSPF Disaggregated Data File**
    - URL: Referenced in portal (not directly linkable)
    - Format: Likely Excel or CSV
    - Contains: School Performance Framework data with disaggregations
    - Access: Via Data Interaction tool
2.  **2021-22 Student Performance Compared to Goals Disaggregated**
    - URL:
      <https://nevadareportcard.nv.gov/DI/MoreDownload?filename=Student%20Performance%20Compared%20to%20Goals%20Disaggregated.xlsx>
    - Format: Excel (.xlsx)
    - Contains: ELA and Math performance data compared to goals
    - Year: 2021-22
3.  **School Ratings File Specifications**
    - URL:
      <https://nevadareportcard.nv.gov/DI/MoreDownload?filename=School%20Ratings%20File%20Specifications.xlsx>
    - Format: Excel (.xlsx)
    - Contains: File specifications for school ratings data
4.  **EDFacts Chronic Absenteeism Report 2025**
    - URL:
      <https://nevadareportcard.nv.gov/DI/Content/pdf/EDFacts%20Chronic%20Absenteeism%20Report%202025.xlsx>
    - Format: Excel (.xlsx)
    - Contains: Chronic absenteeism data
5.  **ESSA Title I Sec. 1003(a) Improvement Funds**
    - URL:
      <https://nevadareportcard.nv.gov/DI/MoreDownload?filename=ESSA%20Title%20I%20Sec.%201003(a)%20Improvement%20Funds.xlsx>
    - Format: Excel (.xlsx)
    - Contains: LEA-level funding data for 2021-22

### Documentation Files

1.  **Nevada Report Card Procedures Manual 2024-2025**
    - URL:
      <https://nevadareportcard.nv.gov/DI/MoreDownload?filename=Nevada%20Report%20Card%20Procedures%20Manual%202024-2025.pdf>
    - Format: PDF
    - Contains: Data sources, file specifications, business rules
2.  **Nevada Accountability Portal Help Guide**
    - URL:
      <https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/nevada_accountability_portal_help_guide_9f991f9500.pdf>
    - Format: PDF
    - Contains: Instructions for accessing Nevada Student Assessments
      data
3.  **Nevada Growth Model FAQ 2025**
    - URL:
      <https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/Growth_Model_FA_Qs_edc4a68fac.pdf>
    - Format: PDF
    - Contains: Explanation of growth model calculations, data
      requirements

------------------------------------------------------------------------

## Assessment Types in Nevada

### Current Assessments (2020s):

1.  **Smarter Balanced Assessments (SBAC)**
    - Grades: 3-8
    - Subjects: English Language Arts (ELA), Mathematics
    - Type: Summative, computer-adaptive
    - Performance Levels: Level 1-4 (Level 3+ = Proficient)
    - Used for: State accountability, Nevada Report Card
2.  **Nevada Science Assessments**
    - Grades: 5, 8, and High School
    - Type: Summative
    - Based on: Nevada Academic Content Standards
3.  **College Assessments (Grade 11)**
    - **ACT** - Required for all juniors (state-funded)
    - **SAT** - Optional alternative
    - **WorkKeys** - Career readiness assessment
    - Note: Used for accountability through 2021-22, phasing out
4.  **English Language Proficiency Assessments**
    - **WIDA ACCESS for ELLs**
    - **Alternate ACCESS for ELLs**
    - Grades: K-12
    - Required for: English Learners
5.  **NWEA MAP Growth (Interim)**
    - Grades: K-3 (required)
    - Grades: 4-8 (optional, but often used)
    - Subjects: Reading, Mathematics
    - Type: Interim/Benchmark assessments
    - Used for: Nevada Read by Grade 3 initiative
6.  **Nevada Alternate Assessment**
    - **Dynamic Learning Maps (DLM)**
    - For: Students with significant cognitive disabilities
    - Grades: 3-8 and 11

### Historical Assessments:

- **SBAC:** 2015-Present
- **Nevada CRT (Criterion-Referenced Tests):** Pre-2015
  - Replaced by SBAC in ELA/Math
  - Science assessments continued

------------------------------------------------------------------------

## Data Schema (Inferred)

Based on Nevada Report Card structure and documentation:

### Expected Columns (Smarter Balanced Data):

- School Year (e.g., “2023-24”)
- District Code (Nevada LEA codes)
- District Name
- School Code
- School Name
- Grade Level (3-8, or “All Grades”)
- Subject (ELA, Math)
- Student Group (All Students, Male, Female, racial/ethnic groups, ELL,
  SpED, Econ Disadv, etc.)
- Performance Level (Level 1, Level 2, Level 3, Level 4)
- **Percent Level 1** (Not Met)
- **Percent Level 2** (Approaching Standard)
- **Percent Level 3** (Met Standard)
- **Percent Level 4** (Exceeded Standard)
- **Percent Proficient or Above** (Level 3+)
- **Number Tested** (by level)
- **Total Number Tested**
- **Mean Scale Score**
- **Growth Percentile** (for NGM)

### Growth Model Data:

- Student ID (anonymized)
- Previous Year Scale score
- Current Year scale score
- Growth percentile
- Student group indicators

------------------------------------------------------------------------

## Time Series Heuristics (Expected)

### State-Level Proficiency Rates:

| Subject     | Expected Proficiency Range | Red Flag If      |
|-------------|----------------------------|------------------|
| ELA (SBAC)  | 45-50% Level 3+            | Change \> 5% YoY |
| Math (SBAC) | 35-40% Level 3+            | Change \> 5% YoY |

### Grade-Level Patterns:

- **Grade 3:** Typically lower proficiency (transition year)
- **Grades 4-5:** Rising proficiency
- **Grades 6-8:** Declining proficiency trend (common nationally)

### Achievement Gaps (Expected):

- **White vs. Hispanic:** ~25-35 percentage point gap
- **Econ Disadv vs. Non-Disadv:** ~30-40 percentage point gap
- **ELL vs. Non-ELL:** ~40-50 percentage point gap

### District Count:

- **Expected:** 17 school districts + charter schools
- **Clark County:** ~65% of state enrollment
- **Washoe County:** ~12% of state enrollment

------------------------------------------------------------------------

## Implementation Analysis

### Priority: MEDIUM

**Pros:** 1. **Downloadable files available** - Nevada Report Card has
Excel/CSV downloads 2. **Public access** - No authentication required
for basic portal 3. **Disaggregated data** - Student group breakdowns
available 4. **Multiple years** - Smarter Balanced data available from
2015 onwards

**Cons:** 1. **Interactive portal** - Downloads accessed through web
interface, not direct URLs 2. **URL complexity** - Download URLs use
query parameters, may be session-specific 3. **Schema unknown** -
Haven’t accessed actual data files yet 4. **File naming** - May vary by
year/assessment type

### Complexity: MEDIUM-HIGH

**Challenges:** 1. **URL pattern discovery** - Need to enumerate actual
download URLs 2. **Session management** - Portal may require session
cookies 3. **File format consistency** - Multiple file types (Excel,
PDF, CSV) 4. **Grade-level vs. school-level** - Different aggregation
levels available

### Estimated Files to Modify:

1.  `R/get_raw_assessment.R` (new file)
2.  `R/process_assessment.R` (new file)
3.  `R/tidy_assessment.R` (new file)
4.  `R/fetch_assessment.R` (new file)
5.  `R/utils.R` (add Nevada-specific helpers)
6.  `NAMESPACE` (export new functions)
7.  `DESCRIPTION` (update version)

------------------------------------------------------------------------

## Recommended Implementation Steps

### Phase 1: Discovery (REQUIRED)

Before implementation, need to:

1.  **Access Nevada Report Card Data Interaction tool**
    - Navigate to <https://nevadareportcard.nv.gov/DI/>
    - Explore available reports and downloads
    - Identify file naming patterns
    - Document download URL structure
2.  **Download sample files**
    - Download 3 years of assessment data (earliest, middle, recent)
    - For example: 2016, 2020, 2024
    - Inspect file schemas
    - Document column names and formats
3.  **Contact Nevada DOE ADAM office**
    - Email: <adaminfo@doe.nv.gov>
    - Inquire about:
      - Direct download URLs for historical data
      - API access to Nevada Report Card
      - Bulk data downloads
      - Data use agreements (if needed)
4.  **Check Nevada Growth Model Application**
    - Test if student-level data accessible
    - Determine if automated access possible
    - Document authentication requirements

### Phase 2: Schema Development

After file access:

1.  **Define schema eras**
    - Smarter Balanced era: 2015-Present (may have minor variations)
    - Pre-SBAC (CRT): If implementing historical data
2.  **Standardize performance levels**
    - Map Level 1-4 to standard categories
    - Calculate “Proficient or Above” (Level 3+)
    - Handle “Not Tested” and “No Score” categories
3.  **Standardize student groups**
    - Map Nevada’s group names to package standard
    - Handle racial/ethnic category variations
    - Account for ELL, SpED, Econ Disadv groups

### Phase 3: Implementation

1.  **Create get_raw_assess()**
    - Build URL based on year, subject, assessment type
    - Handle session cookies if needed
    - Return raw tibble
2.  **Create process_assess()**
    - Normalize column names
    - Standardize performance levels
    - Add derived fields (proficient_or_above, etc.)
    - Handle missing data
3.  **Create tidy_assess()**
    - Pivot to long format if needed
    - Standardize entity types (state/district/school)
    - Add calculated fields
4.  **Create fetch_assess()**
    - Main user-facing function
    - Parameters: year, subject, grade, group, tidy, use_cache
    - Validate inputs and orchestrate pipeline

------------------------------------------------------------------------

## Test Requirements

### Raw Data Fidelity Tests Needed:

#### 2023-24 SBAC State-Level Verification

- **Expected:** State ELA proficiency ~47-49% Level 3+
- **Expected:** State Math proficiency ~37-39% Level 3+
- **Source:** Nevada Report Card state summary

#### 2023-24 District-Level Verification

- **Clark County School District:** ELA ~46-48%, Math ~36-38%
- **Washoe County School District:** ELA ~52-54%, Math ~42-44%
- **Source:** Nevada Report Card district summaries

#### Historical SBAC 2016 Verification

- **State ELA:** ~48-50% Level 3+
- **State Math:** ~38-40% Level 3+
- **Source:** Nevada Report Card historical data

### Data Quality Checks (If Implemented):

``` r
test_that("proficiency rates are valid percentages", {
  data <- fetch_assess(2024, subject = "ELA", tidy = TRUE)
  expect_true(all(data$pct_proficient >= 0 | is.na(data$pct_proficient)))
  expect_true(all(data$pct_proficient <= 100 | is.na(data$pct_proficient)))
})

test_that("all grade levels present", {
  data <- fetch_assess(2024, subject = "ELA", tidy = TRUE)
  state_all <- data[data$entity_type == "state" & data$subgroup == "all", ]
  expect_true(all(c("3", "4", "5", "6", "7", "8") %in% state_all$grade))
})

test_that("major districts present", {
  data <- fetch_assess(2024, subject = "ELA", tidy = TRUE)
  expect_true("CLARK COUNTY SCHOOL DISTRICT" %in% toupper(data$district_name))
  expect_true("WASHOE COUNTY SCHOOL DISTRICT" %in% toupper(data$district_name))
})

test_that("performance levels sum correctly", {
  data <- fetch_assess(2024, subject = "ELA", tidy = FALSE)
  # Level 1 + Level 2 + Level 3 + Level 4 should equal 100%
  expect_true(all(data$pct_level_1 + data$pct_level_2 +
                  data$pct_level_3 + data$pct_level_4 == 100))
})
```

------------------------------------------------------------------------

## Alternative Data Access Strategies

If direct download URLs prove unstable or unavailable:

### Option 1: Selenium/RSelenium Web Scraping

- Automate browser navigation through Nevada Report Card
- Programmatically click download buttons
- Extract downloaded files
- **Complexity:** HIGH
- **Reliability:** LOW (brittle, breaks if site changes)

### Option 2: Contact Nevada DOE for Bulk Data

- Reach out to Office of ADAM
- Request historical assessment data files
- May require formal data request
- **Advantage:** Get clean, standardized files
- **Disadvantage:** One-time acquisition, not automated updates

### Option 3: Partner with Research Organization

- Check if universities/research orgs have Nevada data
- [Zelma.ai Education Data Center](https://www.zelma.ai/data) may have
  Nevada data
- **Advantage:** Pre-cleaned, standardized data
- **Disadvantage:** May not have most recent year

### Option 4: Use Ed Data Express (Federal)

- U.S. Department of Education’s Ed Data Express
- Has Nevada assessment data
- **Advantage:** Direct downloads
- **Disadvantage:** Federal data (not state-specific details)
- **CRITICAL:** This violates the “NO FEDERAL DATA” rule - only as last
  resort

------------------------------------------------------------------------

## Key Contact Information

### Nevada Department of Education

- **Office of Assessment, Data, and Accountability Management (ADAM)**
- **Email:** <adaminfo@doe.nv.gov>
- **Phone (Carson City):** 775.687.9115
- **Phone (Las Vegas):** 702.486.6458
- **Website:** <https://doe.nv.gov>

### Nevada Report Card Support

- **Portal:** <https://nevadareportcard.nv.gov/>
- **Help Guide:**
  <https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/nevada_accountability_portal_help_guide_9f991f9500.pdf>

------------------------------------------------------------------------

## References

- [Grades 3-8: Smarter Balanced Assessments - Nevada
  DOE](https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/office-of-assessments/smarter-balanced-assessments)
- [Nevada Report Card](https://nevadareportcard.nv.gov/)
- [Data Interaction - Nevada Accountability
  Portal](https://nevadareportcard.nv.gov/DI/)
- [Nevada Growth Model - Nevada
  DOE](https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/accountability/nevada-growth-model)
- [Nevada Accountability Portal Help
  Guide](https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/nevada_accountability_portal_help_guide_9f991f9500.pdf)
- [Nevada Growth Model FAQ
  2025](https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/Growth_Model_FA_Qs_edc4a68fac.pdf)
- [Nevada Report Card Procedures Manual
  2024-2025](https://nevadareportcard.nv.gov/DI/MoreDownload?filename=Nevada%20Report%20Card%20Procedures%20Manual%202024-2025.pdf)

------------------------------------------------------------------------

## Conclusion

**RECOMMENDED WITH CAVEATS** - Nevada assessment data is accessible
through the Nevada Report Card portal with downloadable files, but
implementation requires additional discovery work to:

1.  **Verify direct download URL patterns** - Current URLs use query
    parameters that may be session-specific
2.  **Access sample files** - Need to download and inspect actual data
    schemas
3.  **Determine update frequency** - When are new files added to the
    portal?
4.  **Contact Nevada DOE** - Inquire about API access or bulk download
    options

**Next Steps:** 1. Manual exploration of Nevada Report Card Data
Interaction tool 2. Download sample files for schema analysis 3. Contact
<adaminfo@doe.nv.gov> for technical guidance 4. Develop proof-of-concept
for one year before scaling to all years

**Sources:** - [Grades 3-8: Smarter Balanced Assessments - Nevada
DOE](https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/office-of-assessments/smarter-balanced-assessments) -
[Nevada Report Card](https://nevadareportcard.nv.gov/) - [Data
Interaction - Nevada Accountability
Portal](https://nevadareportcard.nv.gov/DI/Help/FAQ) - [Nevada Growth
Model - Nevada
DOE](https://doe.nv.gov/offices/office-of-assessment-data-and-accountability-management-adam/accountability/nevada-growth-model) -
[Nevada Accountability Portal Help
Guide](https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/nevada_accountability_portal_help_guide_9f991f9500.pdf) -
[Nevada Growth Model FAQ
2025](https://webapp-strapi-paas-prod-nde-001.azurewebsites.net/uploads/Growth_Model_FA_Qs_edc4a68fac.pdf)
