## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 5
)

## ----load-packages------------------------------------------------------------
library(nvschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))

## ----statewide-trend----------------------------------------------------------
# Note: 2025 data temporarily unavailable from NDE
enr <- fetch_enr_multi(c(2021:2024, 2026))

state_totals <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  group_by(end_year) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))

state_totals

## ----statewide-chart----------------------------------------------------------
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

## ----top-districts------------------------------------------------------------
enr_2026 <- fetch_enr(2026)

top_districts <- enr_2026 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students)

top_districts

## ----top-districts-chart------------------------------------------------------
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

## ----demographics-------------------------------------------------------------
demographics <- enr_2026 |>
  filter(is_district, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian", "native_american", "multiracial")) |>
  group_by(subgroup) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(pct = round(n_students / sum(n_students) * 100, 1)) |>
  arrange(desc(n_students))

demographics

## ----demographics-chart-------------------------------------------------------
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

## ----regional-----------------------------------------------------------------
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

## ----regional-chart-----------------------------------------------------------
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

## ----growth-chart-------------------------------------------------------------
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

## ----charters-----------------------------------------------------------------
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

