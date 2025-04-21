library(tidyverse)
library(janitor)
library(sf)
library(tmap)
library(geodata)
library(tinytable)
library(tigris)
library(dplyr)
library(stringr)
library(plotrix)

broadband <- read_csv("https://raw.githubusercontent.com/Mustafa-Ali-code/stat100project/refs/heads/main/datasets/broadband_data_2020October.csv")
education <- read_csv("https://raw.githubusercontent.com/Mustafa-Ali-code/stat100project/refs/heads/main/datasets/Education2023.csv")
population <- read_csv("https://raw.githubusercontent.com/Mustafa-Ali-code/stat100project/refs/heads/main/datasets/PopulationEstimates.csv")
poverty <- read_csv("https://raw.githubusercontent.com/Mustafa-Ali-code/stat100project/refs/heads/main/datasets/Poverty2023.csv")
unemployment <- read_csv("https://raw.githubusercontent.com/Mustafa-Ali-code/stat100project/refs/heads/main/datasets/Unemployment2023.csv")

# Clean names
broadband <- clean_names(broadband)
education <- clean_names(education)
population <- clean_names(population)
poverty <- clean_names(poverty)
unemployment <- clean_names(unemployment)


# Step 2: Extract relevant values

# Broadband: availability and usage
broadband_clean <- broadband |>
  select(fips = county_id, state = st, county_name,
         broadband_availability_per_fcc, broadband_usage)

# Education: % with bachelor's degree (2019–23)
education_clean <- education |>
  filter(attribute == "Percent of adults with a bachelor's degree or higher, 2019-23") |>
  select(fips = fips_code, pct_bachelors_2023 = value)

# Population: 2023 estimate
population_clean <- population |>
  filter(attribute == "POP_ESTIMATE_2023") |>
  select(fips = fip_stxt, population_2023 = value)

# Poverty: poverty rate and median income
poverty_clean <- poverty |>
  filter(attribute %in% c("PCTPOVALL_2023", "MEDHHINC_2023")) |>
  pivot_wider(names_from = attribute, values_from = value) |>
  rename(fips = fips_code,
         poverty_rate_2023 = PCTPOVALL_2023,
         median_income_2023 = MEDHHINC_2023)

# Unemployment: 2023 rate
unemployment_clean <- unemployment |>
  filter(attribute == "Unemployment_rate_2023") |>
  select(fips = fips_code, unemployment_rate_2023 = value)

# Merge all cleaned data on fips
final_data <- broadband_clean |>
  left_join(education_clean, by = "fips") |>
  left_join(population_clean, by = "fips") |>
  left_join(poverty_clean, by = "fips") |>
  left_join(unemployment_clean, by = "fips")

final_data <- final_data |>
  mutate(
    broadband_availability_per_fcc = as.numeric(broadband_availability_per_fcc),
    broadband_usage = as.numeric(broadband_usage)
  )


#Graph 1: Broadband Usage vs. Broadband Availability
ggplot(final_data, aes(x = broadband_availability_per_fcc, y = broadband_usage, na.rm = TRUE)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Broadband Usage vs. Availability (per FCC)",
    x = "Broadband Availability (%)",
    y = "Broadband Usage (%)"
  ) +
  theme_minimal()


counties_sf <- st_read("cb_2023_us_county_5m.shp") |>
  clean_names()

counties_sf <- counties_sf |>
  mutate(fips = geoid)

final_data <- final_data |>
  mutate(fips = str_pad(fips, width = 5, pad = "0"))

map_data <- left_join(counties_sf, final_data, by = "fips")

map_data <- map_data |> 
  filter(!str_sub(fips, 1, 2) %in% c("02", "15", "72"))

tmap_mode("view")

tm_shape(map_data) +
  tm_polygons("broadband_usage",
              fill.scale = tm_scale_intervals(style = "jenks"),
              fill.legend = tm_legend(title = "Broadband Usage (%)"),
              col_alpha = 0.1) +
  tm_title("Broadband Usage by County") +
  tm_crs("auto")

# Graph 2: Usage vs. Median Income
income_pie_data <- final_data |>
  mutate(income_group = case_when(
    median_income_2023 < 50000 ~ "<50k",
    median_income_2023 >= 50000 & median_income_2023 < 60000 ~ "50–60k",
    median_income_2023 >= 60000 & median_income_2023 < 70000 ~ "60–70k",
    median_income_2023 >= 70000 & median_income_2023 < 80000 ~ "70–80k",
    median_income_2023 >= 80000 ~ "80k+",
    TRUE ~ NA_character_
  )) |>
  group_by(income_group) |>
  summarize(total_usage = sum(broadband_usage, na.rm = TRUE)) |>
  na.omit() |>
  arrange(income_group)

slices <- income_pie_data$total_usage
percentages <- round(slices / sum(slices) * 100, 1)
labels <- paste0(income_pie_data$income_group, " (", percentages, "%)")

pie3D(
  slices,
  labels = NA,
  explode = 0.1,
  main = "Broadband Usage by Income Group (3D Pie Chart)",
  radius = 1.4,
  col = rainbow(length(slices)),
  labelcex = 1
)

legend("topright", legend = labels, fill = rainbow(length(slices)), cex = 1.1, bty = "n")

## Graph 3: Usage vs. Education (% Bachelor's Degree or Higher)
final_data |>
  filter(!is.na(pct_bachelors_2023)) |>  # First, drop pre-existing NAs
  mutate(edu_group = cut(
    pct_bachelors_2023,
    breaks = c(0, 10, 20, 30, 40, 50, Inf),
    labels = c("<10%", "10–20%", "20–30%", "30–40%", "40–50%", "50%+"),
    right = FALSE  # Optional: adjust if bins should be inclusive on the left instead
  )) |>
  filter(!is.na(edu_group)) |>  # Drop any NAs created by cut()
  group_by(edu_group) |>
  summarize(avg_usage = mean(broadband_usage, na.rm = TRUE)) |>
  ggplot(aes(x = edu_group, y = avg_usage)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Broadband Usage by Education Level",
    x = "% with Bachelor's Degree or Higher (Grouped)",
    y = "Average Broadband Usage (%)"
  ) +
  theme_minimal()

## Table: Top 10 Counties by Usage
top10_table <- final_data |>
  select(county_name, state, broadband_usage, median_income_2023,
         pct_bachelors_2023, poverty_rate_2023) |>
  mutate(broadband_usage = round(broadband_usage * 100, 1)) |>
  arrange(desc(broadband_usage)) |>
  slice_head(n = 10) |>
  rename(
    `County` = county_name,
    `State` = state,
    `Usage (%)` = broadband_usage,
    `Median Income` = median_income_2023,
    `% Bachelor's Degree` = pct_bachelors_2023,
    `Poverty Rate (%)` = poverty_rate_2023
  )

tt(top10_table)
