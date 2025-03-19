library(here)
library(tidyverse)
library(data.table)
library(countrycode)

# Function to load and clean WPP data
load_wpp_data <- function(file_path, sheet_names, skip_rows, id_cols, age_groups) {
  map_dfr(sheet_names, ~ {
    read_excel(file_path, sheet = .x, skip = skip_rows) %>%
      rename(
        country = `Region, subregion, country or area *`,
        country_code = `Location code`,
        variant = Variant,
        year = Year
      ) %>%
      select(-c("Index", "Notes", "ISO3 Alpha-code", "ISO2 Alpha-code", 
                "SDMX code**", "Type", "Parent code")) %>%
      select(all_of(id_cols), all_of(age_groups)) %>%
      filter(country_code < 900) %>%  # Exclude regions
      mutate(across(all_of(age_groups), as.numeric)) # Ensure numeric values
  })
}

# Define common parameters
id_cols <- c("variant", "country", "country_code", "year")
age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")

# Load fertility data
fertility_data <- load_wpp_data(
  file_path = here("TBburden/indata/WPP2024_FERT_F04_BIRTHS_BY_5-YEAR_AGE_GROUPS_OF_MOTHER.xlsx"),
  sheet_names = c("Medium variant", "Low variant", "High variant"),
  skip_rows = 16,
  id_cols = id_cols,
  age_groups = age_groups
)

# Load female population data
population_data <- load_wpp_data(
  file_path = here("TBburden/indata/WPP2024_POP_F02_3_POPULATION_5-YEAR_AGE_GROUPS_FEMALE.xlsx"),
  sheet_names = c("Medium variant", "Low variant", "High variant"),
  skip_rows = 16,
  id_cols = id_cols,
  age_groups = age_groups
)

# Function to recategorize age groups
recode_age_groups <- function(data) {
  data %>%
    # pivot_longer(cols = -all_of(id_cols), names_to = "age_group", values_to = "value") %>%
    # mutate(value = as.numeric(value)) %>%
    # pivot_wider(names_from = age_group, values_from = value) %>%
    mutate(
      `15-24` = `15-19` + `20-24`,
      `25-34` = `25-29` + `30-34`,
      `35-44` = `35-39` + `40-44`,
      `45-54` = `45-49` + `50-54`
    ) %>%
    select(all_of(id_cols), `15-24`, `25-34`, `35-44`, `45-54`) %>%
    pivot_longer(cols = -all_of(id_cols), names_to = "age_group", values_to = "value") %>%
    pivot_wider(names_from = variant, values_from = value)
}

# Apply age group recoding
fertility_data <- recode_age_groups(fertility_data)
population_data <- recode_age_groups(population_data)

# Merge fertility and population data
num_births <- fertility_data %>%
  left_join(population_data, by = c("country", "country_code", "year", "age_group"), 
            suffix = c("_births", "_pop"))

num_births <- num_births %>% 
  rename(
   births_best = Medium_births,
   births_lo = Low_births,
   births_hi = High_births,
   pop_best = Medium_pop,
   pop_lo = Low_pop,
   pop_hi = High_pop
  )

length(unique(num_births$country))

# Clean country names using countrycode
num_births <- num_births %>%
  mutate(country = countrycode(country, origin = "country.name", destination = "country.name"))

# Convert estimates to thousands
num_births <- num_births %>%
  mutate(across(contains("births"), ~ .x * 1000),
         across(contains("pop"), ~ .x * 1000))

# check
num_births |> 
  summarise(population = sum(pop_best, na.rm = TRUE),
            births = sum(births_best, na.rm = TRUE)) # OK

save(num_births, file = here("TBburden/outdata/UNPOP.RData"))
