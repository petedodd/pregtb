# Load required packages
library(here)
library(data.table)
library(tidyverse)
library(countrycode)
library(readxl)  # Read Excel files
library(readr)   # Read CSV files

# Load TB burden data
data_path <- here::here("TBburden/indata/df_burden.Rdata")

if (!file.exists(data_path)) {
  df_burden <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=estimates", header = TRUE)
  df_age_sex <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=estimates_age_sex", header = TRUE)
  df_dic <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=dictionary")
  save(df_burden, df_age_sex, df_dic, file = data_path)
} else {
  load(data_path)
}

analysis_year <- max(df_age_sex$year)

# Extract TB_HIV-related column names
tbhiv <- names(df_burden)[grepl('tbhiv_prct', names(df_burden))]

# Clean country names & ISO3 codes
df_burden <- df_burden %>%
  mutate(
    country = countrycode(country, origin = "country.name", destination = "country.name"),
    iso3 = countrycode(iso3, origin = "iso3c", destination = "iso3c")
  )

# Keep only relevant columns for the latest year
tb_hiv <- df_burden %>%
  filter(year == analysis_year) %>%
  select(country, iso3, g_whoregion, all_of(tbhiv))

# Handle missing values (fill with group mean)
tb_hiv <- tb_hiv %>%
  group_by(g_whoregion) %>%
  mutate(across(all_of(tbhiv), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()

# Rename columns for easier use
names(tb_hiv) <- gsub('e_|_prct', '', names(tb_hiv))

# Calculate TB_HIV width, remove individual bounds
if (all(c("tbhiv_lo", "tbhiv_hi") %in% names(tb_hiv))) {
  tb_hiv <- tb_hiv %>%
    mutate(tbhivWidth = tbhiv_hi - tbhiv_lo) %>%
    select(-tbhiv_lo, -tbhiv_hi)
}

# Load UNAIDS HIV data
popn_hiv <- read_excel(here::here("TBburden/indata/UNAIDS_HIV2024Estimates.xlsx"), skip = 3)
unaids_dic <- read_csv(here::here("TBburden/indata/unaids_dic.csv"))

# Standardize column names
names(popn_hiv)[1] <- 'year'
names(popn_hiv)[3] <- 'country'
unaids_dic$descriptions <- names(popn_hiv)
names(popn_hiv) <- unaids_dic$varnames

# Filter relevant UNAIDS variables
keep <- unaids_dic %>%
  filter(grepl('15f', varnames) & !grepl('deaths|New', descriptions)) %>%
  pull(varnames)

# Keep only latest year & remove global
popn_hiv <- popn_hiv %>%
  filter(country != 'Global' & year == analysis_year) %>%
  select(country, iso3, all_of(keep))

# Clean country names & ISO3 codes
popn_hiv <- popn_hiv %>%
  mutate(
    country = countrycode(country, origin = "country.name", destination = "country.name"),
    iso3 = countrycode(iso3, origin = "iso3c", destination = "iso3c")
  )

# Compute HIV prevalence
popn_hiv <- popn_hiv %>%
  mutate(
    h15f_best = (h15f / pop15f) * 100, # x 100 to match TB_HIV scale
    h15fWidth = ifelse(pop15f == 0, 0, (h15f.hi - h15f.lo) / pop15f * 100),
    h15fWidth = ifelse(h15fWidth == 0, 0, h15f_best * sqrt((h15fWidth / h15f_best)^2))
  ) %>%
  select(-h15f, -h15f.hi, -h15f.lo, -pop15f)

# Merge TB & HIV data
tb_hiv_risk <- tb_hiv %>%
  left_join(popn_hiv, by = c('country', 'iso3'))

# Handle missing values in merged dataset
h15f <- names(tb_hiv_risk)[grepl('h15f', names(tb_hiv_risk))]

tb_hiv_risk <- tb_hiv_risk %>%
  group_by(g_whoregion) %>%
  mutate(across(all_of(h15f), ~ replace(., is.na(.), mean(., na.rm = TRUE)))) %>%
  ungroup()

# Compute IRR (Incidence Rate Ratio)
tb_hiv_risk |> 
  select(tbhiv, tbhivWidth, h15f_best, h15fWidth) |> 
  summary()

tb_hiv_risk <- tb_hiv_risk %>%
  mutate(
    IRR = ifelse(tbhiv == 100 | h15f_best == 100, NA, 
                 (tbhiv / (100 - tbhiv)) / (h15f_best / (100 - h15f_best))),
    IRRWidth = ifelse(h15fWidth == 0, 0, 
                      IRR * sqrt((tbhivWidth / tbhiv)^2 + (h15fWidth / h15f_best)^2))
  )

# Replace Inf or NaN IRR values
tb_hiv_risk <- tb_hiv_risk %>%
  mutate(
    IRR = ifelse(is.infinite(IRR) | IRR == 0, 1, IRR),
    IRRWidth = ifelse(is.infinite(IRRWidth) | is.na(IRRWidth), 0, IRRWidth)
  )

# Summarize IRR by WHO region
tb_hiv_risk %>%
  group_by(g_whoregion) %>%
  summarise(IRR = mean(IRR, na.rm = TRUE),
            IRRWidth = sqrt(sum(IRRWidth^2, na.rm = TRUE) / n())/2) %>%
  ggplot(aes(y = IRR, x = g_whoregion)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(y = "Incidence Rate Ratio (IRR)", x = "WHO Region")

tb_hiv_risk %>%
  group_by(g_whoregion) %>%
  summarise(
    IRR = mean(IRR, na.rm = TRUE),
    IRRWidth = sqrt(sum(IRRWidth^2, na.rm = TRUE) / n()) / sqrt(n())  # Standard Error (SE)
  ) %>%
  ggplot(aes(y = IRR, x = g_whoregion)) +
  geom_pointrange(aes(ymin = IRR - 1.96 * IRRWidth, ymax = IRR + 1.96 * IRRWidth), width = 0.2) + # 95% CI
  theme_minimal() +
  labs(y = "Incidence Rate Ratio (IRR)", x = "WHO Region") +
  coord_flip()

ggplot() +
  geom_jitter(data = tb_hiv_risk, aes(x = g_whoregion, y = IRR), width = 0.2, alpha = 0.5) +
  geom_pointrange(
    data = tb_hiv_risk %>%
      group_by(g_whoregion) %>%
      summarise(
        IRR = mean(IRR, na.rm = TRUE),
        IRRWidth = sqrt(sum(IRRWidth^2, na.rm = TRUE) / n()) / sqrt(n())  # SE
      ),
    aes(y = IRR, x = g_whoregion, ymin = IRR - 1.96 * IRRWidth, ymax = IRR + 1.96 * IRRWidth),
    color = "red", linewidth = 0.8, shape = 13, size = 1.2, fatten = 5
  ) +
  theme_minimal() +
  labs(y = "Incidence Rate Ratio (IRR)", x = "WHO Region") +
  coord_flip()
ggsave(filename=here("TBburden/plots/TBRiskHIV.png"),
       width=10, height=8, dpi=600)

# Pregnant women living with HIV data
hiv_data <- fread(here("TBburden/indata/hiv_data.csv"))

# Inspect indicators
hiv_data %>%
  distinct(Indicator, IndicatorCode)

# Clean and transform HIV data
hiv_data <- hiv_data %>%
  mutate(
    IndicatorCode = gsub("00000000", "", IndicatorCode),  # Simplify IndicatorCode
    Indicator = case_when(
      IndicatorCode == "HIV_21" ~ "hiv",       # Estimated number of pregnant women needing ART
      IndicatorCode == "HIV_20" ~ "hiv_prop",  # Estimated number of pregnant women living with HIV
      IndicatorCode == "HIV_15" ~ "art",       # Number of pregnant women receiving ART
      TRUE ~ NA_character_                     # Default for unmatched cases
    )
  ) %>%
  select(
    country = Location, iso3 = SpatialDimValueCode,
    g_whoregion = ParentLocationCode, year = Period, IndicatorCode, Indicator,
    latest = IsLatestYear, value = FactValueNumeric,
    value_lo = FactValueNumericLow, value_hi = FactValueNumericHigh
  )

# Keep data for the latest year
hiv_data <- hiv_data %>%
  filter(year == year[which.min(abs(analysis_year - year))]) %>%
  arrange(country, Indicator) %>%
  select(-latest, -IndicatorCode, -year)

# Reshape data to wide format
hiv_data <- hiv_data %>%
  pivot_wider(
    names_from = Indicator,
    names_glue = "{Indicator}_{.value}",
    values_from = c(value, value_lo, value_hi)
  ) %>%
  pivot_longer(
    cols = -c(country, iso3, g_whoregion),
    names_to = "Indicator",
    values_to = "value"
  ) %>%
  mutate(Indicator = gsub("_value", "", Indicator)) %>%
  pivot_wider(names_from = Indicator, values_from = value)

# Check number of unique countries
num_countries <- hiv_data %>%
  distinct(country) %>%
  nrow()
print(paste("Number of unique countries:", num_countries))  # Should be 194

# Clean up country names and ISO3 codes
hiv_data <- hiv_data %>%
  mutate(
    country = countrycode(country, origin = "country.name", destination = "country.name"),
    iso3 = countrycode(iso3, origin = "iso3c", destination = "iso3c")
  ) %>%
  select(country, iso3, hiv, hiv_lo, hiv_hi)  # Keep only relevant columns

# Inspect data for a specific country
hiv_data %>%
  filter(country == "Afghanistan")

save(df_burden, df_age_sex, df_dic, tb_hiv_risk, hiv_data, file = here("TBburden/outdata/TBHIV.Rdata"))