# Pregnancy and TB data

# Aim: construct a single merged dataset at country level

# Sources of data
# http://www.who.int/tb/country/data/download/en/
# https://extranet.who.int/tme/generateCSV.asp?ds=dictionary
# https://population.un.org/wpp/Download/Standard/Population/
# https://population.un.org/wpp/Download/Standard/Fertility/

# Clear workspace and console
rm(list = ls())
cat("\014")

# Load required libraries
library(readxl)
library(tidyverse)
library(viridis)
library(here)
library(countrycode)
library(countries)

## also needed installed:
## janitor - install.packages("janitor")
## HEdtree - devtools::install_github("petedodd/HEdtree")

# Load ISO country codes
load(here("TBburden/indata/isodict.Rdata"))
code <- read_csv(here("TBburden/indata", "all.csv")) %>%
  rename(country = name, iso3 = `alpha-3`) %>%
  select(country, `country-code`, iso3, region, `sub-region`)

# Standardize country names in ISO data
ISO <- ISO %>%
  mutate(country = case_when(
    country == "Czech Republic" ~ "Czechia",
    country == "Serbia & Montenegro" ~ "Serbia",
    country == "Swaziland" ~ "Eswatini",
    country == "The Former Yugoslav Republic of Macedonia" ~ "North Macedonia",
    TRUE ~ country
  ))

# Standardize country names using countrycode
ISO <- ISO %>%
  mutate(country = countrycode(country, origin = "country.name", destination = "country.name"))
code <- code %>%
  mutate(country = countrycode(country, origin = "country.name", destination = "country.name"))

# Merge ISO data with additional codes
ISOS <- code %>%
  select(country, iso3, region, sub_region = `sub-region`) %>%
  left_join(ISO, by = c("country", "iso3")) %>%
  select(country, iso2, iso3, region, sub_region, g_whoregion)

# Check for missing WHO regions and assign based on region
ISOS <- ISOS %>%
  mutate(g_whoregion = case_when(
    region == "Africa" ~ "AFR",
    region == "Americas" ~ "AMR",
    region == "Europe" ~ "EUR",
    region == "Asia" ~ "SEA",
    region == "Oceania" ~ "WPR",
    TRUE ~ NA_character_
  ))

# Verify WHO region assignments
table(ISOS$g_whoregion, ISOS$region, useNA = "always")

# Load TB and HIV data
# TB burden, TB risk given HIV, and pregnancy HIV data
if (!file.exists(here("TBburden/outdata/TBHIV.Rdata"))) {
  source(here("TBburden/R/TBrisk_hiv.R"))
} else {
  load(here("TBburden/outdata/TBHIV.Rdata"))
}

# Load UN population & fertility data
if (!file.exists(here("TBburden/outdata/UNPOP.RData"))) {
  source(here("TBburden/R/population_fertility.R"))
} else {
  load(here("TBburden/outdata/UNPOP.RData"))
}

# Define the most recent year for analysis
analysis_year <- max(df_age_sex$year)

# Filter females of reproductive age group
df_ISO <- ISOS %>%
  select(country, iso3, g_whoregion)

# Standardize country names in TB data
df <- df_age_sex %>%
  mutate(country = countrycode(country,
                               origin = "country.name",
                               destination = "country.name"
  ))

# Merge TB data with ISO data
df <- df_ISO %>%
  left_join(df, by = c("country", "iso3"))

df |> 
  select(country, iso3, g_whoregion, age_group, year, best, lo, hi)

unique(df$age_group)
df |> 
  filter(sex == "f" & age_group == "15plus") |>
  summarise(
    sum(best, na.rm = TRUE)
  )

unique(df$age_group)
# Filter for females in reproductive age groups
df <- df %>%
  filter(sex == "f", age_group %in% c("15-24", "25-34", "35-44", "45-54"))

df |> 
  summarise(
    sum(best, na.rm = TRUE)
  )

# Check number of unique countries
num_countries <- length(unique(df$country))
print(paste("Number of unique countries:", num_countries))  # Should be 215


# Merge with ISO3 codes
num_births <- num_births %>%
  left_join(ISOS, by = c("country")) %>%
  select(country, iso3, g_whoregion, age_group, year, starts_with("births"), starts_with("pop"))

table(num_births$g_whoregion, useNA = "always")
length(unique(num_births$country))

num_births |> 
  filter(is.na(g_whoregion)) 

# Filter num_births to match WHO TB data or the most recent year
num_births <- num_births %>%
  filter(year == year[which.min(abs(year - analysis_year))]) %>%
  select(-year)

# Check for missing HIV data
hiv_data |> 
  filter(is.na(hiv))

# Merge with births data
num_births <- num_births %>%
  left_join(hiv_data, by = c("country", "iso3"))

# combining the different datasets -
# births, population of females in the reproductive age and
# TB incident case in women in the reproductive age group
unique(df$age_group)
unique(num_births$age_group)
new_df_births <- df %>%
  select(country, iso3, iso_numeric, year, age_group, best, lo, hi) %>%
  left_join(num_births, by = c("country", "iso3", "age_group")) %>%
  dplyr::rename(TBI_best = best, TBI_lo = lo, TBI_hi = hi) %>%
  filter(!is.na(g_whoregion))

length(unique(new_df_births$country)) # 215
setdiff(unique(new_df_births$country), unique(df$country))
setdiff(unique(df$country), unique(new_df_births$country))


# Add uncertainty widths for births, HIV, and TBI
## use the 'width' -- there will be a common factor of 3.92 for all contributors:
## A = B*C
## log(A) = log(B) + log(C)
## differentiate:  dA / A = dB/B + dC/C
## (A.sd/A)^2 = (B.sd/B)^2 + (C.sd/C)^2
new_df_births <- new_df_births %>%
  mutate(
    birthsWidth = births_hi - births_lo,
    birthsWidthSq = birthsWidth^2,
    HIVwidth = hiv_hi - hiv_lo,
    HIVwidthSq = HIVwidth^2,
    TBIwidth = TBI_hi - TBI_lo,
    TBIwidthSq = TBIwidth^2,
    popWidth = pop_hi - pop_lo,
    popWidthSq = popWidth^2
  )

# Summarize total births and their uncertainty per country
births_summary <- new_df_births %>%
  group_by(country) %>%
  summarise(
    total_births = sum(births_best, na.rm = TRUE),
    totBirthsWidth = sqrt(sum(birthsWidthSq, na.rm = TRUE))  # SD propagation for births uncertainty
  ) %>%
  ungroup()

# Calculate HIV rate and propagate uncertainty for each country
summary(new_df_births$hiv)
new_df_births <- new_df_births %>%
  left_join(births_summary, by = "country") %>%
  mutate(
    hiv_best = hiv / total_births,  # pregnant women with HIV
    HIVwidth = hiv_best * sqrt(
      (totBirthsWidth / total_births)^2 +  # Uncertainty from total births
        (HIVwidth / hiv)^2  # Uncertainty from HIV
    )
  )

summary(new_df_births$hiv_best)
x <- new_df_births |> 
  filter(is.na(hiv_best)) |> 
  select(country, age_group, births_best, hiv, total_births, hiv_best, HIVwidth) |> 
  distinct(country)

names(new_df_births)[grepl('hiv',names(new_df_births))]

# Fill missing HIV values with region means for safety
new_df_births <- new_df_births %>%
  group_by(g_whoregion) %>%
  mutate(hiv_best = coalesce(hiv_best, mean(hiv_best, na.rm = TRUE)),
         HIVwidth = ifelse(
           is.na(HIVwidth) & is.na(hiv_best),
           sqrt(mean(HIVwidth^2, na.rm = TRUE)),
           HIVwidth
         )) %>%
  ungroup()
# 
# # if HIV > 1, then hiv_best = 0
# # some countries have more HIV pregnant women than total births
# # this arises from using region means
# new_df_births <- new_df_births %>%
#   mutate(
#     hiv_best = ifelse(hiv_best > 1, 0, hiv_best),
#     HIVwidth = ifelse(hiv_best > 1, 0, HIVwidth)
#   )

summary(new_df_births$hiv_best)

# check     
new_df_births |>
  filter(country == "Zimbabwe") |>
  select(matches("births|hiv|HIV"))

# Calculate births by HIV status and their uncertainties
new_df_births <- new_df_births %>%
  mutate(
    birthsH1 = births_best * hiv_best,
    birthsH1Width = birthsH1 * sqrt(
      (birthsWidth / births_best)^2 + (HIVwidth / hiv_best)^2
    ),
    birthsH0 = births_best - birthsH1,
    birthsH0Width = sqrt(birthsWidth^2 + birthsH1Width^2)
  )

# Add pregnancy & postpartum duration
new_df_births <- new_df_births %>%
  mutate(
    pregDur = 280/365,  # Duration of pregnancy in years
    ppDur = 91.25/365  # Duration of postpartum period in years
  )

# # Calculate person-years and TBI estimates
# new_df_births <- new_df_births %>%
#   mutate(
#     PY.P = births_best * pregDur,
#     PY.PH0 = birthsH0 * pregDur,
#     PY.PH1 = birthsH1 * pregDur,
#     PY.PP = births_best * ppDur,
#     PY.PPH0 = birthsH0 * ppDur,
#     PY.PPH1 = birthsH1 * ppDur,
#     TBI.P_best = births_best * pregDur * (TBI_best / pop_best),
#     TBI.PH0_best = birthsH0 * pregDur * (TBI_best / pop_best),
#     TBI.PH1_best = birthsH1 * pregDur * (TBI_best / pop_best),
#     TBI.PP_best = births_best * ppDur * (TBI_best / pop_best),
#     TBI.PPH0_best = birthsH0 * ppDur * (TBI_best / pop_best),
#     TBI.PPH1_best = birthsH1 * ppDur * (TBI_best / pop_best)
#     )
#          
# # Calculate uncertainties for person-years and TBI estimates
# new_df_births <- new_df_births %>%
#   mutate(
#     PY.Pwidth = pregDur * birthsWidth,
#     PY.PH0width = pregDur * birthsH0Width,
#     PY.PH1width = pregDur * birthsH1Width,
#     PY.PPwidth = ppDur * birthsWidth,
#     PY.PPH0width = ppDur * birthsH0Width,
#     PY.PPH1width = ppDur * birthsH1Width,
#     TBI.Pwidth = TBI.P_best * sqrt(
#       (TBIwidth / TBI_best)^2 + (birthsWidth / births_best)^2 + (popWidth / pop_best)^2
#       ),
#     TBI.PH0width = TBI.PH0_best * sqrt(
#       (TBIwidth / TBI_best)^2 + (birthsH0Width / births_best)^2 + (popWidth / pop_best)^2
#       ),
#     TBI.PH1width = TBI.PH1_best * sqrt(
#       (TBIwidth / TBI_best)^2 + (birthsH1Width / births_best)^2 + (popWidth / pop_best)^2
#       ),
#     TBI.PPwidth = TBI.PP_best * sqrt(
#       (TBIwidth / TBI_best)^2 + (birthsWidth / births_best)^2 + (popWidth / pop_best)^2
#       ),
#     TBI.PPH0width = TBI.PPH0_best * sqrt(
#       (TBIwidth / TBI_best)^2 + (birthsH0Width / births_best)^2 + (popWidth / pop_best)^2
#       ),
#     TBI.PPH1width = TBI.PPH1_best * sqrt(
#       (TBIwidth / TBI_best)^2 + (birthsH1Width / births_best)^2 + (popWidth / pop_best)^2
#       )
#     )
#         
# # Scale up TB in HIV by IRR
# new_df_births <- new_df_births %>%
#   left_join(tb_hiv_risk %>% select(country, IRR, IRRWidth), by = "country") %>%
#   mutate(
#     TBI.PH1_best = TBI.PH1_best * IRR,
#     TBI.PH1width = TBI.PH1_best * sqrt(
#       (TBI.PH1width / TBI.PH1_best)^2 + (IRRWidth / IRR)^2
#       ),
#     TBI.PPH1_best = TBI.PPH1_best * IRR,
#     TBI.PPH1width = TBI.PPH1_best * sqrt(
#       (TBI.PPH1width / TBI.PPH1_best)^2 + (IRRWidth / IRR)^2
#       )
#     ) %>%
#   select(-IRR, -IRRWidth)

## then hi = best + width/2
## then lo = best - width/2
## paying attention to any -ves
## and:
## to aggregate eg over countries, sum the square of the widths, and the sqrt them
## and then use as above to generate lo/hi around the aggregate best

# calc includes a workaround to remove negative values

# ---------------------------------------------------------------------
# This section is working on correcting split of TB cases by HIV status
# Calculate person-years and TBI estimates
summary(new_df_births$hiv_best)
new_df_births <- new_df_births %>%
  mutate(
    PY.P = births_best * pregDur,
    PY.PH0 = births_best * (1-hiv_best) * pregDur,
    PY.PH1 = births_best * hiv_best * pregDur,
    PY.PP = births_best * ppDur,
    PY.PPH0 = births_best * (1-hiv_best) * ppDur,
    PY.PPH1 = births_best * hiv_best * ppDur,
    TBI.P_best = PY.P * (TBI_best / pop_best),
    # TBI.PH0_best = birthsH0 * pregDur * (TBI_best / pop_best),
    # TBI.PH1_best = birthsH1 * pregDur * (TBI_best / pop_best),
    TBI.PP_best = PY.PP * (TBI_best / pop_best)
    # TBI.PPH0_best = birthsH0 * ppDur * (TBI_best / pop_best)
    # TBI.PPH1_best = birthsH1 * ppDur * (TBI_best / pop_best)
  )

new_df_births |> 
  summarise(sum(PY.P), sum(PY.PH0))
# Calculate uncertainties for person-years and TBI estimates
new_df_births <- new_df_births %>%
  mutate(
    PY.Pwidth = PY.P * sqrt(
      (birthsWidth / births_best)^2 + (HIVwidth / hiv_best)^2
    ),
    PY.PH0width = PY.PH0 * sqrt(
      (birthsWidth / births_best)^2 + (HIVwidth / hiv_best)^2
    ),
    PY.PH1width = PY.PH1 * sqrt(
      (birthsWidth / births_best)^2 + (HIVwidth / hiv_best)^2
    ),
    PY.PPwidth = PY.PP * sqrt(
      (birthsWidth / births_best)^2 + (HIVwidth / hiv_best)^2
    ),
    PY.PPH0width = PY.PPH0 * sqrt(
      (birthsWidth / births_best)^2 + (HIVwidth / hiv_best)^2
    ),
    PY.PPH1width = PY.PPH1 * sqrt(
      (birthsWidth / births_best)^2 + (HIVwidth / hiv_best)^2
    ),
    TBI.Pwidth = TBI.P_best * sqrt(
      (TBIwidth / TBI_best)^2 + (birthsWidth / births_best)^2
    ),
    TBI.PPwidth = TBI.PP_best * sqrt(
      (TBIwidth / TBI_best)^2 + (birthsWidth / births_best)^2 
    )
  )

# Split total TB incidence by HIV status with uncertainty
new_df_births <- new_df_births %>%
  left_join(tb_hiv_risk %>% select(country, IRR, IRRWidth), by = "country") 

names(new_df_births)
new_df_births |> 
  filter(country=='Zimbabwe') |> 
  select(age_group, TBI_best, pop_best, births_best, TBI.P_best,TBI.PP_best,hiv_best, IRR)

new_df_births <- new_df_births %>%
  mutate(
    # TB incidence in HIV-negative individuals during pregnancy
    TBI.PH0_best = (TBI.P_best * (1 - hiv_best)) / (IRR * hiv_best + (1 - hiv_best)),
    TBI.PH0width = TBI.PH0_best * sqrt(
      (TBI.Pwidth / TBI.P_best)^2 + (IRRWidth / IRR)^2 + (HIVwidth / hiv_best)^2
    ),
    
    # TB incidence in HIV-positive individuals during pregnancy
    TBI.PH1_best = (TBI.P_best * IRR * hiv_best) / (IRR * hiv_best + (1 - hiv_best)),
    TBI.PH1width = TBI.PH1_best * sqrt(
      (TBI.Pwidth / TBI.P_best)^2 + (IRRWidth / IRR)^2 + (HIVwidth / hiv_best)^2
    ),
    
    # TB incidence in HIV-negative individuals during postpartum
    TBI.PPH0_best = (TBI.PP_best * (1 - hiv_best)) / (IRR * hiv_best + (1 - hiv_best)),
    TBI.PPH0width = TBI.PPH0_best * sqrt(
      (TBI.PPwidth / TBI.PP_best)^2 + (IRRWidth / IRR)^2 + (HIVwidth / hiv_best)^2
    ),
    
    # TB incidence in HIV-positive individuals during postpartum
    TBI.PPH1_best = (TBI.PP_best * IRR * hiv_best) / (IRR * hiv_best + (1 - hiv_best)),
    TBI.PPH1width = TBI.PPH1_best * sqrt(
      (TBI.PPwidth / TBI.PP_best)^2 + (IRRWidth / IRR)^2 + (HIVwidth / hiv_best)^2
    )
  ) %>%
  select(-IRR, -IRRWidth)

# check
new_df_births |> 
  filter(country=='Zimbabwe') |> 
  select(age_group, TBI_best, pop_best, births_best, TBI.P_best, TBI.PH0_best, TBI.PH1_best, TBI.PP_best, TBI.PPH0_best, TBI.PPH1_best)

new_df_births |> 
  group_by(g_whoregion) |>
  summarise(TBI.P_best = sum(TBI.P_best),
            TBI.PH0_best = sum(TBI.PH0_best), 
            TBI.PH1_best = sum(TBI.PH1_best), 
            TBI.PP_best = sum(TBI.PP_best),
            TBI.PPH0_best = sum(TBI.PPH0_best), 
            TBI.PPH1_best = sum(TBI.PPH1_best),
            TBI.PHIV = 100*mean(TBI.PH1_best/TBI.P_best, na.rm = TRUE), 
            TBI.PPHIV = 100*mean(TBI.PPH1_best/TBI.PP_best, na.rm = TRUE))

# ---------------------------------------------------------------------

# Calculate lower and upper bounds for TBI estimates
new_df_births <- new_df_births %>%
  mutate(
    TBI.P_lo = TBI.P_best - TBI.Pwidth / 2,
    TBI.P_lo = ifelse(TBI.P_lo < 0, 0, TBI.P_lo),
    TBI.P_hi = TBI.P_best + TBI.Pwidth / 2,
    TBI.PH0_lo = TBI.PH0_best - TBI.PH0width / 2,
    TBI.PH0_lo = ifelse(TBI.PH0_lo < 0, 0, TBI.PH0_lo),
    TBI.PH0_hi = TBI.PH0_best + TBI.PH0width / 2,
    TBI.PH1_lo = TBI.PH1_best - TBI.PH1width / 2,
    TBI.PH1_lo = ifelse(TBI.PH1_lo < 0, 0, TBI.PH1_lo),
    TBI.PH1_hi = TBI.PH1_best + TBI.PH1width / 2,
    TBI.PP_lo = TBI.PP_best - TBI.PPwidth / 2,
    TBI.PP_lo = ifelse(TBI.PP_lo < 0, 0, TBI.PP_lo),
    TBI.PP_hi = TBI.PP_best + TBI.PPwidth / 2,
    TBI.PPH0_lo = TBI.PPH0_best - TBI.PPH0width / 2,
    TBI.PPH0_lo = ifelse(TBI.PPH0_lo < 0, 0, TBI.PPH0_lo),
    TBI.PPH0_hi = TBI.PPH0_best + TBI.PPH0width / 2,
    TBI.PPH1_lo = TBI.PPH1_best - TBI.PPH1width / 2,
    TBI.PPH1_lo = ifelse(TBI.PPH1_lo < 0, 0, TBI.PPH1_lo),
    TBI.PPH1_hi = TBI.PPH1_best + TBI.PPH1width / 2
  )

# Summarize TBI cases by country
# Probably don't need na.rm=TRUE here...
names(new_df_births)[grepl('TBI_',names(new_df_births))]
pregTB_cases <- new_df_births %>%
  group_by(country) %>%
  summarise(
    TBI.P_best = sum(TBI.P_best, na.rm = TRUE),
    TBI.P_W2 = sum((TBI.P_hi - TBI.P_lo)^2, na.rm = TRUE),
    TBI.PH0_best = sum(TBI.PH0_best, na.rm = TRUE),
    TBI.PH0_W2 = sum((TBI.PH0_hi - TBI.PH0_lo)^2, na.rm = TRUE),
    TBI.PH1_best = sum(TBI.PH1_best, na.rm = TRUE),
    TBI.PH1_W2 = sum((TBI.PH1_hi - TBI.PH1_lo)^2, na.rm = TRUE),
    TBI.PP_best = sum(TBI.PP_best, na.rm = TRUE),
    TBI.PP_W2 = sum((TBI.PP_hi - TBI.PP_lo)^2, na.rm = TRUE),
    TBI.PPH0_best = sum(TBI.PPH0_best, na.rm = TRUE),
    TBI.PPH0_W2 = sum((TBI.PPH0_hi - TBI.PPH0_lo)^2, na.rm = TRUE),
    TBI.PPH1_best = sum(TBI.PPH1_best, na.rm = TRUE),
    TBI.PPH1_W2 = sum((TBI.PPH1_hi - TBI.PPH1_lo)^2, na.rm = TRUE)
  ) %>%
  mutate(
    TBI.P_lo = TBI.P_best - sqrt(TBI.P_W2) / 2,
    TBI.P_hi = TBI.P_best + sqrt(TBI.P_W2) / 2,
    TBI.PH0_lo = TBI.PH0_best - sqrt(TBI.PH0_W2) / 2,
    TBI.PH0_hi = TBI.PH0_best + sqrt(TBI.PH0_W2) / 2,
    TBI.PH1_lo = TBI.PH1_best - sqrt(TBI.PH1_W2) / 2,
    TBI.PH1_hi = TBI.PH1_best + sqrt(TBI.PH1_W2) / 2,
    TBI.PP_lo = TBI.PP_best - sqrt(TBI.PP_W2) / 2,
    TBI.PP_hi = TBI.PP_best + sqrt(TBI.PP_W2) / 2,
    TBI.PPH0_lo = TBI.PPH0_best - sqrt(TBI.PPH0_W2) / 2,
    TBI.PPH0_hi = TBI.PPH0_best + sqrt(TBI.PPH0_W2) / 2,
    TBI.PPH1_lo = TBI.PPH1_best - sqrt(TBI.PPH1_W2) / 2,
    TBI.PPH1_hi = TBI.PPH1_best + sqrt(TBI.PPH1_W2) / 2
  )

## Total population and births
names(new_df_births)
glimpse(new_df_births)
new_df_births |>
  filter(country == "Zimbabwe") |>
  mutate(x = as.numeric(pop_hi)-as.numeric(pop_lo)) |>
  select(matches("pop|births"))
key_parms <- c("pop_best", "popWidthSq", "births_best", "birthsWidthSq", "birthsH0", "birthsH0WidthSq", "birthsH1", "birthsH1WidthSq")

df <- new_df_births |>
  dplyr::mutate(
    popWidthSq = (pop_hi-pop_lo)^2,
    birthsWidthSq = birthsWidth^2,
    birthsH0WidthSq = birthsH0Width^2,
    birthsH1WidthSq = birthsH1Width^2
  )

summary(df$popWidthSq) 

summary_regions_population <- df %>%
  dplyr::group_by(g_whoregion) %>%
  dplyr::summarise_at(key_parms, ~ sum(., na.rm = T)) %>%
  janitor::adorn_totals("row")

## add hi/lo
summary_regions_population <- summary_regions_population |>
  mutate(
    pop_best = pop_best,
    pop_lo = pop_best - sqrt(popWidthSq) / 2,
    pop_hi = pop_best + sqrt(popWidthSq) / 2,
    births_best = births_best,
    births_lo = births_best - sqrt(birthsWidthSq) / 2,
    births_hi = births_best + sqrt(birthsWidthSq) / 2,
    birthsH0 = birthsH0,
    birthsH0_lo = birthsH0 - sqrt(birthsH0WidthSq) / 2,
    birthsH0_hi = birthsH0 + sqrt(birthsH0WidthSq) / 2,
    birthsH1 = birthsH1,
    birthsH1_lo = birthsH1 - sqrt(birthsH1WidthSq) / 2,
    birthsH1_hi = birthsH1 + sqrt(birthsH1WidthSq) / 2
  ) |>
  select(
    g_whoregion, pop_best, pop_lo, pop_hi,
    births_best, births_lo, births_hi,
    birthsH0, birthsH0_lo, birthsH0_hi,
    birthsH1, birthsH1_lo, birthsH1_hi
  ) |>
  pivot_longer(cols = -g_whoregion, names_to = "key", values_to = "value") |>
  mutate(value = scales::comma(round(value, -2))) |>
  pivot_wider(names_from = key, values_from = value)

# looks okay:
# approximately 2 billion females aged 15-49 years in 2024
# 132 million births worldwide in 2024
summary_regions_population

write.csv(summary_regions_population, here::here("TBburden/outdata", "summary_regions_population_2022.csv"), row.names = FALSE)

## person time
key_parms <- names(new_df_births)[grep("PY", names(new_df_births))]
width_parms <- key_parms[grepl("width", key_parms)]

new_df_births <- new_df_births |>
  dplyr::mutate(across(all_of(width_parms), ~ .^2, .names = "{col}Sq"))

names(new_df_births)[grep("PY", names(new_df_births))]
key_parms <- key_parms[!grepl("width", key_parms)]
width_parms <- paste0(width_parms, "Sq")
key_parms <- c(key_parms, width_parms)

summary_regions_py <- new_df_births %>%
  dplyr::group_by(g_whoregion) %>%
  dplyr::summarise_at(key_parms, ~ sum(., na.rm = T)) %>%
  janitor::adorn_totals("row")

# formatC(1e6, format = "d", big.mark = ",")

## add hi/lo
names(summary_regions_py)
summary_regions_py <- summary_regions_py |>
  dplyr::mutate(
    PY.P_lo = PY.P - sqrt(PY.PwidthSq) / 2,
    PY.P_hi = PY.P + sqrt(PY.PwidthSq) / 2,
    PY.PH0_lo = PY.PH0 - sqrt(PY.PH0widthSq) / 2,
    PY.PH0_hi = PY.PH0 + sqrt(PY.PH0widthSq) / 2,
    PY.PH1_lo = PY.PH1 - sqrt(PY.PH1widthSq) / 2,
    PY.PH1_hi = PY.PH1 + sqrt(PY.PH1widthSq) / 2,
    PY.PP_lo = PY.PP - sqrt(PY.PPwidthSq) / 2,
    PY.PP_hi = PY.PP + sqrt(PY.PPwidthSq) / 2,
    PY.PPH0_lo = PY.PPH0 - sqrt(PY.PPH0widthSq) / 2,
    PY.PPH0_hi = PY.PPH0 + sqrt(PY.PPH0widthSq) / 2,
    PY.PPH1_lo = PY.PPH1 - sqrt(PY.PPH1widthSq) / 2,
    PY.PPH1_hi = PY.PPH1 + sqrt(PY.PPH1widthSq) / 2
  ) |>
  select(
    g_whoregion,
    PY.P, PY.P_lo, PY.P_hi,
    PY.PH0, PY.PH0_lo, PY.PH0_hi,
    PY.PH1, PY.PH1_lo, PY.PH1_hi,
    PY.PP, PY.PP_lo, PY.PP_hi,
    PY.PPH0, PY.PPH0_lo, PY.PPH0_hi,
    PY.PPH1, PY.PPH1_lo, PY.PPH1_hi
  ) |>
  pivot_longer(cols = -g_whoregion, names_to = "key", values_to = "value") |>
  dplyr::mutate(value = formatC(round(value, -2), format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_regions_py

write.csv(summary_regions_py, here::here("TBburden/outdata", "summary_regions_py_2022.csv"), row.names = FALSE)

# summary baseline TB incidence 15-54 years 
summary_regions_tbi <- new_df_births %>%
  dplyr::group_by(g_whoregion) %>%
  dplyr::summarise_at(c('TBI_best', 'TBIwidthSq'), ~ sum(., na.rm = T)) %>%
  janitor::adorn_totals("row")

# formatC(1e6, format = "d", big.mark = ",")

## add hi/lo
names(summary_regions_tbi)
summary_regions_tbi <- summary_regions_tbi |>
  dplyr::mutate(
    TBI_lo = TBI_best - sqrt(TBIwidthSq) / 2,
    TBI_hi = TBI_best + sqrt(TBIwidthSq) / 2
  ) |>
  select(
    g_whoregion,
    TBI=TBI_best, TBI_lo, TBI_hi,
  ) |>
  pivot_longer(cols = -g_whoregion, names_to = "key", values_to = "value") |>
  dplyr::mutate(value = formatC(round(value, -2), format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_regions_tbi

write.csv(summary_regions_tbi, here::here("TBburden/outdata", "summary_regions_tbi_2022.csv"), row.names = FALSE)

# TODO: start here

## NOTE PJD changed as should be summing variances
key_parms <- new_df_births %>%
  select(matches("TBI\\.")) %>%
  names()
best_parms <- key_parms[grepl("best", key_parms)]
width_parms <- key_parms[grepl("width", key_parms)]

new_df_births <- new_df_births |>
  mutate(
    across(all_of(width_parms), ~ .^2, .names = "{col}Sq")
  )

new_df_births %>%
  select(matches("TBI\\.")) %>%
  names()
width_parms <- paste0(width_parms, "Sq")
key_parms <- c(best_parms, width_parms)

summary_regions <- new_df_births %>%
  dplyr::group_by(g_whoregion) %>%
  dplyr::summarise_at(key_parms, ~ sum(., na.rm = T)) %>%
  janitor::adorn_totals("row")

## add hi/lo
summary_regions <- summary_regions |>
  mutate(
    TBI.P_lo = TBI.P_best - sqrt(TBI.PwidthSq) / 2,
    TBI.P_hi = TBI.P_best + sqrt(TBI.PwidthSq) / 2,
    TBI.PH0_lo = TBI.PH0_best - sqrt(TBI.PH0widthSq) / 2,
    TBI.PH0_hi = TBI.PH0_best + sqrt(TBI.PH0widthSq) / 2,
    TBI.PH1_lo = TBI.PH1_best - sqrt(TBI.PH1widthSq) / 2,
    TBI.PH1_hi = TBI.PH1_best + sqrt(TBI.PH1widthSq) / 2,
    TBI.PP_lo = TBI.PP_best - sqrt(TBI.PPwidthSq) / 2,
    TBI.PP_hi = TBI.PP_best + sqrt(TBI.PPwidthSq) / 2,
    TBI.PPH0_lo = TBI.PPH0_best - sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH0_hi = TBI.PPH0_best + sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH1_lo = TBI.PPH1_best - sqrt(TBI.PPH1widthSq) / 2,
    TBI.PPH1_hi = TBI.PPH1_best + sqrt(TBI.PPH1widthSq) / 2
  ) |>
  select(
    g_whoregion,
    TBI.P_best, TBI.P_lo, TBI.P_hi,
    TBI.PH0_best, TBI.PH0_lo, TBI.PH0_hi,
    TBI.PH1_best, TBI.PH1_lo, TBI.PH1_hi,
    TBI.PP_best, TBI.PP_lo, TBI.PP_hi,
    TBI.PPH0_best, TBI.PPH0_lo, TBI.PPH0_hi,
    TBI.PPH1_best, TBI.PPH1_lo, TBI.PPH1_hi
  ) |>
  pivot_longer(cols = -g_whoregion, names_to = "key", values_to = "value") |>
  mutate(value = pmax(value, 0)) |> # remove negative values: mostly on lo estimates for now
  mutate(value = formatC(round(value, -1), format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_regions

# check
summary_regions |> 
  select(TBI.P_best, TBI.PH0_best, TBI.PH1_best) |>
  mutate(across(c(TBI.P_best, TBI.PH0_best, TBI.PH1_best), ~as.numeric(gsub(",", "", .)))) |>
  mutate(TBI.PH1 = 100*TBI.PH1_best/TBI.P_best)

summary_regions_byagegroup <- new_df_births %>%
  group_by(g_whoregion, age_group) %>%
  summarise_at(vars(all_of(key_parms)), ~ sum(.x, na.rm = TRUE)) %>%
  ungroup()

## add hi/lo
summary_regions_byagegroup <- summary_regions_byagegroup |>
  mutate(
    TBI.P_lo = TBI.P_best - sqrt(TBI.PwidthSq) / 2,
    TBI.P_hi = TBI.P_best + sqrt(TBI.PwidthSq) / 2,
    TBI.PH0_lo = TBI.PH0_best - sqrt(TBI.PH0widthSq) / 2,
    TBI.PH0_hi = TBI.PH0_best + sqrt(TBI.PH0widthSq) / 2,
    TBI.PH1_lo = TBI.PH1_best - sqrt(TBI.PH1widthSq) / 2,
    TBI.PH1_hi = TBI.PH1_best + sqrt(TBI.PH1widthSq) / 2,
    TBI.PP_lo = TBI.PP_best - sqrt(TBI.PPwidthSq) / 2,
    TBI.PP_hi = TBI.PP_best + sqrt(TBI.PPwidthSq) / 2,
    TBI.PPH0_lo = TBI.PPH0_best - sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH0_hi = TBI.PPH0_best + sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH1_lo = TBI.PPH1_best - sqrt(TBI.PPH1widthSq) / 2,
    TBI.PPH1_hi = TBI.PPH1_best + sqrt(TBI.PPH1widthSq) / 2
  ) |>
  select(
    g_whoregion, age_group,
    TBI.P_best, TBI.P_lo, TBI.P_hi,
    TBI.PH0_best, TBI.PH0_lo, TBI.PH0_hi,
    TBI.PH1_best, TBI.PH1_lo, TBI.PH1_hi,
    TBI.PP_best, TBI.PP_lo, TBI.PP_hi,
    TBI.PPH0_best, TBI.PPH0_lo, TBI.PPH0_hi,
    TBI.PPH1_best, TBI.PPH1_lo, TBI.PPH1_hi
  ) |>
  pivot_longer(cols = -c(g_whoregion, age_group), names_to = "key", values_to = "value") |>
  mutate(value = pmax(value, 0)) |> # remove negative values: mostly on lo estimates for now
  mutate(value = formatC(round(value), format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_regions_byagegroup

write.csv(summary_regions_byagegroup, here::here("TBburden/outdata", "summary_regions_byagegroup_2022.csv"), row.names = FALSE)

# The 30 TB high burden countries
# TODO: check if this is an updated list
hbc <- c(
  "Angola", "Bangladesh", "Brazil", "China",
  "Democratic People's Republic of Korea", "Democratic Republic of the Congo",
  "Ethiopia", "India", "Indonesia", "Kenya", "Mozambique", "Myanmar", "Nigeria",
  "Pakistan", "Philippines", "Russian Federation", "South Africa", "Thailand",
  "United Republic of Tanzania", "Viet Nam", "Cambodia",
  "Central African Republic", "Congo", "Lesotho", "Liberia",
  "Namibia", "Papua New Guinea", "Sierra Leone", "Zambia", "Zimbabwe"
)

summary_hbc <- new_df_births %>%
  filter(country %in% hbc) %>%
  group_by(country) %>%
  summarise_at(vars(all_of(key_parms)), ~ sum(.x, na.rm = TRUE)) %>%
  janitor::adorn_totals("row")

## add hi/lo
summary_hbc <- summary_hbc |>
  mutate(
    TBI.P_lo = TBI.P_best - sqrt(TBI.PwidthSq) / 2,
    TBI.P_hi = TBI.P_best + sqrt(TBI.PwidthSq) / 2,
    TBI.PH0_lo = TBI.PH0_best - sqrt(TBI.PH0widthSq) / 2,
    TBI.PH0_hi = TBI.PH0_best + sqrt(TBI.PH0widthSq) / 2,
    TBI.PH1_lo = TBI.PH1_best - sqrt(TBI.PH1widthSq) / 2,
    TBI.PH1_hi = TBI.PH1_best + sqrt(TBI.PH1widthSq) / 2,
    TBI.PP_lo = TBI.PP_best - sqrt(TBI.PPwidthSq) / 2,
    TBI.PP_hi = TBI.PP_best + sqrt(TBI.PPwidthSq) / 2,
    TBI.PPH0_lo = TBI.PPH0_best - sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH0_hi = TBI.PPH0_best + sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH1_lo = TBI.PPH1_best - sqrt(TBI.PPH1widthSq) / 2,
    TBI.PPH1_hi = TBI.PPH1_best + sqrt(TBI.PPH1widthSq) / 2
  ) |>
  select(
    country,
    TBI.P_best, TBI.P_lo, TBI.P_hi,
    TBI.PH0_best, TBI.PH0_lo, TBI.PH0_hi,
    TBI.PH1_best, TBI.PH1_lo, TBI.PH1_hi,
    TBI.PP_best, TBI.PP_lo, TBI.PP_hi,
    TBI.PPH0_best, TBI.PPH0_lo, TBI.PPH0_hi,
    TBI.PPH1_best, TBI.PPH1_lo, TBI.PPH1_hi
  ) |>
  pivot_longer(cols = -country, names_to = "key", values_to = "value") |>
  mutate(value = pmax(value, 0)) |> # remove negative values: mostly on lo estimates for now
  mutate(value = formatC(round(value), format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_hbc

write.csv(summary_hbc, here::here("TBburden/outdata", "summary_hbc_2022.csv"), row.names = FALSE)


# Adjusting estimates for pregnancy and postpartum TB risk
# IRRs for pregnancy and postpartum TB
IRR <- data.table::fread(here::here("TBrisk/outdata/meta_summary.csv"))
IRR

# Keep only the IRRs for HIV & no HIV
IRR <- IRR[!period %in% c("P", "PP")]
tmp <- IRR
# tmp <- data.table::data.table(parm = c('preg', 'pp'),
#                               lo = c(1.17, 1.53),
#                               mid = c(1.34, 1.91),
#                               hi = c(1.54, 2.39))
tmp1 <- HEdtree::getLNparms(tmp[, m], (tmp[, hi] - tmp[, lo])^2 / 3.92^2, med = FALSE)

# curve(dlnorm(x,tmp1$mu[1],tmp1$sig[1]),from=0,to=3)
# curve(dlnorm(x,tmp1$mu[2],tmp1$sig[2]),from=0,to=3)
tmp[, DISTRIBUTION := paste0("LN(", tmp1$mu, ",", tmp1$sig, ")")] # LN distributions

nreps <- nrow(new_df_births)
P <- HEdtree::parse.parmtable(data.frame(tmp[, .(NAME = period, DISTRIBUTION)]))
PSA <- HEdtree::makePSA(nreps, P) # PSAE = PSA effects

PSA |>
  pivot_longer(cols = everything(), names_to = "key", values_to = "value") |>
  mutate(key = factor(key,
                      levels = c("PH0", "PH1", "PPH0", "PPH1"),
                      labels = c(
                        "Pregnancy no HIV data",
                        "Pregnancy HIV data",
                        "Postpartum no HIV data",
                        "Postpartum HIV data"
                      )
  )) |>
  ggplot(aes(x = value, fill = key)) +
  geom_histogram(bins = 50, alpha = 0.5, position = "identity") +
  theme_minimal() +
  facet_wrap(~key, scales = "free") +
  theme(legend.position = "none")

# names(new_df_births)[grepl('pregTBI_', names(new_df_births))]

names(new_df_births)
P <- names(new_df_births)[grepl("TBI.P", names(new_df_births))]
PP <- P[grepl(".PP", P)]
P <- P[!grepl(".PP", P)]

PH0 <- P[grepl("PH0", P)]
PH1 <- P[grepl("PH1", P)]
P <- P[!grepl("PH0|PH1", P)]

PPH0 <- PP[grepl("PH0", PP)]
PPH1 <- PP[grepl("PH1", PP)]
PP <- PP[!grepl("PH0|PH1", PP)]

# # merge in the IRRs
# new_df_births_adjusted <- cbind(new_df_births, PSA)
# 
# new_df_births_adjusted <- new_df_births_adjusted %>%
#   ungroup() %>%
#   mutate(
#     # across(all_of(P), ~ . * P),
#     across(all_of(PH0), ~ . * PH0),
#     across(all_of(PH1), ~ . * PH1),
#     # across(all_of(PP), ~ . * PP),
#     across(all_of(PPH0), ~ . * PPH0),
#     across(all_of(PPH1), ~ . * PPH1)
#   )

# merge in the IRRs
tmp2 <- tmp |> 
  select(period:hi) |> 
  mutate(width = hi - lo) |>
  select(-lo, -hi) |>
  pivot_longer(cols = -c(period), names_to = "name", values_to = "value") |>
  pivot_wider(names_from = c(period, name), values_from = "value", names_glue = "{period}_{name}")


new_df_births_adjusted <- cbind(new_df_births, tmp2)

new_df_births_adjusted <- new_df_births_adjusted %>%
  ungroup() %>%
  mutate(
    across(c(TBI.PH0_best, TBI.PH1_best, TBI.PPH0_best, TBI.PPH1_best), 
           .fns = ~ ., .names = "{.col}_tmp"),  # Create temporary backup variables
    
    TBI.PH0_best = TBI.PH0_best_tmp * PH0_m,
    TBI.PH1_best = TBI.PH1_best_tmp * PH1_m,
    TBI.PPH0_best = TBI.PPH0_best_tmp * PPH0_m,
    TBI.PPH1_best = TBI.PPH1_best_tmp * PPH1_m,
    TBI.P_best = TBI.PH0_best + TBI.PH1_best, # recalculate total
    TBI.PP_best = TBI.PPH0_best + TBI.PPH1_best, # recalculate total
    
    TBI.PH0width = TBI.PH0_best * sqrt(
      (TBI.PH0width / TBI.PH0_best_tmp)^2 + (PH0_width / PH0_m)^2),
    TBI.PH1width = TBI.PH1_best * sqrt(
      (TBI.PH1width / TBI.PH1_best_tmp)^2 + (PH1_width / PH1_m)^2),
    TBI.PPH0width = TBI.PPH0_best * sqrt(
      (TBI.PPH0width / TBI.PPH0_best_tmp)^2 + (PPH0_width / PPH0_m)^2),
    TBI.PPH1width = TBI.PPH1_best * sqrt(
      (TBI.PPH1width / TBI.PPH1_best_tmp)^2 + (PPH1_width / PPH1_m)^2),
    TBI.Pwidth = sqrt(TBI.PH0width^2 + TBI.PH1width^2), 
    TBI.PPwidth = sqrt(TBI.PPH0width^2 + TBI.PPH1width^2)
  ) %>%
  select(-ends_with("_tmp")) 

# check
new_df_births |> 
  filter(country=='Zimbabwe') |> 
  select(age_group, TBI.P_best, TBI.PH0_best, TBI.PH1_best, TBI.PP_best, TBI.PPH0_best, TBI.PPH1_best) 

new_df_births_adjusted |> 
  ungroup() |>
  filter(country=='Zimbabwe') |> 
  select(age_group, TBI.P_best, TBI.PH0_best, TBI.PH1_best, TBI.PP_best, TBI.PPH0_best, TBI.PPH1_best) 
# 
# new_df_births <- new_df_births_adjusted %>%
#   ungroup() %>%
#   # group_by(country, age_group) |> 
#   mutate(
#     TBI.P_best = TBI.PH0_best + TBI.PH1_best,
#     TBI.Pwidth = sqrt(TBI.PH0width^2 + TBI.PH1width^2),
#     TBI.P_lo = pmax(TBI.P_best - TBI.Pwidth / 2, 0),  
#     TBI.P_hi = TBI.P_best + TBI.Pwidth / 2
#   )

summary_regions_adjusted <- new_df_births_adjusted %>%
  dplyr::group_by(g_whoregion) %>%
  summarise_at(vars(all_of(key_parms)), ~ sum(.x, na.rm = TRUE)) %>%
  janitor::adorn_totals("row")

## add hi/lo
summary_regions_adjusted <- summary_regions_adjusted |>
  mutate(
    # TBI.P_best = TBI.PH0_best + TBI.PH1_best,
    # TBI.PwidthSq = sqrt(TBI.PH0widthSq + TBI.PH1widthSq)^2,
    TBI.P_lo = TBI.P_best - sqrt(TBI.PwidthSq) / 2,
    TBI.P_hi = TBI.P_best + sqrt(TBI.PwidthSq) / 2,
    TBI.PH0_lo = TBI.PH0_best - sqrt(TBI.PH0widthSq) / 2,
    TBI.PH0_hi = TBI.PH0_best + sqrt(TBI.PH0widthSq) / 2,
    TBI.PH1_lo = TBI.PH1_best - sqrt(TBI.PH1widthSq) / 2,
    TBI.PH1_hi = TBI.PH1_best + sqrt(TBI.PH1widthSq) / 2,
    TBI.PP_lo = TBI.PP_best - sqrt(TBI.PPwidthSq) / 2,
    TBI.PP_hi = TBI.PP_best + sqrt(TBI.PPwidthSq) / 2,
    TBI.PPH0_lo = TBI.PPH0_best - sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH0_hi = TBI.PPH0_best + sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH1_lo = TBI.PPH1_best - sqrt(TBI.PPH1widthSq) / 2,
    TBI.PPH1_hi = TBI.PPH1_best + sqrt(TBI.PPH1widthSq) / 2
  ) |>
  select(
    g_whoregion,
    TBI.P_best, TBI.P_lo, TBI.P_hi,
    TBI.PH0_best, TBI.PH0_lo, TBI.PH0_hi,
    TBI.PH1_best, TBI.PH1_lo, TBI.PH1_hi,
    TBI.PP_best, TBI.PP_lo, TBI.PP_hi,
    TBI.PPH0_best, TBI.PPH0_lo, TBI.PPH0_hi,
    TBI.PPH1_best, TBI.PPH1_lo, TBI.PPH1_hi
  ) |>
  pivot_longer(cols = -g_whoregion, names_to = "key", values_to = "value") |>
  mutate(value = pmax(value, 0)) |> # remove negative values: mostly on lo estimates for now
  mutate(value = formatC(round(value,-2), format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

# check
summary_regions |> 
  select(TBI.P_best,TBI.PH0_best, TBI.PH1_best) |>
  mutate(across(c(TBI.P_best,TBI.PH0_best, TBI.PH1_best), ~as.numeric(gsub(",", "", .)))) |>
  mutate(TBI.PH1 = 100*TBI.PH1_best/TBI.P_best)

summary_regions_adjusted |> 
  select(g_whoregion, TBI.P_best,TBI.PH0_best, TBI.PH1_best) |>
  mutate(across(c(TBI.P_best,TBI.PH0_best, TBI.PH1_best), ~as.numeric(gsub(",", "", .)))) |>
  mutate(TBI.PH1 = 100*TBI.PH1_best/TBI.P_best)

write.csv(summary_regions_adjusted, here::here("TBburden/outdata", "summary_regions_adjusted.csv"), row.names = FALSE)

summary_regions_byagegroup_adjusted <- new_df_births_adjusted %>%
  group_by(g_whoregion, age_group) %>%
  summarise_at(vars(all_of(key_parms)), ~ sum(.x, na.rm = TRUE))

## add hi/lo
summary_regions_byagegroup_adjusted <- summary_regions_byagegroup_adjusted |>
  mutate(
    # TBI.P_best = TBI.PH0_best + TBI.PH1_best,
    # TBI.PwidthSq = sqrt(TBI.PH0widthSq + TBI.PH1widthSq)^2,
    TBI.P_lo = TBI.P_best - sqrt(TBI.PwidthSq) / 2,
    TBI.P_hi = TBI.P_best + sqrt(TBI.PwidthSq) / 2,
    TBI.PH0_lo = TBI.PH0_best - sqrt(TBI.PH0widthSq) / 2,
    TBI.PH0_hi = TBI.PH0_best + sqrt(TBI.PH0widthSq) / 2,
    TBI.PH1_lo = TBI.PH1_best - sqrt(TBI.PH1widthSq) / 2,
    TBI.PH1_hi = TBI.PH1_best + sqrt(TBI.PH1widthSq) / 2,
    TBI.PP_lo = TBI.PP_best - sqrt(TBI.PPwidthSq) / 2,
    TBI.PP_hi = TBI.PP_best + sqrt(TBI.PPwidthSq) / 2,
    TBI.PPH0_lo = TBI.PPH0_best - sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH0_hi = TBI.PPH0_best + sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH1_lo = TBI.PPH1_best - sqrt(TBI.PPH1widthSq) / 2,
    TBI.PPH1_hi = TBI.PPH1_best + sqrt(TBI.PPH1widthSq) / 2
  ) |>
  select(
    g_whoregion, age_group,
    TBI.P_best, TBI.P_lo, TBI.P_hi,
    TBI.PH0_best, TBI.PH0_lo, TBI.PH0_hi,
    TBI.PH1_best, TBI.PH1_lo, TBI.PH1_hi,
    TBI.PP_best, TBI.PP_lo, TBI.PP_hi,
    TBI.PPH0_best, TBI.PPH0_lo, TBI.PPH0_hi,
    TBI.PPH1_best, TBI.PPH1_lo, TBI.PPH1_hi
  ) |>
  pivot_longer(cols = -c(g_whoregion, age_group), names_to = "key", values_to = "value") |>
  mutate(value = pmax(value, 0)) |> # remove negative values: mostly on lo estimates for now
  mutate(value = formatC(round(value), format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_regions_byagegroup_adjusted

summary_regions_byagegroup_adjusted |> 
  select(age_group, TBI.P_best,TBI.PH0_best, TBI.PH1_best) |>
  mutate(across(c(TBI.P_best,TBI.PH0_best, TBI.PH1_best), ~as.numeric(gsub(",", "", .)))) |>
  mutate(TBI.PH1 = 100*TBI.PH1_best/TBI.P_best)

write.csv(summary_regions_byagegroup_adjusted, here::here(
  "TBburden/outdata",
  "summary_regions_byagegroup_adjusted.csv"
), row.names = FALSE)

# The 30 TB high burden countries
summary_hbc_adjusted <- new_df_births_adjusted %>%
  filter(country %in% hbc) %>%
  group_by(country) %>%
  summarise_at(vars(all_of(key_parms)), ~ sum(.x, na.rm = TRUE)) %>%
  janitor::adorn_totals("row")

## add hi/lo
summary_hbc_adjusted <- summary_hbc_adjusted |>
  mutate(
    # TBI.P_best = TBI.PH0_best + TBI.PH1_best,
    # TBI.PwidthSq = sqrt(TBI.PH0widthSq + TBI.PH1widthSq)^2,
    TBI.P_lo = TBI.P_best - sqrt(TBI.PwidthSq) / 2,
    TBI.P_hi = TBI.P_best + sqrt(TBI.PwidthSq) / 2,
    TBI.PH0_lo = TBI.PH0_best - sqrt(TBI.PH0widthSq) / 2,
    TBI.PH0_hi = TBI.PH0_best + sqrt(TBI.PH0widthSq) / 2,
    TBI.PH1_lo = TBI.PH1_best - sqrt(TBI.PH1widthSq) / 2,
    TBI.PH1_hi = TBI.PH1_best + sqrt(TBI.PH1widthSq) / 2,
    TBI.PP_lo = TBI.PP_best - sqrt(TBI.PPwidthSq) / 2,
    TBI.PP_hi = TBI.PP_best + sqrt(TBI.PPwidthSq) / 2,
    TBI.PPH0_lo = TBI.PPH0_best - sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH0_hi = TBI.PPH0_best + sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH1_lo = TBI.PPH1_best - sqrt(TBI.PPH1widthSq) / 2,
    TBI.PPH1_hi = TBI.PPH1_best + sqrt(TBI.PPH1widthSq) / 2
  ) |>
  select(
    country,
    TBI.P_best, TBI.P_lo, TBI.P_hi,
    TBI.PH0_best, TBI.PH0_lo, TBI.PH0_hi,
    TBI.PH1_best, TBI.PH1_lo, TBI.PH1_hi,
    TBI.PP_best, TBI.PP_lo, TBI.PP_hi,
    TBI.PPH0_best, TBI.PPH0_lo, TBI.PPH0_hi,
    TBI.PPH1_best, TBI.PPH1_lo, TBI.PPH1_hi
  ) |>
  pivot_longer(cols = -country, names_to = "key", values_to = "value") |>
  mutate(value = pmax(value, 0)) |> # remove negative values: mostly on lo estimates for now
  mutate(value = formatC(round(value), format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_hbc_adjusted

write.csv(summary_hbc_adjusted, here::here("TBburden/outdata", "summary_hbc_adjusted.csv"), row.names = FALSE)

# country level estimates
summary_countries_adjusted <- new_df_births_adjusted %>%
  group_by(country) %>%
  summarise_at(vars(all_of(key_parms)), ~ sum(.x, na.rm = TRUE)) %>%
  janitor::adorn_totals("row")

summary_countries_adjusted <- summary_countries_adjusted %>%
  mutate(
    # TBI.P_best = TBI.PH0_best + TBI.PH1_best,
    # TBI.PwidthSq = sqrt(TBI.PH0widthSq + TBI.PH1widthSq)^2,
    TBI.P_lo = TBI.P_best - sqrt(TBI.PwidthSq) / 2,
    TBI.P_hi = TBI.P_best + sqrt(TBI.PwidthSq) / 2,
    TBI.PH0_lo = TBI.PH0_best - sqrt(TBI.PH0widthSq) / 2,
    TBI.PH0_hi = TBI.PH0_best + sqrt(TBI.PH0widthSq) / 2,
    TBI.PH1_lo = TBI.PH1_best - sqrt(TBI.PH1widthSq) / 2,
    TBI.PH1_hi = TBI.PH1_best + sqrt(TBI.PH1widthSq) / 2,
    TBI.PP_lo = TBI.PP_best - sqrt(TBI.PPwidthSq) / 2,
    TBI.PP_hi = TBI.PP_best + sqrt(TBI.PPwidthSq) / 2,
    TBI.PPH0_lo = TBI.PPH0_best - sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH0_hi = TBI.PPH0_best + sqrt(TBI.PPH0widthSq) / 2,
    TBI.PPH1_lo = TBI.PPH1_best - sqrt(TBI.PPH1widthSq) / 2,
    TBI.PPH1_hi = TBI.PPH1_best + sqrt(TBI.PPH1widthSq) / 2
  ) |>
  select(
    country,
    TBI.P_best, TBI.P_lo, TBI.P_hi,
    TBI.PH0_best, TBI.PH0_lo, TBI.PH0_hi,
    TBI.PH1_best, TBI.PH1_lo, TBI.PH1_hi,
    TBI.PP_best, TBI.PP_lo, TBI.PP_hi,
    TBI.PPH0_best, TBI.PPH0_lo, TBI.PPH0_hi,
    TBI.PPH1_best, TBI.PPH1_lo, TBI.PPH1_hi
  ) |>
  pivot_longer(cols = -country, names_to = "key", values_to = "value") |>
  # mutate(value = pmax(value, 0)) |> # remove negative values: mostly on lo estimates for now
  mutate(value = formatC(round(value), format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

write.csv(summary_countries_adjusted, here::here("TBburden/outdata", "summary_countries_22_adjusted.csv"), row.names = FALSE)

length(summary_countries_adjusted$country)

a <- new_df_births_adjusted |> 
  summarise(
    TBI_best = sum(TBI.P_best + TBI.PP_best)
  ) 

b <- df |> summarise(
  TBI_best = sum(TBI_best)
)

c <- df_age_sex |> 
  filter(sex == "f" & age_group == "15plus") |>
  summarise(
    TBI_best = sum(best, na.rm = TRUE)
  )

a/b*100
a/c*100

unique(df_age_sex$age_group)
# 15-54
d <- df_age_sex |> 
  filter(sex == "f" & age_group %in% c("15-24","25-34","35-44","45-54")) |>
  summarise(
    TBI_best = sum(best, na.rm = TRUE)
  )

a/d*100

# 15-44
e <- df_age_sex |> 
  filter(sex == "f" & age_group %in% c("15-24","25-34","35-44")) |>
  summarise(
    TBI_best = sum(best, na.rm = TRUE)
  )

a/e*100
