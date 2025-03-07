library(here)
library(tidyverse)
library(countrycode)
library(readxl)  # For reading Excel files
library(readr)   # For reading CSV files

# Load TB burden data
if (!file.exists(here::here("TBburden/indata/df_burden.Rdata"))) {
  df_burden <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=estimates", header = TRUE)
  save(df_burden, file = here::here("TBburden/indata/df2022.Rdata"))
} else {
  load(here::here("TBburden/indata/df_burden.Rdata"))
}

# Find the column names related to TB_HIV
tbhiv <- names(df_burden)[grepl('tbhiv_prct', names(df_burden))]
tbhiv
df_dic |> 
  filter(grepl('tbhiv_prct', variable_name)) |> 
  select(variable_name, definition)

# Clean country names and ISO3 codes
df_burden <- df_burden %>%
  mutate(
    country = countrycode(country, origin = "country.name", destination = "country.name"),
    iso3 = countrycode(iso3, origin = "iso3c", destination = "iso3c")
  )

# Check data for the most recent year
# slight differences
df_burden %>%
  filter(year == max(year)) %>%
  select(country, year, e_inc_num, e_inc_tbhiv_num, all_of(tbhiv[1])) %>%
  mutate(check = e_inc_tbhiv_num / e_inc_num * 100) %>%
  head(10)

df_burden %>%
  filter(year == max(year)) %>%
  select(country, year, e_inc_num, e_inc_tbhiv_num, all_of(tbhiv[1])) %>%
  mutate(check = e_inc_tbhiv_num / e_inc_num * 100) %>%
  tail(10)

# Keep only the most recent year & variables of interest
tb_hiv <- df_burden %>%
  filter(year == max(year)) %>%
  select(country, iso3, g_whoregion, all_of(tbhiv))

summary(tb_hiv)

# Dealing with missing values (using group mean)
tb_hiv <- tb_hiv %>%
  group_by(g_whoregion) %>%
  mutate(across(all_of(tbhiv), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()

# Rename columns for easier handling
names(tb_hiv) <- gsub('e_|_prct', '', names(tb_hiv))

# Calculate TB_HIV width and drop the individual lo/hi bounds
tb_hiv <- tb_hiv %>%
  mutate(
    tbhivWidth = tbhiv_hi - tbhiv_lo
  ) %>%
  select(-tbhiv_lo, -tbhiv_hi)

# Read in UNAIDS HIV data & dictionary
popn_hiv <- read_excel(here::here("TBburden/indata/UNAIDS_HIV2024Estimates.xlsx"),
                       skip = 3)

unaids_dic <- read_csv(here::here("TBburden/indata/unaids_dic.csv"))
names(popn_hiv)[1] <- 'year'
names(popn_hiv)[3] <- 'country'
unaids_dic$descriptions <- names(popn_hiv)
names(popn_hiv) <- unaids_dic$varnames

# Filter UNAIDS data for relevant variables
keep <- unaids_dic %>%
  filter(grepl('15f', varnames) & !grepl('deaths|New', descriptions)) %>%
  pull(varnames)

# Filter out "Global" and keep the most recent year
popn_hiv <- popn_hiv %>%
  filter(country != 'Global' & year == max(year)) %>%
  select(country, iso3, all_of(keep))

# Clean country names and ISO3 codes for UNAIDS data
popn_hiv <- popn_hiv %>%
  mutate(
    country = countrycode(country, origin = "country.name", destination = "country.name"),
    iso3 = countrycode(iso3, origin = "iso3c", destination = "iso3c")
  )

# Calculate the proportion of HIV+ in the population
popn_hiv <- popn_hiv %>%
  mutate(
    h15f_best = h15f / pop15f * 100,
    h15fWidth = (h15f.hi - h15f.lo) / pop15f * 100,
    h15fWidth = h15f_best * sqrt((h15fWidth / h15f_best)^2) # Fix the double comma issue here
  ) %>%
  select(-h15f, -h15f.hi, -h15f.lo, -pop15f)

# Merge TB and HIV data
tb_hiv <- tb_hiv %>%
  left_join(popn_hiv, by = c('country', 'iso3'))

# Handle missing values for HIV proportion in merged data
h15f <- names(tb_hiv)[grepl('h15f', names(tb_hiv))]
tb_hiv <- tb_hiv %>%
  group_by(g_whoregion) %>%
  mutate(across(all_of(h15f), ~replace(., is.na(.), mean(., na.rm = TRUE)))) %>%
  ungroup()

# Calculate IRR (Incidence Rate Ratio) for TB given HIV
tb_hiv <- tb_hiv %>%
  mutate(
    IRR = (tbhiv / (100 - tbhiv)) / (h15f_best / (100 - h15f_best)),
    IRRWidth = IRR * sqrt(
      (tbhivWidth / tbhiv)^2 + 
        (h15f_best / h15fWidth)^2
    )
  )

# Summarize IRR by WHO region
tb_hiv %>%
  group_by(g_whoregion) %>%
  summarise(IRR = mean(IRR, na.rm = TRUE),
            IRRWidth = sqrt(sum(IRRWidth^2, na.rm = TRUE) / n())/2) %>%
  ggplot(aes(y = IRR, x = g_whoregion)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = IRR - 1.96 * IRRWidth, ymax = IRR + 1.96 * IRRWidth, width = 0.2)) +
  theme_minimal() +
  labs(y = "Incidence Rate Ratio (IRR)", x = "WHO Region") # Add labels for clarity
