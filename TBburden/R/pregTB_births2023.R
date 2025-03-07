# Pregnancy and TB data

# Aim: construct a single merged dataset at country level

# Sources of data
# http://www.who.int/tb/country/data/download/en/
# https://extranet.who.int/tme/generateCSV.asp?ds=dictionary
# https://population.un.org/wpp/Download/Standard/Population/
# https://population.un.org/wpp/Download/Standard/Fertility/

cat("\014") # clears Console (in RStudio)
sessionInfo() # gives session info, ver of R, packages
rm(list = ls()) # removes work space environment

library(readxl)
library(tidyverse)
library(viridis)
library(here)
library(dplyr)
# library(plyr)
# library(janitor)
library(countrycode)
library(countries)

# load ISO country codes
load(here::here("TBburden/indata/isodict.Rdata"))
code <- read_csv(here::here("TBburden/indata", "all.csv")) # more codes details
code <- code %>%
  dplyr::rename(country = name, iso3 = `alpha-3`) %>%
  select(country, `country-code`, iso3, region, `sub-region`)

# country names differ from those in WHO data
ISO$country <- as.character(ISO$country)
ISO$country[ISO$country == "Czech Republic"] <- "Czechia"
ISO$country[ISO$country == "Serbia & Montenegro"] <- "Serbia"
ISO$country[ISO$country == "Swaziland"] <- "Eswatini"
ISO$country[ISO$country == "The Former Yugoslav Republic of Macedonia"] <- "North Macedonia"

names(ISO)
names(code)
head(code)
setdiff(ISO$country, code$country)
setdiff(code$country, ISO$country)
table(ISO$g_whoregion, useNA = "always")

ISO <- ISO |>
  mutate(country = countrycode(country, origin = "country.name", destination = "country.name"))
code <- code |>
  mutate(country = countrycode(country, origin = "country.name", destination = "country.name"))

setdiff(ISO$country, code$country)
setdiff(code$country, ISO$country)

ISOS <- code %>%
  select(country, iso3, region, sub_region = `sub-region`) |>
  left_join(ISO, by = c("country", "iso3")) |>
  select(country, iso2, iso3, region, sub_region, g_whoregion)

ISOS |>
  filter(is.na(g_whoregion))
ISOS |>
  filter(is.na(iso3))

table(ISOS$g_whoregion, ISOS$region, useNA = "always")

ISOS |>
  filter(is.na(g_whoregion) & region == "Africa")
ISOS |>
  filter(is.na(g_whoregion) & region == "Americas")
ISOS |>
  filter(is.na(g_whoregion) & region == "Europe")
ISOS |>
  filter(is.na(g_whoregion) & region == "Asia")
ISOS |>
  filter(is.na(g_whoregion) & region == "Oceania")
ISOS |>
  filter(g_whoregion == "EMR")

ISOS <- ISOS |>
  mutate(g_whoregion = case_when(
    region == "Africa" ~ "AFR",
    region == "Americas" ~ "AMR",
    region == "Europe" ~ "EUR",
    region == "Asia" ~ "SEA",
    region == "Oceania" ~ "WPR",
    TRUE ~ NA_character_
  ))

table(ISOS$g_whoregion, ISOS$region, useNA = "always")
ISOS |>
  filter(is.na(g_whoregion))

# read in WHO TB data disaggregated by age and sex
# read in the data dictionary
# Estimated number of incident cases (all forms) - Worked it out from the main dataset???
if (!file.exists(here::here("TBburden/indata/df2022.Rdata"))) {
  df <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=estimates_age_sex", header = TRUE)
  save(df, file = here::here("TBburden/indata/df2022.Rdata"))
} else {
  load(here::here("TBburden/indata/df2022.Rdata"))
}


if (!file.exists(here::here("TBburden/indata/df_dic.Rdata"))) {
  df_dic <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=dictionary")
  save(df_dic, file = here::here("TBburden/indata/df_dic.Rdata"))
} else {
  load(here::here("TBburden/indata/df_dic.Rdata"))
}

# Filter females of reproductive age group
df_ISO <- ISOS %>%
  select(country, iso3, g_whoregion)

setdiff(df$country, df_ISO$country)
setdiff(df_ISO$country, df$country)

length(unique(df$country))
length(unique(df_ISO$country))

# Update country names: going with the WHO names for now

#' TODO: look at `auto_merge` from `countries` package
# z2 <- auto_merge(df |>
#                    select(-iso2), ISO,
#                  by = c("country"),
#                  country_to = "UN_en",
#                  merging_info = FALSE)
# z2 |> filter(is.na(iso3))

df <- df %>%
  mutate(country = countrycode(country,
                               origin = "country.name",
                               destination = "country.name"
  ))


setdiff(df$country, df_ISO$country)
setdiff(df_ISO$country, df$country)

df_iso <- df_ISO %>%
  left_join(df, by = c("country", "iso3"))

df_1 <- df_iso %>%
  filter(
    sex == "f",
    age_group %in% c("15-24", "25-34", "35-44", "45-54")
  )
regional_prop <- df_1 %>%
  dplyr::group_by(g_whoregion, age_group) %>%
  dplyr::summarise(
    sum_best_age = sum(best, na.rm = T),
    sum_lo_age = sum(lo, na.rm = T),
    sum_hi_age = sum(hi, na.rm = T)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(g_whoregion) %>%
  dplyr::mutate(
    sum_best_over = sum(sum_best_age, na.rm = T),
    sum_lo_over = sum(sum_lo_age, na.rm = T),
    sum_hi_over = sum(sum_hi_age, na.rm = T)
  ) %>%
  # dplyr::ungroup() %>% dplyr::group_by(g_whoregion, age_group) %>%
  dplyr::mutate(
    prop_best = sum_best_age / sum_best_over,
    prop_lo = sum_lo_age / sum_lo_over,
    prop_hi = sum_hi_age / sum_hi_over
  )

df_2 <- df_1

length(unique(df_2$country)) # now 215, used to be 216

# Births by age of mother
# Births by five-year age group of mother, region, subregion and country, 1950-2100 (thousands)

# TODO: to check this new data ->> how to include uncertainty
# BD <- data.table::fread('~/Downloads/WPP2024_Fertility_by_Age5.csv.gz')
# print(object.size(BD),units='auto')
# # table(BD$Time)
# table(BD$Variant)
# # BD <- BD[Time==2022]
# BD[,unique(ISO3_code)]
# BD <- BD[ISO3_code!=""] # these do not include uncertainty
# BDM <- BD[Variant=="Medium" & Time==2024]
# BDL <- BD[Variant=="Low" & Time==2024]
# BDH <- BD[Variant=="High" & Time==2024]

# Using 2020-2025 projections from WPP2019 data
WPP2019_BAGEM <- read_excel(here::here("TBburden/indata/WPP2019_FERT_F06_BIRTHS_BY_AGE_OF_MOTHER.xlsx"),
                            sheet = "MEDIUM VARIANT", skip = 16
) # WPP2019_FERT_F06_BIRTHS_BY_AGE_OF_MOTHER MEDIUM VARIANT
WPP2019_BAGEL <- read_excel(here::here("TBburden/indata/WPP2019_FERT_F06_BIRTHS_BY_AGE_OF_MOTHER.xlsx"),
                            sheet = "LOW VARIANT", skip = 16
) # WPP2019_FERT_F06_BIRTHS_BY_AGE_OF_MOTHER LOW VARIANT
WPP2019_BAGEH <- read_excel(here::here("TBburden/indata/WPP2019_FERT_F06_BIRTHS_BY_AGE_OF_MOTHER.xlsx"),
                            sheet = "HIGH VARIANT", skip = 16
) # WPP2019_FERT_F06_BIRTHS_BY_AGE_OF_MOTHER HIGH VARIANT

names(WPP2019_BAGEM)
WPP2019_BAGEL
WPP2019_BAGEH

births <- rbind(WPP2019_BAGEM, WPP2019_BAGEL, WPP2019_BAGEH)

for (k in 1:length(names(births))) { # convert all the rows with type 'integer' to 'numeric
  if (is.integer(births[, k])) {
    births[, k] <- as.numeric(births[, k])
  }
}

names(births)[3] <- "country"
births |>
  filter(`Country code` > 900) |>
  select(country) |>
  distinct()


#
births <- births |>
  pivot_longer(cols = -c(1:8), names_to = "age", values_to = "value") |>
  mutate(value = as.numeric(value)) |>
  pivot_wider(names_from = age, values_from = value)


# recategorizing age groups to match the WHO TB data
table(df_2$age_group)

# births$`15plus` <- rowSums(births[7:13], na.rm = TRUE)
births$"15-24" <- rowSums(births[c("15-19", "20-24")], na.rm = TRUE)
births$"25-34" <- rowSums(births[c("25-29", "30-34")], na.rm = TRUE)
births$"35-44" <- rowSums(births[c("35-39", "40-44")], na.rm = TRUE)
births$"45-54" <- births$`45-49`

names(births)[3] <- "country"

births
births |>
  filter(country == "United Kingdom" & Period == "2020-2025")

births <- births %>%
  select(-c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")) %>%
  pivot_longer(cols = -c(Index:Period), names_to = "age_group", values_to = "estimate")

births <- separate(births, Period, c("from", "to"))
births$to <- as.numeric(births$to) - 1
births$year <- mapply(seq, births$from, births$to, SIMPLIFY = FALSE)
births$estimate <- (births$estimate * 1000) / 5 # convert to thousands

# check
births |>
  filter(country == "United Kingdom" & year == "2020:2024" & Variant == "Medium variant") |>
  summarise(sum(estimate, na.rm = T))

births <- births %>%
  unnest(year) %>%
  select(-from, -to) %>%
  spread(Variant, estimate)

births |>
  filter(country == "United Kingdom" & year == "2020") |>
  summarise(sum(`Medium variant`, na.rm = T))

births <- births %>% dplyr::rename(births_lo = `Low variant`, births_hi = `High variant`, births_best = `Medium variant`)

births <- births |>
  filter(!`Country code` > 900)
births_1 <- births %>% filter(!tolower(country) %in%
                                tolower(c(
                                  "WORLD", "More developed regions",
                                  "Less developed regions",
                                  "Least developed countries",
                                  "Less developed regions, excluding least developed countries",
                                  "Less developed regions, excluding China",
                                  "High-income countries",
                                  "Middle-income countries",
                                  "Upper-middle-income countries",
                                  "Lower-middle-income countries",
                                  "Low-income countries",
                                  "Sub-Saharan Africa",
                                  "AFRICA",
                                  "Eastern Africa",
                                  "Middle Africa",
                                  "Northern Africa",
                                  "Western Sahara",
                                  "Southern Africa",
                                  "Western Africa",
                                  "ASIA",
                                  "Eastern Asia",
                                  "South-Central Asia",
                                  "Central Asia",
                                  "Southern Asia",
                                  "South-Eastern Asia",
                                  "Western Asia",
                                  "EUROPE",
                                  "Eastern Europe",
                                  "Northern Europe",
                                  "Southern Europe",
                                  "Western Europe",
                                  "LATIN AMERICA AND THE CARIBBEAN",
                                  "Caribbean",
                                  "Central America",
                                  "South America",
                                  "NORTHERN AMERICA",
                                  "OCEANIA",
                                  "Australia/New Zealand",
                                  "Melanesia",
                                  "Micronesia",
                                  "Polynesia"
                                )))
unique(births_1$country)

# Changing some country names to match the WHO TB data and ISO codes #
setdiff(births_1$country, df_2$country)
setdiff(df_2$country, births_1$country)

length(unique(births_1$country))
length(unique(df_2$country))

# Using the countrycode package to clean up country names
births_1 <- births_1 %>%
  mutate(country = countrycode(country, origin = "country.name", destination = "country.name"))

setdiff(births_1$country, df_2$country)
setdiff(df_2$country, births_1$country)

# births_1$country <- (mapvalues((births_1$country),
#   from = c(
#     "United Kingdom",
#     "Dem. People's Republic of Korea",
#     "Micronesia (Fed. States of)",
#     "State of Palestine",
#     "TFYR Macedonia",
#     "Swaziland",
#     # "Czech Republic",
#     "Samoa",
#     "United States Virgin Islands"
#   ),
#   to = c(
#     "United Kingdom of Great Britain and Northern Ireland",
#     "Democratic People's Republic of Korea",
#     "Micronesia (Federated States of)",
#     "West Bank and Gaza Strip",
#     "North Macedonia",
#     "Eswatini",
#     # "Czechia",
#     "American Samoa",
#     "US Virgin Islands"
#   )
# ))


births_2 <- births_1 %>%
  select(-c("Notes", `Country code`)) %>%
  left_join(df_ISO, by = "country")
# births2<- births %>% select(-c("Notes")) %>% left_join(code, by = "country", `Country code`)%>% filter(!is.na(iso3))
length(unique(births_2$country)) # 200
# births_2 <- births_1 %>% filter(age_group %in% c("15-24", "25-34", "35-44", "45-54")) %>% filter(country %in% df_1$country)
# length(unique(births$country))

# births_3 <- births_1 %>% filter(country %in% df_2$country) %>% filter(age_group=="15plus")
#
# births_4 <- rbind(births_2, births_3)
# births$iso3 <- as.character(births$iso3 )
# births <- births %>% mutate(iso3 = ifelse(country=="Mayotte", "MYT",
#                                           ifelse(country=="Réunion", "REU",
#                                                  ifelse(country=="China, Taiwan Province of China", "TWN",
#                                                         ifelse(country=="Guadeloupe", "GLP",
#                                                                ifelse(country=="Martinique", "MTQ",
#                                                                       ifelse(country=="United States Virgin Islands", "VIR",
#                                                                              ifelse(country=="French Guiana", "GUF",
#                                                                                     iso3))))))))
# filter 2022 data to match the WHO TB data
births_2022 <- births_2 %>%
  filter(year == 2022) %>%
  select(country, iso3, g_whoregion, age_group, births_best, births_lo, births_hi)
length(unique(births_2022$country))

# read in population of females in the reproductive age group
# Annual female population by five-year age group, region, subregion and country, 1950-2100 (thousands)
pop_f <- read_excel(here::here("TBburden/indata/WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.xlsx"),
                    sheet = "MEDIUM VARIANT", skip = 16
) # WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE MEDIUM VARIANT

names(pop_f)
pop_f <- pop_f %>%
  dplyr::rename(pop_f = Variant, country = `Region, subregion, country or area *`, year = `Reference date (as of 1 July)`)

pop_f |>
  filter(`Country code` > 900) |>
  select(country) |>
  distinct() |>
  pull()

pop_f <- pop_f |>
  filter(!`Country code` > 900)

#
pop_f <- pop_f |>
  pivot_longer(cols = -c(Index:year), names_to = "age", values_to = "value") |>
  mutate(value = as.numeric(value)) |>
  pivot_wider(names_from = age, values_from = value)

# pop_f$`15plus` <- rowSums(pop_f[10:27], na.rm = TRUE)
pop_f$"15-24" <- rowSums(pop_f[c("15-19", "20-24")], na.rm = TRUE)
pop_f$"25-34" <- rowSums(pop_f[c("25-29", "30-34")], na.rm = TRUE)
pop_f$"35-44" <- rowSums(pop_f[c("35-39", "40-44")], na.rm = TRUE)
pop_f$"45-54" <- rowSums(pop_f[c("45-49", "50-54")], na.rm = TRUE)

names(pop_f)
pop_f <- pop_f %>%
  select(c("country", "pop_f", "year", "15-24", "25-34", "35-44", "45-54")) %>%
  gather(age_group, pop_f, c("15-24", "25-34", "35-44", "45-54"))
pop_f$pop_f <- pop_f$pop_f * 1000

# pop_f1 <- pop_f %>% filter(age_group %in% c("15-24", "25-34", "35-44", "45-54")) #%>% filter(country %in% df_1$country)
# pop_f2 <- pop_f %>% filter(country %in% df_2$country) %>% filter(age_group=="15plus")


# pop_f3 <- rbind(pop_f1, pop_f2)

pop_f1 <- pop_f %>% filter(!country %in%
                             c(
                               "WORLD", "More developed regions",
                               "Less developed regions",
                               "Least developed countries",
                               "Less developed regions, excluding least developed countries",
                               "Less developed regions, excluding China",
                               "High-income countries",
                               "Middle-income countries",
                               "Upper-middle-income countries",
                               "Lower-middle-income countries",
                               "Low-income countries",
                               "Sub-Saharan Africa",
                               "AFRICA",
                               "Eastern Africa",
                               "Middle Africa",
                               "Northern Africa",
                               "Western Sahara",
                               "Southern Africa",
                               "Western Africa",
                               "ASIA",
                               "Eastern Asia",
                               "South-Central Asia",
                               "Central Asia",
                               "Southern Asia",
                               "South-Eastern Asia",
                               "Western Asia",
                               "EUROPE",
                               "Eastern Europe",
                               "Northern Europe",
                               "Southern Europe",
                               "Western Europe",
                               "LATIN AMERICA AND THE CARIBBEAN",
                               "Caribbean",
                               "Central America",
                               "South America",
                               "NORTHERN AMERICA",
                               "OCEANIA",
                               "Australia/New Zealand",
                               "Melanesia",
                               "Micronesia",
                               "Polynesia"
                             ))

length(unique(pop_f1$country)) # 200

pop_f1 <- pop_f1 %>%
  mutate(country = countrycode(country, origin = "country.name", destination = "country.name"))

# b<- df_ISO %>% filter(!country %in% pop_f1$country)
# pop_f1$country[grep("Wallis", pop_f1$country) ]
# df_ISO$country[grep("Greenland", df_ISO$country) ]
# Changing some country names to match the WHO TB data and ISO codes #

# pop_f1$country <- (mapvalues((pop_f1$country),
#   from = c(
#     "United Kingdom",
#     "Dem. People's Republic of Korea",
#     "Micronesia (Fed. States of)",
#     "State of Palestine",
#     "TFYR Macedonia",
#     "Czechia",
#     "Samoa",
#     "Swaziland",
#     "United States Virgin Islands"
#   ),
#   to = c(
#     "United Kingdom of Great Britain and Northern Ireland",
#     "Democratic People's Republic of Korea",
#     "Micronesia (Federated States of)",
#     "West Bank and Gaza Strip",
#     "North Macedonia",
#     "Czech Republic",
#     "American Samoa",
#     "Eswatini",
#     "US Virgin Islands"
#   )
# ))

pop_f2 <- pop_f1 %>%
  left_join(df_ISO, by = "country") %>%
  select(country, iso3, age_group, year, pop_f)
length(unique(pop_f2$country)) # 200

# missing_iso3 <- pop_f3 %>% filter(!country %in% ISO$country) %>% select(country, iso3) %>% filter(!duplicated(country))
# pop_f3$iso3 <- as.character(pop_f3$iso3)
# pop_f3 <- pop_f3 %>% mutate(iso3 = ifelse(country=="Mayotte", "MYT",
#                             ifelse(country=="Réunion", "REU",
#                                    ifelse(country=="China, Taiwan Province of China", "TWN",
#                                           ifelse(country=="Guadeloupe", "GLP",
#                                                  ifelse(country=="Martinique", "MTQ",
#                                                         ifelse(country=="United States Virgin Islands", "VIR",
#                                                                ifelse(country=="French Guiana", "GUF",
#                                                                       iso3))))))))

# (no_iso3 <- pop_f2 |>
#     filter(is.na(iso3)) |>
#     select(country) |>
#     distinct())
#
# df_ISO |>
#   filter(country %in% no_iso3$country)
# no_iso3 <- code |>
#   filter(country %in% no_iso3$country) |>
#   select(country, iso3) |>
#   left_join(no_iso3, by = "country")
#
#
# pop_f2022 <- pop_f2 %>%
#   left_join(no_iso3, by = "country") %>%
#   mutate(iso3 = coalesce(iso3.x, iso3.y)) %>%
#   filter(year == 2022) %>%
#   select(-year, -country, -iso3.x, -iso3.y)

pop_f2022 <- pop_f2 %>%
  filter(year == 2022)

# (no_iso3 <- births_2022 |>
#     filter(is.na(iso3)) |>
#     select(country) |>
#     distinct())
#
# no_iso3 <- code |>
#   filter(country %in% no_iso3$country | grepl('Taiwan', country))  |>
#   mutate(country = ifelse(grepl('Taiwan', country), 'China, Taiwan Province of China', country)) |>
#   left_join(no_iso3, by = "country")|>
#   select(country, iso3, g_whoregion=region)
#
# num_births <- births_2022 %>%
#   left_join(no_iso3, by = "country") %>%
#   mutate(iso3 = coalesce(iso3.x, iso3.y),
#          g_whoregion = coalesce(g_whoregion.x, g_whoregion.y)) %>%
#   left_join(pop_f2022, by = c("iso3", "age_group")) %>%
#   filter(country!='Channel Islands') %>%
#   select(-country, -iso3.x, -iso3.y, -g_whoregion.x, -g_whoregion.y)

num_births <- births_2022 %>%
  left_join(pop_f2022, by = c("country", "iso3", "age_group")) %>%
  filter(country != "Channel Islands")

table(num_births$g_whoregion)
table(ISOS$g_whoregion)

# num_births <- num_births %>%
#   mutate(g_whoregion = case_when(
#     g_whoregion == 'Africa' ~ "AFR",
#     g_whoregion == 'Americas' ~ "AMR",
#     .default = g_whoregion)
#   )

table(num_births$g_whoregion, useNA = "always")
length(unique(num_births$country))
# %>%
# gather(metric, value, c("births_best", "births_lo", "births_hi"))
# num_births$value <- num_births$pop_f * (num_births$value/1000) #  absolute number of births
# num_births <- num_births %>% spread(metric, value) %>% select(-country)

# HIV data
hiv_data <- data.table::fread(here::here("TBburden/indata/hiv_data.csv"))

names(hiv_data)
table(hiv_data$Indicator, hiv_data$IndicatorCode)

hiv_data |>
  distinct(Indicator, IndicatorCode)

# Indicator  IndicatorCode
# <char>         <char>
#   1: Estimated number of pregnant women living with HIV needing antiretrovirals for preventing mother-to-child transmission HIV_0000000021
# 2:                                                                     Estimated number of pregnant women living with HIV HIV_0000000020
# 3:      Number of pregnant women living with HIV who received antiretrovirals for preventing mother-to-child transmission HIV_0000000015
hiv_data <- hiv_data |>
  mutate(
    IndicatorCode = gsub("00000000", "", IndicatorCode),
    Indicator = case_when(
      IndicatorCode == "HIV_21" ~ "hiv",
      IndicatorCode == "HIV_20" ~ "hiv_prop",
      IndicatorCode == "HIV_15" ~ "art",
      .default = NA_character_
    )
  ) |>
  select(
    country = Location, iso3 = SpatialDimValueCode,
    g_whoregion = ParentLocationCode, year = Period, IndicatorCode, Indicator,
    latest = IsLatestYear, value = FactValueNumeric,
    value_lo = FactValueNumericLow, value_hi = FactValueNumericHigh
  )

# keep latest year
hiv_data <- hiv_data %>%
  filter(latest) %>%
  select(-latest, -IndicatorCode, -year)

hiv_data <- hiv_data |>
  pivot_wider(names_from = Indicator, names_glue = "{Indicator}_{.value}", values_from = c(value, value_lo, value_hi)) |>
  pivot_longer(cols = -c(country, iso3, g_whoregion), names_to = "Indicator", values_to = "value") |>
  mutate(Indicator = gsub("_value", "", Indicator)) |>
  # dplyr::group_by(g_whoregion, Indicator) |>
  # mutate(value = coalesce(value, mean(value, na.rm = TRUE))) |> # giving implausible values for some countries
  ungroup() |>
  pivot_wider(names_from = Indicator, values_from = value)



# 194 countries with 3 rows each
hiv_data |>
  select(country) |>
  distinct() |>
  pull() |>
  length()

# clean up some country names
length(unique(hiv_data$country)) # 194

hiv_data <- hiv_data %>%
  mutate(
    country = countrycode(country, origin = "country.name", destination = "country.name"),
    iso3 = countrycode(iso3, origin = "iso3c", destination = "iso3c")
  ) |>
  select(country, iso3, hiv, hiv_lo, hiv_hi) # just keep # needing ART


# merge with births data
names(hiv_data)
# setdiff(hiv_data$country, num_births$country)
# setdiff(num_births$country, hiv_data$country)
num_births <- num_births %>%
  left_join(hiv_data, by = c("country", "iso3"))

hiv_data |>
  filter(country == "Afghanistan")

# combining the different datasets -
# births, population of females in the reproductive age and
# TB incident case in women in the reproductive age group
new_df_births <- df_2 %>%
  select(country, iso3, iso_numeric, year, age_group, best, lo, hi) %>%
  left_join(num_births, by = c("country", "iso3", "age_group")) %>%
  dplyr::rename(TBI_best = best, TBI_lo = lo, TBI_hi = hi) %>%
  filter(!is.na(g_whoregion))
length(unique(new_df_births$country)) # 192

setdiff(unique(new_df_births$country), unique(num_births$country))
setdiff(unique(num_births$country), unique(new_df_births$country))

setdiff(unique(new_df_births$country), unique(df_2$country))
setdiff(unique(df_2$country), unique(new_df_births$country))
# new_df_births$g_whoregion[is.na(new_df_births$g_whoregion)] <- "AFR" # problem with Swaziland

# df_2 <- ISO %>% select(-iso2, -iso3, -iso_numeric) %>% left_join(df, by="country")

# Estimate the incidence of TB in pregnancy
# new_df_births$pregTBI_best <- new_df_births$births_best * 280/365 * (new_df_births$TBI_best/new_df_births$pop_f)
# new_df_births$pregTBI_lo   <- new_df_births$births_lo   * 280/365 * (new_df_births$TBI_lo/new_df_births$pop_f)
# new_df_births$pregTBI_hi   <- new_df_births$births_hi   * 280/365 * (new_df_births$TBI_hi/new_df_births$pop_f)
#
# # Estimate the incidence of TB in post-partum period
# new_df_births$ppTBI_best <- new_df_births$births_best * 91.25/365 * (new_df_births$TBI_best/new_df_births$pop_f)
# # new_df_births$ppTBI_lo   <- new_df_births$births_lo   * 91.25/365 * (new_df_births$TBI_lo/new_df_births$pop_f)
# # new_df_births$ppTBI_hi   <- new_df_births$births_hi   * 91.25/365 * (new_df_births$TBI_hi/new_df_births$pop_f)
# new_df_births$ppTBI_lo   <- new_df_births$births_best * 91.25/365 * (new_df_births$TBI_best/new_df_births$pop_f)
# new_df_births$ppTBI_hi   <- new_df_births$births_best * 91.25/365 * (new_df_births$TBI_best/new_df_births$pop_f)

# new_df_births$pregTBI_best_sd <- sqrt((new_df_births$births_hi - new_df_births$births_best)^2 + (new_df_births$TBI_hi - new_df_births$TBI_best)^2)
# new_df_births$pregTBI_best_sd1 <- sqrt((0.5*(new_df_births$births_hi - new_df_births$births_lo)/sqrt(3))^2 + (0.5*(new_df_births$TBI_hi - new_df_births$TBI_lo)/sqrt(3))^2)
# new_df_births$pregTBI_best_sd2 <- sqrt((0.5*(new_df_births$births_hi - new_df_births$births_lo))^2 + (0.5*(new_df_births$TBI_hi - new_df_births$TBI_lo))^2)
# new_df_births$pregTBI_best_sd3 <- sqrt(((new_df_births$births_hi - new_df_births$births_lo)/sqrt(12))^2 + ((new_df_births$TBI_hi - new_df_births$TBI_lo)/sqrt(12))^2)

## use the 'width' -- there will be a common factor of 3.92 for all contributors:
# Fill missing HIV values with region means for safety
new_df_births <- new_df_births |>
  dplyr::group_by(g_whoregion) |>
  dplyr::mutate(across(matches("hiv"), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) |> # safety
  ungroup()

# Add uncertainty widths for births, HIV, and TBI
new_df_births <- new_df_births |>
  dplyr::mutate(
    birthsWidth = births_hi - births_lo,
    birthsWidthSq = birthsWidth^2,
    HIVwidth = hiv_hi - hiv_lo,
    HIVwidthSq = HIVwidth^2,
    TBIwidth = TBI_hi - TBI_lo,
    TBIwidthSq = TBIwidth^2
  )

# Summarize total births and their uncertainty per country
births_summary <- new_df_births %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    tot.births = sum(births_best, na.rm = TRUE),
    totBirthsWidth = sqrt(sum(birthsWidthSq, na.rm = TRUE)) # SD propagation for births uncertainty
  ) %>%
  ungroup()

# Now, calculate HIV rate and propagate uncertainty for each country
new_df_births <- new_df_births %>%
  left_join(births_summary, by = "country") %>%
  mutate(
    # Best estimate HIV rate
    hiv_best = hiv / tot.births, # no age specific
    
    # Propagate uncertainty for HIV rate
    HIVwidth = hiv_best * sqrt(
      (birthsWidth / births_best)^2 + # uncertainty from birthsWidth
        (totBirthsWidth / tot.births)^2 + # uncertainty from total births
        (HIVwidth / hiv)^2 # uncertainty from HIV
    )
  )


new_df_births |>
  filter(country == "Zimbabwe") |>
  select(matches("births|hiv"))

new_df_births <- new_df_births |>
  mutate(
    birthsH1 = births_best * hiv_best,
    birthsH1Width = birthsH1 * sqrt(
      (birthsWidth / births_best)^2 +
        (HIVwidth / hiv_best)^2
    ),
    birthsH0 = births_best - birthsH1,
    birthsH0Width = sqrt(
      birthsWidth^2 + birthsH1Width^2
    )
  )

## calx
names(new_df_births)
new_df_births <- new_df_births |>
  mutate(
    PY.P = births_best * 280 / 365,
    PY.PH0 = birthsH0 * 280 / 365,
    PY.PH1 = birthsH1 * 280 / 365,
    PY.PP = births_best * 91.25 / 365,
    PY.PPH0 = birthsH0 * 91.25 / 365,
    PY.PPH1 = birthsH1 * 91.25 / 365,
    TBI.P_best = births_best * 280 / 365 * (TBI_best / pop_f),
    TBI.PH0_best = birthsH0 * 280 / 365 * (TBI_best / pop_f),
    TBI.PH1_best = birthsH1 * 280 / 365 * (TBI_best / pop_f),
    TBI.PP_best = births_best * 91.25 / 365 * (TBI_best / pop_f),
    TBI.PPH0_best = birthsH0 * 91.25 / 365 * (TBI_best / pop_f),
    TBI.PPH1_best = birthsH1 * 91.25 / 365 * (TBI_best / pop_f)
  )

options(scipen = 999)
## A = B*C
## log(A) = log(B) + log(C)
## differentiate:  dA / A = dB/B + dC/C
## (A.sd/A)^2 = (B.sd/B)^2 + (C.sd/C)^2

# new_df_births <- new_df_births |>
#   mutate(
#     PY.P_width = PY.P *
#       sqrt((birthsWidth/births_best)^2),
#     PY.PP_width = PY.PP *
#       sqrt((birthsWidth/births_best)^2),
#     pregTBIwidth = pregTBI_best *
#       sqrt((TBIwidth/TBI_best)^2 + (birthsWidth/births_best)^2),
#     ppTBIwidth = ppTBI_best *
#       sqrt((TBIwidth/TBI_best)^2 + (birthsWidth/births_best)^2)
#   )

new_df_births <- new_df_births |>
  mutate(
    PY.Pwidth = 280 / 365 * birthsWidth,
    PY.PH0width = 280 / 365 * birthsH0Width,
    PY.PH1width = 280 / 365 * birthsH1Width,
    PY.PPwidth = 91.25 / 365 * birthsWidth,
    PY.PPH0width = 91.25 / 365 * birthsH0Width,
    PY.PPH1width = 91.25 / 365 * birthsH1Width,
    TBI.Pwidth = TBI.P_best *
      sqrt((TBIwidth / TBI_best)^2 + (birthsWidth / births_best)^2),
    TBI.PH0width = TBI.PH0_best *
      sqrt((TBIwidth / TBI_best)^2 + (birthsH0Width / births_best)^2),
    TBI.PH1width = TBI.PH1_best *
      sqrt((TBIwidth / TBI_best)^2 + (birthsH1Width / births_best)^2),
    TBI.PPwidth = TBI.PP_best *
      sqrt((TBIwidth / TBI_best)^2 + (birthsWidth / births_best)^2),
    TBI.PPH0width = TBI.PPH0_best *
      sqrt((TBIwidth / TBI_best)^2 + (birthsH0Width / births_best)^2),
    TBI.PPH1width = TBI.PPH1_best *
      sqrt((TBIwidth / TBI_best)^2 + (birthsH1Width / births_best)^2)
  )

## TODO -- PJD
## then hi = best + width/2
## then lo = best - width/2
## paying attention to any -ves
## and:
## to aggregate eg over countries, sum the square of the widths, and the sqrt them
## and then use as above to generate lo/hi around the aggregate best

# calc includes a workaround to remove negative values
new_df_births <- new_df_births |>
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

# new_df_births <- new_df_births %>%
#   mutate(
#     PY.P_lo = PY.P - (PY.P_width)/2,
#     PY.P_hi = PY.P + (PY.P_width)/2,
#     PY.PP_lo = PY.PP - (PY.PP_width)/2,
#     PY.PP_hi = PY.PP + (PY.PP_width)/2
#   )
#
# new_df_births |>
#   select(contains('py_')) |>
#   select(-contains('width')) |>
#   summary()
#
# new_df_births |>
#   ungroup() |>
#   group_by(g_whoregion) |>
#   dplyr::summarise(PY.P = mean(PY.P),
#             PY.PP = mean(PY.PP))
## then you
## PJD NOTE you didn't need na.rm=TRUE here...
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
  )

summary(pregTB_cases$TBI.PH1_W2)
pregTB_cases <- pregTB_cases %>%
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

length(unique(new_df_births$country))

## Total population and births
names(new_df_births)
key_parms <- c("pop_f", "births_best", "birthsWidthSq", "birthsH0", "birthsH0WidthSq", "birthsH1", "birthsH1WidthSq")

df <- new_df_births |>
  mutate(
    birthsWidthSq = birthsWidth^2,
    birthsH0WidthSq = birthsH0Width^2,
    birthsH1WidthSq = birthsH1Width^2
  )

summary_regions_population <- df %>%
  dplyr::group_by(g_whoregion) %>%
  dplyr::summarise_at(key_parms, ~ sum(., na.rm = T)) %>%
  janitor::adorn_totals("row")

## add hi/lo
summary_regions_population <- summary_regions_population |>
  mutate(
    pop_f1549 = pop_f,
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
    g_whoregion, pop_f1549,
    births_best, births_lo, births_hi,
    birthsH0, birthsH0_lo, birthsH0_hi,
    birthsH1, birthsH1_lo, birthsH1_hi
  ) |>
  pivot_longer(cols = -g_whoregion, names_to = "key", values_to = "value") |>
  mutate(value = scales::comma(value)) |>
  pivot_wider(names_from = key, values_from = value)

# looks okay:
# approximately 1.8 billion females aged 15-49 years in 2024
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
  dplyr::mutate(value = formatC(value, format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_regions_py

write.csv(summary_regions_py, here::here("TBburden/outdata", "summary_regions_py_2022.csv"), row.names = FALSE)

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
  mutate(value = formatC(value, format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_regions

write.csv(summary_regions, here::here("TBburden/outdata", "summary_regions_2022.csv"), row.names = FALSE)


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
  mutate(value = formatC(value, format = "d", big.mark = ",")) |>
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
  mutate(value = formatC(value, format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_hbc

write.csv(summary_hbc, here::here("TBburden/outdata", "summary_hbc_2022.csv"), row.names = FALSE)


# Adjusting estimates for pregnancy and postpartum TB risk
# IRRs for pregnancy and postpartum TB
IRR <- data.table::fread(here::here("TBrisk/outdata/meta_summary.csv"))
IRR

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
                      levels = c("P", "PH0", "PH1", "PP", "PPH0", "PPH1"),
                      labels = c(
                        "Pregnancy no HIV data",
                        "Pregnancy with HIV data",
                        "Pregnancy HIV data",
                        "Postpartum no HIV data",
                        "Postpartum with HIV data",
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

# merge in the IRRs
new_df_births_adjusted <- cbind(new_df_births, PSA)

new_df_births_adjusted <- new_df_births_adjusted %>%
  mutate(
    across(all_of(P), ~ . * P),
    across(all_of(PH0), ~ . * PH0),
    across(all_of(PH1), ~ . * PH1),
    across(all_of(PP), ~ . * PP),
    across(all_of(PPH0), ~ . * PPH0),
    across(all_of(PPH1), ~ . * PPH1)
  )

summary_regions_adjusted <- new_df_births_adjusted %>%
  dplyr::group_by(g_whoregion) %>%
  summarise_at(vars(all_of(key_parms)), ~ sum(.x, na.rm = TRUE)) %>%
  janitor::adorn_totals("row")

## add hi/lo
summary_regions_adjusted <- summary_regions_adjusted |>
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
  mutate(value = formatC(value, format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

write.csv(summary_regions_adjusted, here::here("TBburden/outdata", "summary_regions_adjusted.csv"), row.names = FALSE)

summary_regions_byagegroup_adjusted <- new_df_births_adjusted %>%
  group_by(g_whoregion, age_group) %>%
  summarise_at(vars(all_of(key_parms)), ~ sum(.x, na.rm = TRUE))

## add hi/lo
summary_regions_byagegroup_adjusted <- summary_regions_byagegroup_adjusted |>
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
  mutate(value = formatC(value, format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_regions_byagegroup_adjusted

write.csv(summary_regions_byagegroup_adjusted, here::here(
  "TBburden/outdata",
  "summary_regions_byagegroup_adjusted.csv"
), row.names = FALSE)

# The 30 TB high burden countries
summary_hbc_adjusted <- new_df_births %>%
  filter(country %in% hbc) %>%
  group_by(country) %>%
  summarise_at(vars(all_of(key_parms)), ~ sum(.x, na.rm = TRUE)) %>%
  janitor::adorn_totals("row")

## add hi/lo
summary_hbc_adjusted <- summary_hbc_adjusted |>
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
  mutate(value = formatC(value, format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

summary_hbc_adjusted

write.csv(summary_hbc_adjusted, here::here("TBburden/outdata", "summary_hbc_adjusted.csv"), row.names = FALSE)

# country level estimates
summary_countries_adjusted <- new_df_births %>%
  group_by(country) %>%
  summarise_at(vars(all_of(key_parms)), ~ sum(.x, na.rm = TRUE)) %>%
  janitor::adorn_totals("row")

summary_countries_adjusted <- summary_countries_adjusted %>%
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
  mutate(value = formatC(value, format = "d", big.mark = ",")) |>
  pivot_wider(names_from = key, values_from = value)

write.csv(summary_countries_adjusted, here::here("TBburden/outdata", "summary_countries_22_adjusted.csv"), row.names = FALSE)
