
# Abortion rates: Rate per 1000 women of reproductive age (15-44 years)
# Obtained from: https://www.who.int/data/gho/data/indicators/indicator-details/GHO/SRH_ABORTION_RATE
abortions <- read_csv(here::here("TBburden/indata/abortion_rate.csv"))
names(abortions)
head(abortions)

head(births_2022)
abortions <- abortions %>% 
  filter(Dim1 == "95% uncertainty") %>%
  select(country = Location, g_whoregion = ParentLocationCode, 
         abortion_mean = FactValueNumeric, 
         abortion_lo = FactValueNumericLow, abortion_hi = FactValueNumericHigh) %>%
  arrange(country) 

head(abortions)

length(unique(abortions$country))
length(unique(births_2022$country))

setdiff(abortions$country, births_2022$country)
setdiff(births_2022$country, abortions$country)

abortions <- abortions %>%
  mutate(country = countrycode(country,
                               origin = "country.name",
                               destination = "country.name"
  ))

setdiff(abortions$country, births_2022$country)
setdiff(births_2022$country, abortions$country)

# Stillbirths: Number of stillbirths 
# Obtained from: https://data.unicef.org/topic/child-survival/stillbirths/#data
stillbirths <- read_csv(here::here("TBburden/indata/stillbirths.csv"))
names(stillbirths)

table(stillbirths$ISO.Code, useNA = 'always')
stillbirths <- stillbirths %>%
  filter(!is.na(ISO.Code)) %>%
  mutate(Country.Name = ifelse(ISO.Code == "TUR", 'Turkey', Country.Name)) %>%
  select(country = Country.Name, metric = `Uncertainty.Bounds*`, value = `2021.5`) %>%
  filter(!is.na(value)) %>%
  mutate(metric = case_when(
    metric == "Median" ~ "stillbirths_mean",
    metric == "Lower" ~ "stillbirths_lo",
    metric == "Upper" ~ "stillbirths_hi",
    TRUE ~ NA_character_
  ),
  value = as.numeric(value)) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  arrange(country)

setdiff(stillbirths$country, births_2022$country)
setdiff(births_2022$country, stillbirths$country)

stillbirths <- stillbirths %>%
  mutate(country = countrycode(country,
                               origin = "country.name",
                               destination = "country.name"
  ))

setdiff(stillbirths$country, births_2022$country)
setdiff(births_2022$country, stillbirths$country)
length(unique(stillbirths$country))
length(unique(births_2022$country))

tail(stillbirths)
tail(abortions)

num_births <- num_births %>%
  data.frame()

class(num_births)

# merge with births
tot_births <- num_births %>%
  dplyr::ungroup() %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(tot.births = sum(births_best, na.rm = TRUE))

num_births <- num_births %>%
  ungroup() %>%
  left_join(tot_births, by = "country") %>%
  left_join(abortions |> select(-g_whoregion), by = "country") %>%
  left_join(stillbirths, by = "country") 

summary(num_births)

num_births |> 
  group_by(g_whoregion) |>
  summarise_at(vars(contains("abortion")), ~mean(., na.rm = TRUE)) 

num_births |> 
  group_by(g_whoregion) |>
  summarise_at(vars(contains("stillbirths")), ~mean(., na.rm = TRUE)) 

num_births |> 
  select(country, abortion_mean, stillbirths_mean) |>
  distinct() |>
  summarise(abortion = sum(is.na(abortion_mean)),
            stillbirths = sum(is.na(stillbirths_mean)))

# replace missing abortins & stillbirths with mean by region
str(num_births)
num_births <- num_births %>%
  mutate(no_sb = ifelse(is.na(stillbirths_mean), 1, 0),
         no_ab = ifelse(is.na(abortion_mean), 1, 0)) %>%
  group_by(g_whoregion) %>%
  mutate(
    abortion_mean = ifelse(is.na(abortion_mean), median(abortion_mean, na.rm = TRUE), abortion_mean),
    abortion_lo = ifelse(is.na(abortion_lo), median(abortion_lo, na.rm = TRUE), abortion_lo),
    abortion_hi = ifelse(is.na(abortion_hi), median(abortion_hi, na.rm = TRUE), abortion_hi),
    stillbirths_mean = ifelse(is.na(stillbirths_mean), median(stillbirths_mean, na.rm = TRUE), stillbirths_mean),
    stillbirths_lo = ifelse(is.na(stillbirths_lo), median(stillbirths_lo, na.rm = TRUE), stillbirths_lo),
    stillbirths_hi = ifelse(is.na(stillbirths_hi), median(stillbirths_hi, na.rm = TRUE), stillbirths_hi)
  ) %>%
  ungroup()

names(num_births)
num_births <- num_births %>%
  mutate_at(vars(contains("abortion")), ~(births_best/tot.births) *(.)) %>%
  mutate_at(vars(contains("stillbirths")), ~(births_best/tot.births) *(.)) %>%
  mutate_at(vars(contains("abortion")), ~pop_f * as.numeric(.)/1000) %>%
  select(-tot.births)

summary(num_births)

num_births <- num_births %>%
  mutate(births_best = births_best + stillbirths_mean + abortion_mean,
         births_lo = sqrt(births_lo^2 + stillbirths_hi^2 + abortion_hi^2),
         births_hi = sqrt(births_hi^2 + stillbirths_lo^2 + abortion_lo^2)) %>%
  mutate(stillbirths = stillbirths_mean/births_best*100,
         abortion = abortion_mean/births_best*100) 

num_births |> 
  select(stillbirths_mean, stillbirths_lo, stillbirths_hi, stillbirths) |> 
  summary()
num_births |> 
  filter(no_sb != 1) |>
  select(stillbirths_mean, stillbirths_lo, stillbirths_hi, stillbirths) |> 
  summary()
num_births |> 
  select(abortion_mean, abortion) |> 
  summary()
num_births |> 
  filter(no_ab != 1) |>
  select(abortion_mean, abortion) |> 
  summary()

num_births <- num_births %>% 
  select(-contains('stillbirths'), -contains('abortion'), -contains('no_'))


options(scipen = 999)
summary(num_births)
