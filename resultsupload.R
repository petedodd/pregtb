library(here)
library(data.table)
library(glue)
library(googlesheets4)


## NOTE new sheet
## create an ID to access the googlesheets results sheet
yourl <- "https://docs.google.com/spreadsheets/d/1FQl-kRmsc6dPE01k2B0UqwLy91wyMbiC808qVcEwzc4/edit?gid=0#gid=0"
shidneat <- as.character(as_sheets_id(yourl))

# resource use table 
summary_regions_2022 <- fread(here::here('TBburden/outdata/summary_regions_2022.csv'))
summary_regions_2022_adjusted <- fread(here::here('TBburden/outdata/summary_regions_adjusted.csv'))
summary_hbc_2022 <- fread(here::here('TBburden/outdata/summary_hbc_2022.csv'))
summary_hbc_2022_adjusted <- fread(here::here('TBburden/outdata/summary_hbc_adjusted.csv'))
summary_age_2022 <- fread(here::here('TBburden/outdata/summary_regions_byagegroup_2022.csv'))
summary_age_2022_adjusted <- fread(here::here('TBburden/outdata/summary_regions_byagegroup_adjusted.csv'))
population_2022 <- fread(here::here('TBburden/outdata/summary_regions_population_2022.csv'))
person_time <- fread(here::here('TBburden/outdata/summary_regions_py_2022.csv'))
summary_countries_2022_adjusted <- fread(here::here('TBburden/outdata/summary_countries_22_adjusted.csv'))

write_sheet(summary_regions_2022,shidneat,sheet = "regional_estimatesRAW")
2
write_sheet(summary_regions_2022_adjusted,shidneat,sheet = "adjusted_regional_estimatesRAW")
write_sheet(summary_hbc_2022,shidneat,sheet = "hbc_estimatesRAW")
write_sheet(summary_hbc_2022_adjusted,shidneat,sheet = "adjusted_hbc_estimatesRAW")
write_sheet(summary_age_2022,shidneat,sheet = "agegroup_estimatesRAW")
write_sheet(summary_age_2022_adjusted,shidneat,sheet = "adjusted_agegroup_estimatesRAW")
write_sheet(population_2022,shidneat,sheet = "populationRAW")
write_sheet(person_time,shidneat,sheet = "person_timeRAW")
write_sheet(summary_countries_2022_adjusted,shidneat,sheet = "adjusted_country_estimatesRAW")
