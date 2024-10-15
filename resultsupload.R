library(here::here)
library(data.table)
library(glue)
library(googlesheets4)


## NOTE new sheet
## create an ID to access the googlesheets results sheet
yourl <- "https://docs.google.com/spreadsheets/d/1FQl-kRmsc6dPE01k2B0UqwLy91wyMbiC808qVcEwzc4/edit?gid=0#gid=0"
shidneat <- as.character(as_sheets_id(yourl))

# resource use table 
summary_regions_2022 <- fread(here::here('TBburden/outdata/summary_regions_2022.csv'))
summary_regions_2022_adjusted <- fread(here::here('TBburden/outdata/summary_regions_2022_adjusted.csv'))
summary_hbc_2022 <- fread(here::here('TBburden/outdata/summary_hbc_2022.csv'))
summary_hbc_2022_adjusted <- fread(here::here('TBburden/outdata/summary_hbc_2022_adjusted.csv'))

write_sheet(summary_regions_2022,shidneat,sheet = "regional_estimatesRAW")
2
write_sheet(summary_regions_2022_adjusted,shidneat,sheet = "adjusted_regional_estimatesRAW")
write_sheet(summary_hbc_2022,shidneat,sheet = "hbc_estimatesRAW")
write_sheet(summary_hbc_2022_adjusted,shidneat,sheet = "adjusted_hbc_estimatesRAW")












