# Pregnancy and TB data

# Aim: construct a single merged dataset at country level

# Sources of data
# http://www.who.int/tb/country/data/download/en/
# https://extranet.who.int/tme/generateCSV.asp?ds=dictionary
# https://population.un.org/wpp/Download/Standard/Population/
# https://population.un.org/wpp/Download/Standard/Fertility/

cat("\014") # clears Console (in RStudio)
sessionInfo() # gives session info, ver of R, packages
rm(list=ls()) #removes work space environment

library(here)
library(readxl)
library(data.table)
library(tidyverse)
library(viridis)
## library(getTBinR)
# library(dplyr)
library(plyr)


# setwd("U:/Documents/GitHub/pregTB")
## here("~/Documents/GitHub/pregtb")

# load ISO country codes
load(here::here("isodict.Rdata"))
code <- read_csv(here::here("indata", "all.csv")) # has more details on codes
code <- code %>% dplyr::rename(country=name, iso3=`alpha-3`) %>% select(country, `country-code`, iso3, region, `sub-region`)

# country names differ from those in WHO data
ISO$country <-as.character(ISO$country)
ISO$country[ISO$country=="Czech Republic"] <- "Czechia"
ISO$country[ISO$country=="Serbia & Montenegro"] <- "Serbia"
ISO$country[ISO$country=="Swaziland"] <- "Eswatini"
ISO$country[ISO$country=="The Former Yugoslav Republic of Macedonia"] <- "North Macedonia" 

SM <- code %>% filter(country %in% c("Serbia", "Montenegro")) %>% select(country, iso3, `sub-region`) 
names(SM)[3]<-"g_whoregion"
SM$g_whoregion[SM$g_whoregion=="Southern Europe"] <- "EUR"
# read in WHO TB data disaggregated by age and sex
# read in the data dictionary 
# Estimated number of incident cases (all forms) - Worked it out from the main dataset???
if(!file.exists(here::here('indata/df.Rdata'))){
  df<- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=estimates_age_sex", header = T)
  save(df,file=here::here('indata/df.Rdata'))
} else {
  load(here::here('indata/df.Rdata'))
}

if(!file.exists(here::here('indata/df_dic.Rdata'))){
  df_dic<-read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=dictionary")  
  save(df_dic,file=here::here('indata/df_dic.Rdata'))
} else {
  load(here::here('indata/df_dic.Rdata'))
}




# Filter females of reproductive age group
df_ISO <- ISO %>% select(country, iso3, g_whoregion) 
df_ISO <- rbind(df_ISO,SM)
df_iso <- df_ISO %>% left_join(df, by=c("country", "iso3"))
df_1 <- df_iso %>% filter(sex=="f", age_group %in% c("15-24", "25-34", "35-44", "45-54")) 


regional_prop <- df_1 %>% dplyr::group_by(g_whoregion, age_group) %>% 
  dplyr::summarise(sum_best_age=sum(best, na.rm = T), sum_lo_age=sum(lo, na.rm = T), sum_hi_age=sum(hi, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(g_whoregion) %>% dplyr::mutate(sum_best_over=sum(sum_best_age, na.rm = T), sum_lo_over=sum(sum_lo_age, na.rm = T), 
                                                 sum_hi_over=sum(sum_hi_age, na.rm = T))%>% 
  # dplyr::ungroup() %>% dplyr::group_by(g_whoregion, age_group) %>% 
  dplyr::mutate(prop_best = sum_best_age/sum_best_over, prop_lo = sum_lo_age/sum_lo_over, prop_hi = sum_hi_age/sum_hi_over)

df_2 <- df_iso %>% dplyr::filter(!country %in% df_1$country) %>% dplyr::filter(sex=="f", age_group=="15plus")

temp<- cbind(data.frame(country=rep(df_2$country, 4), age_group=c("15-24", "25-34", "35-44", "45-54")))
df_2 <- df_2 %>% select(-age_group) %>% left_join(temp, by="country")
# df_2 <- ISO %>% select(country, iso3, g_whoregion) %>% left_join(df_1, by=c("country", "iso3"))
df_2 <- regional_prop %>% select(g_whoregion, age_group, prop_best, prop_lo, prop_hi) %>% left_join(df_2, by=c("g_whoregion", "age_group"))
df_2 <- df_2 %>% mutate(best=best*prop_best, lo=lo*prop_lo, hi=hi*prop_hi) %>% select(-c("prop_best", "prop_lo", "prop_hi"))

df_3 <- df_2 %>% select(country, iso3, g_whoregion, iso2, iso_numeric, year,measure, unit, age_group, sex, best, lo, hi) 

df_3 <- as.data.frame(df_3)
df_3 <- rbind(df_1, df_3)

length(unique(df_3$country))

# Crude birth rate and births 
# Crude birth rate (births per 1,000 population)								
cbr_average<- read_csv(here::here("indata/WPP2019_Period_Indicators_Medium.csv"))  %>% select(Location, Variant, Time, CBR)
cbr_lohi<- read_csv(here::here("indata/WPP2019_Period_Indicators_OtherVariants.csv")) %>% select(Location, Variant, Time, CBR)

cbr <- rbind(cbr_average, cbr_lohi)

for (k in 1:length(names(cbr))){ #convert all the rows with type 'integer' to 'numeric
  if (is.integer(cbr[,k])){
    cbr[,k] <- as.numeric(cbr[,k])	
  }
}


# names(cbr)[1] <- "country"
cbr <- cbr %>% dplyr::rename(country=Location, year=Time)
# births <- births %>% select(-c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")) %>% 
#   gather(age_group, estimate, 7:10)

cbr <- separate(cbr, year, c("from", "to"))
cbr$to <- as.numeric(cbr$to)-1
cbr$year <- mapply(seq,cbr$from,cbr$to,SIMPLIFY=FALSE)

cbr <- cbr %>% 
  unnest(year) %>% 
  select(-from,-to) %>% 
  filter(Variant %in% c("Medium", "High", "Low")) 

cbr_wide <- cbr %>% 
  dplyr::group_by_at(vars(-CBR)) %>%  # group by everything other than the value column. 
  dplyr::mutate(row_id=1:dplyr::n()) %>% ungroup() %>%  # build group index
  tidyr::spread(key=Variant, value=CBR) %>%    # spread
  select(-row_id)  # drop the index

# cbr_wide <- cbr %>%
#   group_by_at(vars(-CBR, -year)) %>%  # group by everything other than the value column.
#   dplyr::mutate(row_id=1:dplyr::n()) %>% ungroup() %>%  # build group index
#   nest(CBR, .key = 'value_col') %>%
#   spread(key=Variant, value=value_col) %>%
#   unnest(High, Low, Medium, .sep = '_') %>% # spread
#   select(-row_id)  # drop the index


# cbr_wide <- cbr_wide %>% dplyr::rename(CBR_lo=Low, CBR_hi=High, CBR_best=Medium)
# some country names need sorting out
cbr_wide <- cbr_wide %>% filter(!country %in%
                              c("WORLD", "More developed regions",
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
                                "Africa",
                                "Eastern Africa",
                                "Middle Africa",
                                "Northern Africa",
                                "Western Sahara",
                                "Southern Africa",
                                "Western Africa",
                                "ASIA",
                                "Asia",
                                "Eastern Asia",
                                "South-Central Asia",
                                "Central Asia",
                                "Southern Asia",
                                "South-Eastern Asia",
                                "Western Asia",
                                "EUROPE",
                                "Europe",
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
                                "Polynesia"))



# cbr_wide <- cbr_wide %>% str_replace(country, "[\xe9]", "e")

cbr_wide <- cbr_wide %>%
  mutate(country = str_replace(country, "[\xe9]", "e"))

# cbr_wide$country[grep("Barthélemy", cbr_wide$country) ]
# cbr_wide$country[cbr_wide$country=="Saint-Barth�lemy"] <- "Republique du CÃ´te d'Ivoire"

# Changing some country names to match the WHO TB data and ISO codes #
cbr_wide$country <- (mapvalues((cbr_wide$country), from = c(
  "United Kingdom",
  "Dem. People's Republic of Korea",
  "Micronesia (Fed. States of)",
  "State of Palestine",
  "North Macedonia",
  
  "Cete d'Ivoire",
  "Curaeao",
  "Reunion",
  "Saint-Barthelemy",
  "Bonaire, Sint Eustatius and Saba")
  
  , to = c(
    "United Kingdom of Great Britain and Northern Ireland",
    "Democratic People's Republic of Korea",
    "Micronesia (Federated States of)",
    "West Bank and Gaza Strip",
    "North Macedonia",
   
    "Côte d'Ivoire",
  "Curaçao",
  "Réunion",
  "Saint Barthélemy",
  "Bonaire, Saint Eustatius and Saba")
))
## TODO check CIV

# # add ISO country codes to births
# load(here::here("isodict.Rdata"))
# code <- read_csv(here::here("indata/all.csv")) # has more details on codes
# code <- code %>% dplyr::rename(country=name, iso3=`alpha-3`) %>% select(country, iso3, region, `sub-region`)

# # code$country[grep("Wallis", code$country)]
# code$country[code$country=="Tanzania, United Republic of"]<- "United Republic of Tanzania"
# code$country[code$country=="Hong Kong"]<- "China, Hong Kong SAR"
# code$country[code$country=="Korea (Democratic People's Republic of)"]<- "Democratic People's Republic of Korea"
# code$country[code$country=="Congo, Democratic Republic of the"]<- "Democratic Republic of the Congo"
# code$country[code$country=="Korea, Republic of"]<- "Republic of Korea"
# code$country[code$country=="Moldova, Republic of"]<- "Republic of Moldova"
# code$country[code$country=="Bonaire, Sint Eustatius and Saba"]<- "Bonaire, Saint Eustatius and Saba"
# code$country[code$country=="Virgin Islands (British)"]<- "British Virgin Islands"
# code$country[code$country=="Macao"]<- "China, Macao SAR"
# code$country[code$country=="Wallis and Futuna"]<- "Wallis and Futuna Islands"
# code$country[code$country=="Palestine, State of"]<- "West Bank and Gaza Strip"

# Final dataset for crude birth rates
cbr_wider <- cbr_wide %>% left_join(df_ISO, by = "country") %>% filter(!is.na(iso3))
length(unique(cbr_wider$country))

# missing <- df_3 %>% filter(!country %in% cbr_wider$country)

# read in total Population interpolated by single age, annually from 1950 to 2100. 
# read in population of females in the reproductive age group
# Annual female population by five-year age group, region, subregion and country, 1950-2100 (thousands)								

fn <- here::here('indata/popn_all.Rdata')
if(!file.exists(fn)){
  popn_all <- read_csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv")
  save(popn_all,file=fn)
} else {
  load(fn)
}

pop_f <- read_excel(here::here("indata/pop_med.xlsx"))
pop_f$"15-24" <- rowSums(pop_f[c("15-19", "20-24")], na.rm=TRUE)
pop_f$"25-34" <- rowSums(pop_f[c("25-29", "30-34")], na.rm=TRUE)
pop_f$"35-44" <- rowSums(pop_f[c("35-39", "40-44")], na.rm=TRUE)
pop_f$"45-54" <- rowSums(pop_f[c("45-49", "50-54")], na.rm=TRUE)

pop_f <- pop_f %>% dplyr::rename(pop_f=Variant, country=`Region, subregion, country or area *`, year=`Reference date (as of 1 July)`)  %>%
  select(c("country","pop_f", "year", "15-24", "25-34", "35-44", "45-54")) %>% gather(age_group, pop_f, c("15-24", "25-34", "35-44", "45-54"))
pop_f$pop_f <- pop_f$pop_f*1000

pop_all <- popn_all %>% dplyr::rename(country=Location, year=Time) %>% select(country, year, Variant, PopTotal)%>% 
  filter(Variant %in% c("Medium", "High", "Low")) 
                                 
tpop_wide <- pop_all %>% 
  group_by_at(vars( -year)) %>%  # group by everything other than the value column. 
  dplyr::mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=Variant, value=PopTotal) %>%    # spread
  select(-row_id)  # drop the index


tpop_wide <- tpop_wide %>% dplyr::rename(tpop_lo=Low, tpop_hi=High, tpop_best=Medium)

pop_f <- pop_f %>% filter(!country %in%
                            c("WORLD", "More developed regions",
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
                              "Polynesia"))

pop_f$country[grep("Mayotte", pop_f$country) ]
df_ISO$country[grep("Reunion", df_ISO$country) ]
# Changing some country names to match the WHO TB data and ISO codes #
pop_f$country <- (mapvalues((pop_f$country), from = c(
  "United Kingdom",
  "Dem. People's Republic of Korea",
  "Micronesia (Fed. States of)",
  "State of Palestine",
  "TFYR Macedonia",
  "Czech Republic",
  "Swaziland",
  "United States Virgin Islands")
  
  , to = c(
    "United Kingdom of Great Britain and Northern Ireland",
    "Democratic People's Republic of Korea",
    "Micronesia (Federated States of)",
    "West Bank and Gaza Strip",
    "North Macedonia",
    "Czechia",
    "Eswatini",
    "US Virgin Islands")
))

tpop_wide <- tpop_wide %>% filter(!country %in%
                            c("WORLD", "More developed regions",
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
                              "Polynesia"))

# Changing some country names to match the WHO TB data and ISO codes #
tpop_wide$country <- (mapvalues((tpop_wide$country), from = c(
  "United Kingdom",
  "Dem. People's Republic of Korea",
  "Micronesia (Fed. States of)",
  "State of Palestine",
  # "TFYR Macedonia",
  "Czechia")
  
  , to = c(
    "United Kingdom of Great Britain and Northern Ireland",
    "Democratic People's Republic of Korea",
    "Micronesia (Federated States of)",
    "West Bank and Gaza Strip",
    # "North Macedonia",
    "Czech Republic")
))


pop_f <- pop_f %>% left_join(df_ISO, by = "country") %>% select(country,iso3, age_group, year, pop_f, g_whoregion)

tpop_wider <- tpop_wide %>% left_join(df_ISO, by = "country") %>% dplyr::filter(!is.na("iso3"))

pop_f2017 <- pop_f %>% filter(year==2017) %>% select(-year)
length(unique(pop_f2017$country))
length(unique(tpop_wider$country))
# pop_all2017 <- pop_all %>% filter(Time==2017) %>% select(-Time) 

# tpop_wider$g_whoregion <-tpop_wider$`sub-region`
# cbr_wider$g_whoregion <-cbr_wider$`sub-region`

num_cbrbirths <- cbr_wider  %>% left_join(tpop_wider, by=c("country", "year", "iso3", "g_whoregion")) %>%
  mutate(cbrbirths_lo = Low*tpop_best,
         cbrbirths_best = Medium*tpop_best,
         cbrbirths_hi = High*tpop_best)

# num_births$value <- num_births$pop_f * (num_births$value/1000) #  absolute number of births 
# num_cbrbirths <- num_cbrbirths %>% spread(metric, value) %>% select(-country)

# filter 2017 data to match the WHO TB data 
# cbr_2017 <- cbr_wide %>% filter(year==2017)
num_cbrbirths2017 <- num_cbrbirths %>% filter(year==2017)

## TODO: PJD has error here: Error: `by` can't contain join column `country`, `iso3` which is missing from LHS
new_df_pop <- pop_f2017 %>%
  dplyr::group_by(country, iso3) %>%
  dplyr::summarise(tpopf=sum(pop_f, na.rm = T)) %>%
  left_join(num_cbrbirths2017, by=c("country","iso3"))
# combining the different datasets - births, population of females in the reproductive age and TB incident case in women in the reproductive age group
new_df_cbrbirths <- df_1 %>% group_by(country, iso3) %>%   summarise_at(
  .vars= vars( best, lo, hi), 
  .funs =  sum) %>% left_join(new_df_pop, by=c("country", "iso3")) %>%
  dplyr::rename(TBI_best=best, TBI_lo=lo, TBI_hi=hi)

# new_df_cbrbirths$g_whoregion[is.na(new_df_cbrbirths$g_whoregion)] <- "AFR" # problem with Swaziland

# df_2 <- ISO %>% select(-iso2, -iso3, -iso_numeric) %>% left_join(df, by="country")

# Estimate the incidence of TB in pregnancy
new_df_cbrbirths$pregTBI_best <- new_df_cbrbirths$cbrbirths_best * 9/12 * (new_df_cbrbirths$TBI_best/new_df_cbrbirths$tpopf)
new_df_cbrbirths$pregTBI_lo   <- new_df_cbrbirths$cbrbirths_lo   * 9/12 * (new_df_cbrbirths$TBI_lo/new_df_cbrbirths$tpopf)
new_df_cbrbirths$pregTBI_hi   <- new_df_cbrbirths$cbrbirths_hi   * 9/12 * (new_df_cbrbirths$TBI_hi/new_df_cbrbirths$tpopf)

# Estimate the incidence of TB in post-partum period
new_df_cbrbirths$ppTBI_best <- new_df_cbrbirths$cbrbirths_best * 3/12 * (new_df_cbrbirths$TBI_best/new_df_cbrbirths$tpopf)
new_df_cbrbirths$ppTBI_lo   <- new_df_cbrbirths$cbrbirths_lo   * 3/12 * (new_df_cbrbirths$TBI_lo/new_df_cbrbirths$tpopf)
new_df_cbrbirths$ppTBI_hi   <- new_df_cbrbirths$cbrbirths_hi   * 3/12 * (new_df_cbrbirths$TBI_hi/new_df_cbrbirths$tpopf)

pregTB_births <- new_df_cbrbirths %>% dplyr::group_by(country) %>% summarise(pregTBI_best=sum(pregTBI_best), pregTBI_lo=sum(pregTBI_lo), pregTBI_hi=sum(pregTBI_hi))

ipregTB_births <- new_df_cbrbirths %>% dplyr::group_by(country, iso3) %>% summarise(ipregTBI_best=1.3*(pregTBI_best), ipregTBI_lo=1.3*(pregTBI_lo), ipregTBI_hi=1.3*(pregTBI_hi))

key_parms <- c("tpopf", "TBI_best", "TBI_lo", "TBI_hi", "cbrbirths_best", "cbrbirths_lo", "cbrbirths_hi", "pregTBI_best", 
               "pregTBI_lo", "pregTBI_hi", "ppTBI_best", 
               "ppTBI_lo", "ppTBI_hi")

summary_regions2 <- new_df_cbrbirths%>%group_by(g_whoregion)%>%summarise_at(key_parms, funs(sum), na.rm=T)%>% adorn_totals("row")

summary_SEA <- new_df_cbrbirths%>%filter(g_whoregion=="SEA")%>%group_by(country)%>%
  summarise_at(key_parms, funs(sum), na.rm=T)

## combining uncertainty notes:
## Q: I work out Q.sd = (Q.hi-Q.lo)/(2*1.96)
## C = A + B
## C.sd^2 = A.sd^2 + B.sd^2
## A = B*C
## log(A) = log(B) + log(C)
## differentiate:  dA / A = dB/B + dC/C
## (A.sd/A)^2 = (B.sd/B)^2 + (C.sd/C)^2

## https://www.equator-network.org/reporting-guidelines/gather-statement/
