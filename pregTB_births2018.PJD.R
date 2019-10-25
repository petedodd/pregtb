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

library(readxl)
# library(data.table)
library(tidyverse)
library(viridis)
library(here)
library(dplyr)
library(plyr)
library(janitor)


## setwd("U:/Documents/GitHub/pregTB")
# setwd("~/Documents/GitHub/pregtb")

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
  df <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=estimates_age_sex", header = T)
  save(df,file=here::here('indata/df.Rdata'))
} else {
  load(here::here('indata/df.Rdata'))
}



if(!file.exists(here::here('indata/df_dic.Rdata'))){
  df_dic <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=dictionary")  
  save(df_dic,file=here::here('indata/df_dic.Rdata'))
} else {
  load(here::here('indata/df_dic.Rdata'))
}




# Filter females of reproductive age group
df_ISO <- ISO %>% select(country, iso3, g_whoregion) 
df_ISO <- rbind(df_ISO,SM)
df_iso <- df_ISO %>% left_join(df, by=c("country", "iso3"))
df_1 <- df_iso %>% filter(sex=="f", age_group %in% c("15-24", "25-34", "35-44", "45-54")) 
df_1$W <- df_1$hi - df_1$lo

## PJD: proportion of TB within each age group in region??
regional_prop <- df_1 %>%
  dplyr::group_by(g_whoregion, age_group) %>%
  dplyr::summarise(sum_best_age=sum(best, na.rm = T),
                   sum_W2 = sum(W^2,na.rm=TRUE)
                   ## sum_lo_age=sum(lo, na.rm = T),
                   ## sum_hi_age=sum(hi, na.rm = T)
                   ) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(g_whoregion) %>%
  dplyr::mutate(sum_best_over =sum(sum_best_age, na.rm = T),
                sum_W2_over = sum(sum_W2,na.rm = TRUE)
                ## sum_lo_over=sum(sum_lo_age, na.rm = T),
                ## sum_hi_over=sum(sum_hi_age, na.rm = T)
                )%>% 
  # dplyr::ungroup() %>% dplyr::group_by(g_whoregion, age_group) %>% 
  dplyr::mutate(prop_best = sum_best_age/sum_best_over ##,
                ## prop_lo = sum_lo_age/sum_lo_over,
                ## prop_hi = sum_hi_age/sum_hi_over
                )
## PJD unsure here

regional_prop$propW <- with(regional_prop,
{prop_best *sqrt(sum_W2_over/sum_best_over^2 + sum_W2/sum_best_age^2)})

regional_prop$prop_lo <- regional_prop$prop_best - regional_prop$propW/2
regional_prop$prop_hi <- regional_prop$prop_best + regional_prop$propW/2

df_2 <- df_iso %>% dplyr::filter(!country %in% df_1$country) %>%
  dplyr::filter(sex=="f", age_group=="15plus")

temp <- cbind(data.frame(country=rep(df_2$country, 4),
                         age_group=c("15-24", "25-34", "35-44", "45-54")))
df_2 <- df_2 %>% select(-age_group) %>% left_join(temp, by="country")
# df_2 <- ISO %>% select(country, iso3, g_whoregion) %>% left_join(df_1, by=c("country", "iso3"))
df_2 <- regional_prop %>% select(g_whoregion, age_group, prop_best, prop_lo, prop_hi) %>%
  left_join(df_2, by=c("g_whoregion", "age_group"))
df_2 <- df_2 %>% mutate(best=best*prop_best, lo=lo*prop_lo, hi=hi*prop_hi) %>%
  select(-c("prop_best", "prop_lo", "prop_hi"))


df_3 <- df_2 %>% select(country, iso3, g_whoregion, iso2, iso_numeric, year,
                        measure, unit, age_group, sex, best, lo, hi)
df_3 <- as.data.frame(df_3)
df_1$W <- NULL
df_3 <- rbind(df_1, df_3)

length(unique(df_3$country))

# Births by age of mother
# Births by five-year age group of mother, region, subregion and country, 1950-2100 (thousands)								
births_average <- read_excel(here::here("indata", "births_med.xlsx"))
births_lo<- read_excel(here::here("indata", "births_lo.xlsx"))
births_hi<- read_excel(here::here("indata", "births_hi.xlsx"))


births <- rbind(births_average, births_lo, births_hi)

for (k in 1:length(names(births))){ #convert all the rows with type 'integer' to 'numeric
  if (is.integer(births[,k])){
    births[,k] <- as.numeric(births[,k])	
  }
}
# recategorizing age groups to match the WHO TB data
# births$`15plus` <- rowSums(births[7:13], na.rm = TRUE)
births$"15-24"  <- rowSums(births[c("15-19", "20-24")], na.rm=TRUE)
births$"25-34"  <- rowSums(births[c("25-29", "30-34")], na.rm=TRUE)
births$"35-44"  <- rowSums(births[c("35-39", "40-44")], na.rm=TRUE)
births$"45-54"  <- births$`45-49`

names(births)[3] <- "country"
births <- births %>%
  select(-c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")) %>%
  gather(age_group, estimate, 7:10)

births <- separate(births, Period, c("from", "to"))
births$to <- as.numeric(births$to)-1
births$year <- mapply(seq,births$from,births$to,SIMPLIFY=FALSE)
births$estimate <- (births$estimate*1000)/5

births <- births %>%
  unnest(year) %>% 
  select(-from,-to) %>% 
  spread(Variant, estimate)

births <- births %>%
  dplyr::rename(births_lo=`Low variant`, births_hi=`High variant`, births_best=`Medium variant`)

births_1 <- births %>% filter(!country %in%
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

# b<- df_ISO %>% filter(!country %in% births_2$country)
# births_1$country[grep("Czechia", births_1$country) ]
# df_ISO$country[grep("Greenland", df_ISO$country) ]
# Changing some country names to match the WHO TB data and ISO codes #
births_1$country <- (mapvalues((births_1$country), from = c(
  "United Kingdom",
  "Dem. People's Republic of Korea",
  "Micronesia (Fed. States of)",
  "State of Palestine",
  "TFYR Macedonia",
  "Swaziland",
  # "Czech Republic", 
  "Samoa",
  "United States Virgin Islands")
  
  , to = c(
    "United Kingdom of Great Britain and Northern Ireland",
    "Democratic People's Republic of Korea",
    "Micronesia (Federated States of)",
    "West Bank and Gaza Strip",
    "North Macedonia",
    "Eswatini",
    # "Czechia",
    "American Samoa",
    "US Virgin Islands" )
))


births_2 <- births_1 %>% select(-c("Notes", `Country code`)) %>%
  left_join(df_ISO, by = "country")
# births2<- births %>% select(-c("Notes")) %>% left_join(code, by = "country", `Country code`)%>% filter(!is.na(iso3))
length(unique(births_2$country))
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
# filter 2017 data to match the WHO TB data 
births_2017 <- births_2 %>% filter(year==2018) %>%
  select(country,iso3, g_whoregion, age_group, births_best, births_lo, births_hi)
length(unique(births_2017$country))
# read in population of females in the reproductive age group
# Annual female population by five-year age group, region, subregion and country, 1950-2100 (thousands)								
pop_f <- read_excel(here::here("indata","pop_med.xlsx"))

# pop_f$`15plus` <- rowSums(pop_f[10:27], na.rm = TRUE)
pop_f$"15-24"  <- rowSums(pop_f[c("15-19", "20-24")], na.rm=TRUE)
pop_f$"25-34"  <- rowSums(pop_f[c("25-29", "30-34")], na.rm=TRUE)
pop_f$"35-44"  <- rowSums(pop_f[c("35-39", "40-44")], na.rm=TRUE)
pop_f$"45-54"  <- rowSums(pop_f[c("45-49", "50-54")], na.rm=TRUE)



pop_f <- pop_f %>% dplyr::rename(pop_f=Variant,
                                 country=`Region, subregion, country or area *`,
                                 year=`Reference date (as of 1 July)`)  %>%
  select(c("country","pop_f", "year", "15-24", "25-34", "35-44", "45-54")) %>%
  gather(age_group, pop_f, c("15-24", "25-34", "35-44", "45-54"))
pop_f$pop_f <- pop_f$pop_f*1000

# pop_f1 <- pop_f %>% filter(age_group %in% c("15-24", "25-34", "35-44", "45-54")) #%>% filter(country %in% df_1$country)
# pop_f2 <- pop_f %>% filter(country %in% df_2$country) %>% filter(age_group=="15plus")

# pop_f3 <- rbind(pop_f1, pop_f2)
pop_f1 <- pop_f %>% filter(!country %in%
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

length(unique(pop_f1$country))

# b<- df_ISO %>% filter(!country %in% pop_f1$country)
# pop_f1$country[grep("Wallis", pop_f1$country) ]
# df_ISO$country[grep("Greenland", df_ISO$country) ]
# Changing some country names to match the WHO TB data and ISO codes #
pop_f1$country <- (mapvalues((pop_f1$country), from = c(
  "United Kingdom",
  "Dem. People's Republic of Korea",
  "Micronesia (Fed. States of)",
  "State of Palestine",
  "TFYR Macedonia",
  "Czechia",
  "Samoa",
  "Swaziland",
  "United States Virgin Islands")
  
  , to = c(
    "United Kingdom of Great Britain and Northern Ireland",
    "Democratic People's Republic of Korea",
    "Micronesia (Federated States of)",
    "West Bank and Gaza Strip",
    "North Macedonia",
    "Czech Republic",
    "American Samoa",
    "Eswatini",
    "US Virgin Islands")
))

pop_f2 <- pop_f1 %>% left_join(df_ISO, by = "country") %>%
  select(country, iso3, age_group, year, pop_f)
length(unique(pop_f2$country))

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

pop_f2017 <- pop_f2 %>% filter(year==2018) %>% select(-year, -country) 
num_births <- births_2017 %>% left_join(pop_f2017, by=c("iso3", "age_group")) %>%
  select(-country)
# %>% 
  # gather(metric, value, c("births_best", "births_lo", "births_hi"))
# num_births$value <- num_births$pop_f * (num_births$value/1000) #  absolute number of births 
# num_births <- num_births %>% spread(metric, value) %>% select(-country)

# combining the different datasets - births, population of females in the reproductive age and TB incident case in women in the reproductive age group
new_df_births <- df_3 %>% select(country,iso3, iso_numeric, year, age_group, best, lo, hi) %>%
  left_join(num_births, by=c("iso3", "age_group")) %>%
  dplyr::rename(TBI_best=best, TBI_lo=lo, TBI_hi=hi) %>% filter(!is.na(g_whoregion))
length(unique(new_df_births$country))
# new_df_births$g_whoregion[is.na(new_df_births$g_whoregion)] <- "AFR" # problem with Swaziland

# df_2 <- ISO %>% select(-iso2, -iso3, -iso_numeric) %>% left_join(df, by="country")

# Estimate the incidence of TB in pregnancy
# new_df_births$pregTBI_best <- new_df_births$births_best * 280/365 * (new_df_births$TBI_best/new_df_births$pop_f)
# new_df_births$pregTBI_lo   <- new_df_births$births_lo   * 280/365 * (new_df_births$TBI_lo/new_df_births$pop_f)
# new_df_births$pregTBI_hi   <- new_df_births$births_hi   * 280/365 * (new_df_births$TBI_hi/new_df_births$pop_f)
# 
# # Estimate the incidence of TB in post-partum period
# new_df_births$ppTBI_best <- new_df_births$births_best * 3/12 * (new_df_births$TBI_best/new_df_births$pop_f)
# # new_df_births$ppTBI_lo   <- new_df_births$births_lo   * 3/12 * (new_df_births$TBI_lo/new_df_births$pop_f)
# # new_df_births$ppTBI_hi   <- new_df_births$births_hi   * 3/12 * (new_df_births$TBI_hi/new_df_births$pop_f)
# new_df_births$ppTBI_lo   <- new_df_births$births_best * 3/12 * (new_df_births$TBI_best/new_df_births$pop_f)
# new_df_births$ppTBI_hi   <- new_df_births$births_best * 3/12 * (new_df_births$TBI_best/new_df_births$pop_f)

# new_df_births$pregTBI_best_sd <- sqrt((new_df_births$births_hi - new_df_births$births_best)^2 + (new_df_births$TBI_hi - new_df_births$TBI_best)^2)
# new_df_births$pregTBI_best_sd1 <- sqrt((0.5*(new_df_births$births_hi - new_df_births$births_lo)/sqrt(3))^2 + (0.5*(new_df_births$TBI_hi - new_df_births$TBI_lo)/sqrt(3))^2)
# new_df_births$pregTBI_best_sd2 <- sqrt((0.5*(new_df_births$births_hi - new_df_births$births_lo))^2 + (0.5*(new_df_births$TBI_hi - new_df_births$TBI_lo))^2)
# new_df_births$pregTBI_best_sd3 <- sqrt(((new_df_births$births_hi - new_df_births$births_lo)/sqrt(12))^2 + ((new_df_births$TBI_hi - new_df_births$TBI_lo)/sqrt(12))^2)

## or use the 'width' -- there will be a common factor of 3.92 for all contributors:
new_df_births$birthsWidth <- new_df_births$births_hi - new_df_births$births_lo
new_df_births$birthsWidthSq <- new_df_births$birthsWidth^2 #a multiple of variance
new_df_births$TBIwidth <- new_df_births$TBI_hi - new_df_births$TBI_lo
new_df_births$TBIwidthSq <- new_df_births$TBIwidth^2 #a multiple of variance

## calx
new_df_births$pregTBI_best <- new_df_births$births_best * 280/365 *
  (new_df_births$TBI_best/new_df_births$pop_f)
new_df_births$ppTBI_best <- new_df_births$births_best * 3/12 *
  (new_df_births$TBI_best/new_df_births$pop_f)
## A = B*C
## log(A) = log(B) + log(C)
## differentiate:  dA / A = dB/B + dC/C
## (A.sd/A)^2 = (B.sd/B)^2 + (C.sd/C)^2

new_df_births$pregTBIwidth <- new_df_births$pregTBI_best *
  sqrt((new_df_births$TBIwidth/new_df_births$TBI_best)^2 +
       (new_df_births$birthsWidth/new_df_births$births_best)^2)

new_df_births$ppTBIwidth <- new_df_births$ppTBI_best *
  sqrt((new_df_births$TBIwidth/new_df_births$TBI_best)^2 +
         (new_df_births$birthsWidth/new_df_births$births_best)^2)

## TODO -- PJD
## then hi = best + width/2
## then lo = best - width/2
## paying attention to any -ves
## and:
## to aggregate eg over countries, sum the square of the widths, and the sqrt them
## and then use as above to generate lo/hi around the aggregate best

# new_df_births$pregTBI_lo <- new_df_births$pregTBI_best - new_df_births$pregTBIwidth/2
# new_df_births$pregTBI_hi <- new_df_births$pregTBI_best + new_df_births$pregTBIwidth/2

# calc includes a workaround to remove negative values 
new_df_births$pregTBI_lo <- (new_df_births$pregTBI_best - new_df_births$pregTBIwidth/2)
new_df_births$pregTBI_lo <- ifelse(new_df_births$pregTBI_lo<0,
                                   new_df_births$pregTBI_lo- new_df_births$pregTBI_lo,
                                   new_df_births$pregTBI_lo)
new_df_births$pregTBI_hi <- (new_df_births$pregTBI_best + new_df_births$pregTBIwidth/2)

new_df_births$ppTBI_lo <- (new_df_births$ppTBI_best - new_df_births$ppTBIwidth/2)
new_df_births$ppTBI_lo <- ifelse(new_df_births$ppTBI_lo<0,
                                 new_df_births$ppTBI_lo- new_df_births$ppTBI_lo,
                                   new_df_births$ppTBI_lo )
new_df_births$ppTBI_hi <- (new_df_births$ppTBI_best + new_df_births$ppTBIwidth/2)

## then you
## PJD NOTE you didn't need na.rm=TRUE here...
pregTB_cases <- new_df_births %>%
  group_by(country) %>%
  summarise(pregTBI_best=sum(pregTBI_best,na.rm = TRUE),
            pregTBI_W2 = sum((pregTBI_hi-pregTBI_lo)^2,na.rm=TRUE))
            ## pregTBI_lo=sum(pregTBI_lo),
            ## pregTBI_hi=sum(pregTBI_hi)
            ## )

pregTB_cases$pregTBI_lo <- pregTB_cases$pregTBI_best - sqrt(pregTB_cases$pregTBI_W2)/2
pregTB_cases$pregTBI_hi <- pregTB_cases$pregTBI_best + sqrt(pregTB_cases$pregTBI_W2)/2

ppTB_cases <- new_df_births %>%
  group_by(country) %>%
  summarise(ppTBI_best=sum(ppTBI_best,na.rm=TRUE),
            ppTBI_W2 = sum((pregTBI_hi-pregTBI_lo)^2,na.rm=TRUE))
            ## ppTBI_lo=sum(ppTBI_lo),
            ## ppTBI_hi=sum(ppTBI_hi))

ppTB_cases$ppTBI_lo <- ppTB_cases$ppTBI_best - sqrt(ppTB_cases$ppTBI_W2)/2
ppTB_cases$ppTBI_hi <- ppTB_cases$ppTBI_best + sqrt(ppTB_cases$ppTBI_W2)/2
## ppTB_cases$W <- sqrt(ppTB_cases$ppTBI_W2)


length(unique(new_df_births$country))

# pregTB_births <- new_df_births %>% dplyr::group_by(country) %>% summarise(pregTBI_best=sum(pregTBI_best), pregTBI_lo=sum(pregTBI_lo), pregTBI_hi=sum(pregTBI_hi))
# ipregTB_births <- new_df_births %>% group_by(country,g_whoregion, age_group) %>% summarise(ipregTBI_best=1.3*(pregTBI_best), ipregTBI_lo=1.3*(pregTBI_lo), ipregTBI_hi=1.3*(pregTBI_hi))
# ipregTB_births_summary <- ipregTB_births %>% group_by(g_whoregion, age_group) %>% summarise_at(c("ipregTBI_best", "ipregTBI_lo", "ipregTBI_hi"), funs(sum), na.rm = T)

## NOTE PJD changed as should be summing variances
key_parms <- c("pregTBI_best","pregTBIwidthSq",
               "ppTBI_best", "ppTBIwidthSq")

new_df_births$ppTBIwidthSq <- new_df_births$ppTBIwidth^2
new_df_births$pregTBIwidthSq <- new_df_births$pregTBIwidth^2

summary_regions2 <- new_df_births%>%
  dplyr::group_by(g_whoregion)%>%
  dplyr::summarise_at(key_parms, funs(sum), na.rm=T) %>%
  adorn_totals("row")

## add hi/lo
summary_regions2$pregTBI_lo <- summary_regions2$pregTBI_best -
  sqrt(summary_regions2$pregTBIwidthSq)/2
summary_regions2$pregTBI_hi <- summary_regions2$pregTBI_best +
  sqrt(summary_regions2$pregTBIwidthSq)/2
summary_regions2$ppTBI_lo <- summary_regions2$ppTBI_best -
  sqrt(summary_regions2$ppTBIwidthSq)/2
summary_regions2$ppTBI_hi <- summary_regions2$ppTBI_best +
  sqrt(summary_regions2$ppTBIwidthSq)/2


## summary_regions_byagegroup <- new_df_births%>%
##   group_by(g_whoregion, age_group)%>%
##   summarise_at(key_parms, funs(sum), na.rm=T)

write.csv(summary_regions2,
          here::here( "outdata", "Total number of incident tuberculosis cases in pregnant women.PJD.csv"))

# The 30 TB high burden countries
hbc <- c("Angola", "Bangladesh", "Brazil", "China",
         "Democratic People's Republic of Korea", "Democratic Republic of the Congo",
         "Ethiopia", "India", "Indonesia", "Kenya", "Mozambique", "Myanmar", "Nigeria",
         "Pakistan", "Philippines", "Russian Federation", "South Africa", "Thailand",
         "United Republic of Tanzania", "Viet Nam", "Cambodia",
         "Central African Republic", "Congo", "Lesotho", "Liberia",
         "Namibia", "Papua New Guinea", "Sierra Leone", "Zambia", "Zimbabwe")

summary_hbc <- new_df_births %>%
  filter(country %in% hbc) %>%
  group_by(country)%>%
  summarise_at(key_parms, funs(sum), na.rm=T) %>%
  adorn_totals("row")


## add hi/lo
summary_hbc$pregTBI_lo <- summary_hbc$pregTBI_best -
  sqrt(summary_hbc$pregTBIwidthSq)/2
summary_hbc$pregTBI_hi <- summary_hbc$pregTBI_best +
  sqrt(summary_hbc$pregTBIwidthSq)/2
summary_hbc$ppTBI_lo <- summary_hbc$ppTBI_best -
  sqrt(summary_hbc$ppTBIwidthSq)/2
summary_hbc$ppTBI_hi <- summary_hbc$ppTBI_best +
  sqrt(summary_hbc$ppTBIwidthSq)/2

write.csv(summary_hbc,
          here::here( "outdata", "Total number of incident active tuberculosis cases in pregnant women for the 30 high tuberculosis burden countries as classified by the WHO.PJD.csv"))
