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
library(data.table)
library(tidyverse)
library(viridis)
library(getTBinR)
library(dplyr)
library(plyr)
library(here)
library(janitor)

## setwd("U:/Documents/GitHub/pregTB")
# setwd("~/Documents/GitHub/pregtb")

# read in WHO TB data disaggregated by age and sex
# read in the data dictionary 
                                        # Estimated number of incident cases (all forms) - Worked it out from the main dataset???
fn <- here('indata/df.Rdata')
if(!file.exists(fn)){
  df<- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=estimates_age_sex", header = T)
  save(df,file=fn)
} else {
  load(fn)
}

fn <- here('indata/df_dic.Rdata')
if(!file.exists(fn)){
  df_dic <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=dictionary")
  save(df,file=fn)
} else {
  load(fn)
}

## Filter females of reproductive age group
# age groups selected to match the UN population age groups for now
df_1 <- df %>% filter(sex=="f", age_group %in% c("15-24", "25-34", "35-44", "45-54"))

# Births by age of mother
# Births by five-year age group of mother, region, subregion and country, 1950-2100 (thousands)								
births_average <- read_excel(here("indata", "births_med.xlsx"))
births_lo<- read_excel(here("indata", "births_lo.xlsx"))
births_hi<- read_excel(here("indata", "births_hi.xlsx"))


births <- rbind(births_average, births_lo, births_hi)

for (k in 1:length(names(births))){ #convert all the rows with type 'integer' to 'numeric
  if (is.integer(births[,k])){
    births[,k] <- as.numeric(births[,k])	
  }
}
# recategorizing age groups to match the WHO TB data

births$"15-24" <- rowSums(births[c("15-19", "20-24")], na.rm=TRUE)
births$"25-34" <- rowSums(births[c("25-29", "30-34")], na.rm=TRUE)
births$"35-44" <- rowSums(births[c("35-39", "40-44")], na.rm=TRUE)
births$"45-54" <- births$`45-49`

names(births)[3] <- "country"
births <- births %>% select(-c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")) %>% 
  gather(age_group, estimate, 7:10)

births <- separate(births, Period, c("from", "to"))
births$to <- as.numeric(births$to)-1
births$year <- mapply(seq,births$from,births$to,SIMPLIFY=FALSE)
births$estimate <- (births$estimate*1000)/5
births <- births %>% 
  unnest(year) %>% 
  select(-from,-to) %>% 
  spread(Variant, estimate)

births <- births %>% dplyr::rename(births_lo=`Low variant`, births_hi=`High variant`, births_best=`Medium variant`)

births <- births %>% filter(!country %in%
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
births$country <- (mapvalues((births$country), from = c(
  "United Kingdom",
  "Dem. People's Republic of Korea",
  "Micronesia (Fed. States of)",
  "State of Palestine",
  "TFYR Macedonia",
  "Czechia")
  
  , to = c(
    "United Kingdom of Great Britain and Northern Ireland",
    "Democratic People's Republic of Korea",
    "Micronesia (Federated States of)",
    "West Bank and Gaza Strip",
    "North Macedonia",
    "Czech Republic")
))


# add ISO country codes to births
load(here("isodict.Rdata"))
code <- read_csv(here("indata", "all.csv")) # has more details on codes
code <- code %>% dplyr::rename(country=name, iso3=`alpha-3`) %>% select(country, `country-code`, iso3, region, `sub-region`)

births <- births %>% select(-c("Notes", `Country code`)) %>% left_join(ISO, by = "country") 
# births2<- births %>% select(-c("Notes")) %>% left_join(code, by = "country", `Country code`)%>% filter(!is.na(iso3))

# filter 2017 data to match the WHO TB data 
births_2017 <- births %>% filter(year==2017) %>% select(country,iso3, g_whoregion, age_group, births_best, births_lo, births_hi)

# read in population of females in the reproductive age group
# Annual female population by five-year age group, region, subregion and country, 1950-2100 (thousands)								
pop_f <- read_excel(here("indata","pop_med.xlsx"))
pop_f$"15-24" <- rowSums(pop_f[c("15-19", "20-24")], na.rm=TRUE)
pop_f$"25-34" <- rowSums(pop_f[c("25-29", "30-34")], na.rm=TRUE)
pop_f$"35-44" <- rowSums(pop_f[c("35-39", "40-44")], na.rm=TRUE)
pop_f$"45-54" <- rowSums(pop_f[c("45-49", "50-54")], na.rm=TRUE)

pop_f <- pop_f %>% dplyr::rename(pop_f=Variant, country=`Region, subregion, country or area *`, year=`Reference date (as of 1 July)`)  %>%
  select(c("country","pop_f", "year", "15-24", "25-34", "35-44", "45-54")) %>% gather(age_group, pop_f, c("15-24", "25-34", "35-44", "45-54"))
pop_f$pop_f <- pop_f$pop_f*1000

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

# Changing some country names to match the WHO TB data and ISO codes #
pop_f$country <- (mapvalues((pop_f$country), from = c(
  "United Kingdom",
  "Dem. People's Republic of Korea",
  "Micronesia (Fed. States of)",
  "State of Palestine",
  "TFYR Macedonia",
  "Czechia")
  
  , to = c(
    "United Kingdom of Great Britain and Northern Ireland",
    "Democratic People's Republic of Korea",
    "Micronesia (Federated States of)",
    "West Bank and Gaza Strip",
    "North Macedonia",
    "Czech Republic")
))
pop_f <- pop_f %>% left_join(ISO, by = "country") %>% select(iso3, age_group, year, pop_f)

pop_f2017 <- pop_f %>% filter(year==2017) %>% select(-year) 
num_births <- births_2017 %>% left_join(pop_f2017, by=c("iso3", "age_group")) %>% 
  gather(metric, value, c("births_best", "births_lo", "births_hi"))
# num_births$value <- num_births$pop_f * (num_births$value/1000) #  absolute number of births 
num_births <- num_births %>% spread(metric, value) %>% select(-country)

# combining the different datasets - births, population of females in the reproductive age and TB incident case in women in the reproductive age group
new_df_births <- df_1 %>% select(country,iso3, iso_numeric, year, age_group, best, lo, hi) %>% left_join(num_births, by=c("iso3", "age_group")) %>%
  dplyr::rename(TBI_best=best, TBI_lo=lo, TBI_hi=hi)
new_df_births$g_whoregion[is.na(new_df_births$g_whoregion)] <- "AFR" # problem with Swaziland

# df_2 <- ISO %>% select(-iso2, -iso3, -iso_numeric) %>% left_join(df, by="country")

# Estimate the incidence of TB in pregnancy
new_df_births$pregTBI_best <- new_df_births$births_best * 280/365 * (new_df_births$TBI_best/new_df_births$pop_f)
new_df_births$pregTBI_lo   <- new_df_births$births_lo   * 280/365 * (new_df_births$TBI_lo/new_df_births$pop_f)
new_df_births$pregTBI_hi   <- new_df_births$births_hi   * 280/365 * (new_df_births$TBI_hi/new_df_births$pop_f)

# Estimate the incidence of TB in post-partum period
new_df_births$ppTBI_best <- new_df_births$births_best * 3/12 * (new_df_births$TBI_best/new_df_births$pop_f)
new_df_births$ppTBI_lo   <- new_df_births$births_lo   * 3/12 * (new_df_births$TBI_lo/new_df_births$pop_f)
new_df_births$ppTBI_hi   <- new_df_births$births_hi   * 3/12 * (new_df_births$TBI_hi/new_df_births$pop_f)

new_df_births$pregTBI_best_sd <- sqrt((new_df_births$births_hi - new_df_births$births_best)^2 + (new_df_births$TBI_hi - new_df_births$TBI_best)^2)
new_df_births$pregTBI_best_sd1 <- sqrt((0.5*(new_df_births$births_hi - new_df_births$births_lo)/sqrt(3))^2 + (0.5*(new_df_births$TBI_hi - new_df_births$TBI_lo)/sqrt(3))^2)
new_df_births$pregTBI_best_sd2 <- sqrt((0.5*(new_df_births$births_hi - new_df_births$births_lo))^2 + (0.5*(new_df_births$TBI_hi - new_df_births$TBI_lo))^2)
new_df_births$pregTBI_best_sd3 <- sqrt(((new_df_births$births_hi - new_df_births$births_lo)/sqrt(12))^2 + ((new_df_births$TBI_hi - new_df_births$TBI_lo)/sqrt(12))^2)

pregTB_births <- new_df_births %>% dplyr::group_by(country) %>% summarise(pregTBI_best=sum(pregTBI_best), pregTBI_lo=sum(pregTBI_lo), pregTBI_hi=sum(pregTBI_hi))

ipregTB_births <- new_df_births %>% dplyr::group_by(country, iso3) %>% summarise(ipregTBI_best=1.3*(pregTBI_best), ipregTBI_lo=1.3*(pregTBI_lo), ipregTBI_hi=1.3*(pregTBI_hi))

key_parms <- c("pop_f", "TBI_best", "TBI_lo", "TBI_hi", "births_best", "births_lo", "births_hi", "pregTBI_best", 
               "pregTBI_lo", "pregTBI_hi", "ppTBI_best", 
               "ppTBI_lo", "ppTBI_hi")


summary_regions <- new_df_births%>%group_by(g_whoregion)%>%summarise_at(key_parms, funs(sum), na.rm=T) %>% adorn_totals("row")

summary_regions_byagegroup <- new_df_births%>%group_by(g_whoregion, age_group)%>%summarise_at(key_parms, funs(sum), na.rm=T) 

write.csv(summary_regions, here( "outdata", "Total number of incident tuberculosis cases in pregnant women.csv"))

regions <- summary_regions_byagegroup %>% 
  select(g_whoregion, age_group, births_best, pregTBI_best, ppTBI_best) %>%
  dplyr::rename(Pregnancy = pregTBI_best, Postpartum = ppTBI_best) %>%
  gather(variable, value, c("Pregnancy", "Postpartum")) %>% 
  mutate(TBI_rate = value/births_best*1000) %>%  
  ggplot(aes(x=age_group,y=TBI_rate,fill=variable)) +
  geom_bar(stat="identity",position="dodge") +
  # scale_fill_discrete(name="variable",
  #                     breaks=c(1, 2),
  #                     labels=c("Pregnancy", "Postpartum")) +
  xlab("Age in years")+ylab("TB incidence rate per 1000 pregnant women") + facet_wrap(~g_whoregion) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1))

ggsave(plot=regions,filename=here("plots/TB incidence.svg"),
       width=10, height=8, dpi=400)
ggsave(plot=regions,filename=here("plots/TB incidence.pdf"),
       width=10, height=8)
# summary_SEA <- new_df_births%>%filter(g_whoregion=="SEA")%>%group_by(country)%>%
#   summarise_at(key_parms, funs(sum), na.rm=T)

# The 30 TB high burden countries
hbc <- c("Angola", "Bangladesh", "Brazil", "China", "Democratic People's Republic of Korea", "Democratic Republic of the Congo", "Ethiopia", "India", "Indonesia", "Kenya", "Mozambique", "Myanmar", "Nigeria", "Pakistan", 
         "Philippines", "Russian Federation", "South Africa", "Thailand", "United Republic of Tanzania", "Viet Nam", "Cambodia", "Central African Republic", "Congo", "Lesotho", "Liberia", 
         "Namibia", "Papua New Guinea", "Sierra Leone", "Zambia", "Zimbabwe")

summary_hbc <- new_df_births %>% filter(country %in% hbc) %>% group_by(country)%>%summarise_at(key_parms, funs(sum), na.rm=T) %>% adorn_totals("row")

write.csv(summary_hbc, here( "outdata", "Total number of incident active tuberculosis cases in pregnant women for the 30 high tuberculosis burden countries as classified by the WHO.csv"))
