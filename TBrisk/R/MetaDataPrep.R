# Install package (library) if not installed
usePackage <- function(p) {
  if (!is.element(p, installed.packages())) {
    install.packages(p, dep = TRUE)
  }
  require(p, character.only = TRUE)
}

usePackage("here") # for file path
usePackage("metafor") # metaanalysis
usePackage("data.table") # data manipulation
usePackage("dplyr") # data manipulation
usePackage("meta") # metaanalysis

# Load data
# D <- fread(here::here("metaanalysis", "data", "all_datan.csv"))
D <- fread(here::here("TBrisk", "indata", "all_datan_updated.csv")) # pregnancy data
DPP <- fread(here::here("TBrisk", "indata", "datapp_updated2.csv")) # postpartum data

names(D)

# All studies pregnancy
DR <- D[, .(FA, Full_study, country, year, HIV, IRR, Sample, pregYTBY, pregNTBY, pregYTBN, pregNTBN, m, mlo, mhi)]
DR <- DR[!is.na(m)]
DR
DR$pregnant <- as.numeric(DR$pregYTBY) + as.numeric(DR$pregYTBN)
DR$Notpregnant <- as.numeric(DR$pregNTBY) + as.numeric(DR$pregNTBN)
DR$study_type <- ifelse(DR$FA %in% c("Odayar", "Zenner", "Jonsson"), "cohort", "non_cohort")
DR$include <- ifelse(DR$FA %in% c("Rendell", "Zenner", "Jonsson"), "Yes", "No")
DR[, yi := log(m)]
DR[, si := (log(mhi) - log(mlo)) / 3.92]
DR[, vi := si^2]

# No HIV studies pregnancy
DRH <- DR[HIV != "yes"]
DRF <- DR[Full_study != "No"]
DRB <- DR[Full_study != "Yes"]

# Cohort studies pregnancy
DRC <- DR[study_type == "cohort"]

# Final Yes pregnancy studies: for now, only Rendell, Zenner, Jonsson
DRI <- DR[include == "Yes"]

# All studies postpartum
DRPP <- DPP[, .(FA, Full_study, site, country, year, HIV, IRR, Sample, ppYTBY, ppNTBY, ppYTBN, ppNTBN, m, mlo, mhi)]
DRPP$study_type <- ifelse(DRPP$FA %in% c("Odayar", "Zenner", "Jonsson"), "cohort", "non_cohort")
DRPP$include <- ifelse(DRPP$FA %in% c("Rendell", "Zenner", "Jonsson"), "Yes", "No")
DRPP <- DRPP[!is.na(m)]
DRPP

DRPP[, yi := log(m)]
DRPP[, si := (log(mhi) - log(mlo)) / 3.92]
DRPP[, vi := si^2]
DRPPF <- DRPP[Full_study != "No"]

# No HIV studies postpartum
DRPPH <- DRPP[HIV != "yes"]

# Cohort studies postpartum
DRPPC <- DRPP[study_type == "cohort"]

# Final Yes postpartum studies: for now, only Rendell, Zenner, Jonsson
DRPPI <- DRPP[include == "Yes"]

#  descriptives about the effect size estimates per year of publication
round(aggregate(yi ~ year, data = DR, FUN = function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))), 3)

# DR %>% group_by(FA, country) %>% summarise_if(is.numeric, ~mean(., na.rm=TRUE))

# add HIV data from Pete's analysis 

# pooled

# PT
##    PT.prior PT.preg    PT.pp tb.prior tb.preg tb.pp
##       <num>   <num>    <num>    <num>   <num> <num>
## 1: 18753898  505694 274722.1       91      10     4

## rate ratio with 95% C.I. estimate    lower    upper
##                 not-PPP  1.000000       NA       NA
##                 pregnant 2.588997 1.258043 4.733734
##                 PP       1.950755 0.585772 4.667024

P <- data.frame(
  FA = 'Odayar',
  country = 'South Africa',
  year = 2025,
  HIV = "yes",
  IRR = 'yes',
  Sample = 18753898+505694,
  pregYTBY = c(10,10),
  pregNTBY = 91,
  pregYTBN = c(505694-10, 505694-10),
  pregNTBN = 18753898-91,
  m = c(2.588997, 1.950755),
  mlo = c(1.258043, 0.585772),
  mhi = c(4.733734, 4.667024),
  clinical = c("Pregnancy", "Postpartum"),
  Full_study = "Yes"
)

P
# stratified by status
##    hiv_status_enrolment PT.prior PT.preg    PT.pp tb.prior tb.preg tb.pp
##        <haven_labelled>    <num>   <num>    <num>    <num>   <num> <num>
## 1:                    1 10389664  293833 157764.3       28       1     1
## 2:                    2  8364234  211861 116957.8       63       9     3


# PLHIV
## rate ratio with 95% C.I. estimate     lower     upper
##                 not-PPP  1.000000        NA        NA
##                 pregnant 5.728346 2.6431459 10.940670
##                 PP       3.577219 0.8467287  9.632884

# create a data frame
HIV <- data.frame(
  FA = 'Odayar1',
  country = 'South Africa',
  year = 2025,
  HIV = "yes",
  IRR = 'yes',
  Sample = 10389664+293833,
  pregYTBY = c(1,1),
  pregNTBY = 28,
  pregYTBN = c(293833-1, 293833-1),
  pregNTBN = 10389664-28,
  m = c(5.728346, 3.577219),
  mlo = c(2.6431459, 0.8467287),
  mhi = c(10.940670, 9.632884),
  clinical = c("Pregnancy", "Postpartum"),
  Full_study = "No"
)

# HIV uninfected
## rate ratio with 95% C.I. estimate      lower     upper
##                 not-PPP  1.000000         NA        NA
##                 pregnant 1.439228 0.06109441  6.661936
##                 PP       2.680539 0.11378739 12.407752

noHIV <- data.frame(
  FA = 'Odayar2',
  country = 'South Africa',
  year = 2025,
  HIV = "no",
  IRR = 'yes',
  Sample = 8364234+211861,
  pregYTBY = c(9,3),
  pregNTBY = 63,
  pregYTBN = c(211861-9, 211861-3),
  pregNTBN = 8364234-63,
  m = c(1.439228, 2.680539),
  mlo = c(0.06109441, 0.11378739),
  mhi = c(6.661936, 12.407752),
  clinical = c("Pregnancy", "Postpartum"),
  Full_study = "No"
)

DRH <- rbind(P, HIV, noHIV)


# Calculate yi and SE 
DRH$yi <- log(DRH$m)
DRH$si <- (log(DRH$mhi) - log(DRH$mlo)) / 3.92
DRH$vi <- DRH$si^2
DRH$include = 'Yes'

DRH

setdiff(names(DRH),names(DR))
setdiff(names(DR),names(DRH))

DR <- DR |> 
  select(FA, Full_study, country, year, HIV, IRR, Sample, pregYTBY, pregNTBY, pregYTBN, pregNTBN, m, mlo, mhi, yi, si, vi, include) 
DRPP <- DRPP |>
  select(FA, Full_study, country, year, HIV, IRR, Sample, ppYTBY, ppNTBY, ppYTBN, ppNTBN, m, mlo, mhi, yi, si, vi, include)


setdiff(names(DR),names(DRH))
setdiff(names(DRPP),names(DRPP))

DRHP <- DRH |> 
  filter(clinical == "Pregnancy") |> 
  select(-clinical) |> 
  select(names(DR))
DRHPP <- DRH |> 
  filter(clinical == "Postpartum") |> 
  rename_with(~ gsub('preg', 'pp', .)) |> 
  select(-clinical) |> 
  select(names(DRPP))

DR <- rbind(DR, DRHP)
DRPP <- rbind(DRPP, DRHPP)
