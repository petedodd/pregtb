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
