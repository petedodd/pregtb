# Meta analysis restricted to 3 studies (Rendell, Zenner, Jonsson)
library(here)

# Load data
source(here::here("TBrisk/R/MetaDataPrep.R"))

#' All analysis below is based on 3 studies: Rendell, Zenner, Jonsson
#'

## Fixed-Effects meta-analysis for TB risk during pregnancy ----
resFEP <- rma(
  yi = yi, # effect sizes: log(IRR)
  vi = vi, # variance
  data = DR[include == "Yes"],
  subset = (include == "Yes"), # Restrict to Yes studies(Rendell, Zenner, Jonsson)
  method = "FE" # Fixed-Effects model
)

print(resFEP)
predict(resFEP, transf = exp, digits = 3)

par(mar=c(4,4,1,2))

pdf(here::here("TBrisk/plots/FEPreg.pdf"))
forest(resFEP,
  atransf = exp,
  slab = paste0(
    DR[include == "Yes"][, FA], ", ",
    DR[include == "Yes"][, country], ", ",
    DR[include == "Yes"][, year]
  ),
  xlim = c(-20, 10), at = log(c(0.05, 0.25, 1, 5)),
  ilab = cbind(
    DR[include == "Yes"]$pregYTBY, DR[include == "Yes"]$pregYTBN,
    DR[include == "Yes"]$pregNTBY, DR[include == "Yes"]$pregNTBN
  ),
  ilab.xpos = c(-10, -7.5, -5.5, -3),
  cex = 0.75,
  ylim = c(-1, 6),
  ilab.pos = 2,
  xlab = "Incidence Risk Ratio",
  mlab = "",
  psize = 1,
  addcred = TRUE
)

text(-20, -1, pos = 4, cex = 0.75, bquote(paste("Fixed Effects Model")))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex = 0.75, font = 4)

### switch to bold font
par(font = 2)

### add column headings to the plot
text(-20, 5.0, "Author, Country and Year", pos = 4)
text(10, 5.0, "Incidence Risk Ratio [95% CI]", pos = 2)
text(c(-10.5, -8, -6, -3.5), 5.0, c("TB+", "TB-", "TB+", "TB-"))
text(c(-9.25, -4.75), 4.5, c("Pregnant", "Not pregnant"))

### set par back to the original settings
par(op)

dev.off()


## Random-Effects meta-analysis for TB risk during pregnancy ----
resREP <- rma(
  yi = yi,
  vi = vi,
  data = DR[include == "Yes"],
  subset = (include == "Yes"),
  slab = paste(FA, country, year, sep = ", ")
)

print(resREP)
predict(resREP, transf = exp, digits = 3)

pdf(here::here("TBrisk/plots/REPreg.pdf"))
forest(resREP,
  atransf = exp,
  slab = paste0(
    DR[include == "Yes"][, FA], ", ",
    DR[include == "Yes"][, country], ", ",
    DR[include == "Yes"][, year]
  ),
  xlim = c(-20, 10), at = log(c(0.05, 0.25, 1, 5)),
  ilab = cbind(
    DR[include == "Yes"]$pregYTBY, DR[include == "Yes"]$pregYTBN,
    DR[include == "Yes"]$pregNTBY, DR[include == "Yes"]$pregNTBN
  ),
  ilab.xpos = c(-10, -7.5, -5.5, -3),
  cex = 0.75,
  ylim = c(-1, 6),
  ilab.pos = 2,
  xlab = "Incidence Risk Ratio",
  mlab = "",
  psize = 1,
  addcred = TRUE
)

text(-20, -1, pos = 4, cex = 0.75, bquote(paste(
  "Random Effects Model for (Q = ",
  .(formatC(resREP$QE, digits = 2, format = "f")), ", df = ", .(resREP$k - resREP$p),
  ", p = ", .(formatC(resREP$QEp, digits = 2, format = "f")), "; ", I^2, " = ",
  .(formatC(resREP$I2, digits = 1, format = "f")), "%)"
)))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex = 0.75, font = 4)

### switch to bold font
par(font = 2)

### add column headings to the plot
text(-20, 5.0, "Author, Country and Year", pos = 4)
text(10, 5.0, "Incidence Risk Ratio [95% CI]", pos = 2)
text(c(-10.5, -8, -6, -3.5), 5.0, c("TB+", "TB-", "TB+", "TB-"))
text(c(-9.25, -4.75), 4.5, c("Pregnant", "Not pregnant"))

### set par back to the original settings
par(op)

dev.off()


## Fixed-Effects meta-analysis for TB risk during postpartum ----
resFEPP <- rma(
  yi = yi, # effect sizes: log(IRR)
  vi = vi, # variance
  data = DRPP[include == "Yes"],
  subset = (include == "Yes"), # Restrict to Yes studies(Rendell, Zenner, Jonsson)
  method = "FE" # Fixed-Effects model
)

print(resFEPP)
predict(resFEPP, transf = exp, digits = 3)

pdf(here::here("TBrisk/plots/FEPP.pdf"))
forest(resFEPP,
  atransf = exp,
  slab = paste0(
    DRPP[include == "Yes"][, FA], ", ",
    DRPP[include == "Yes"][, country], ", ",
    DRPP[include == "Yes"][, year]
  ),
  xlim = c(-20, 10), at = log(c(0.05, 0.25, 1, 5)),
  ilab = cbind(
    DRPP[include == "Yes"]$ppYTBY, DRPP[include == "Yes"]$ppYTBN,
    DRPP[include == "Yes"]$ppNTBY, DRPP[include == "Yes"]$ppNTBN
  ),
  ilab.xpos = c(-10, -7.5, -5.5, -3),
  cex = 0.75,
  ylim = c(-1, 5),
  ilab.pos = 2,
  xlab = "Incidence Risk Ratio",
  mlab = "",
  psize = 1,
  addcred = TRUE
)

text(-20, -1, pos = 4, cex = 0.75, bquote(paste("Fixed Effects Model")))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex = 0.75, font = 4)

### switch to bold font
par(font = 2)

### add column headings to the plot
text(-20, 4.0, "Author, Country and Year", pos = 4)
text(10, 4.0, "Incidence Risk Ratio [95% CI]", pos = 2)
text(c(-10.5, -8, -6, -3.5), 4.0, c("TB+", "TB-", "TB+", "TB-"))
text(c(-9.25, -4.75), 3.5, c("Postpartum", "Not postpartum"))

### set par back to the original settings
par(op)

dev.off()

# Random-Effects meta-analysis for TB risk during postpartum ----
resREPP <- rma(
  yi = yi,
  vi = vi,
  data = DRPP[include == "Yes"],
  subset = (include == "Yes"),
  slab = paste(FA, country, year, sep = ", ")
)

print(resREPP)
predict(resREPP, transf = exp, digits = 3)

pdf(here::here("TBrisk/plots/REPP.pdf"))
forest(resREPP,
  atransf = exp,
  slab = paste0(
    DRPP[include == "Yes"][, FA], ", ",
    DRPP[include == "Yes"][, country], ", ",
    DRPP[include == "Yes"][, year]
  ),
  xlim = c(-20, 10), at = log(c(0.05, 0.25, 1, 5)),
  ilab = cbind(
    DRPP[include == "Yes"]$ppYTBY, DRPP[include == "Yes"]$ppYTBN,
    DRPP[include == "Yes"]$ppNTBY, DRPP[include == "Yes"]$ppNTBN
  ),
  ilab.xpos = c(-10, -7.5, -5.5, -3),
  cex = 0.75,
  ylim = c(-1, 5),
  ilab.pos = 2,
  xlab = "Incidence Risk Ratio",
  mlab = "",
  psize = 1,
  addcred = TRUE
)

text(-20, -1, pos = 4, cex = 0.75, bquote(paste(
  "Random Effects Model for (Q = ",
  .(formatC(resREPP$QE, digits = 2, format = "f")), ", df = ", .(resREPP$k - resREPP$p),
  ", p = ", .(formatC(resREPP$QEp, digits = 2, format = "f")), "; ", I^2, " = ",
  .(formatC(resREPP$I2, digits = 1, format = "f")), "%)"
)))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex = 0.75, font = 4)

### switch to bold font
par(font = 2)

### add column headings to the plot
text(-20, 4.0, "Author, Country and Year", pos = 4)
text(10, 4.0, "Incidence Risk Ratio [95% CI]", pos = 2)
text(c(-10.5, -8, -6, -3.5), 4.0, c("TB+", "TB-", "TB+", "TB-"))
text(c(-9.25, -4.75), 3.5, c("Postpartum", "Not postpartum"))

### set par back to the original settings
par(op)

dev.off()

