# Meta analysis restricted to 3 studies (Rendell, Zenner, Jonsson)
library(here)
library(ggplot2)

# Load data
source(here::here("TBrisk/R/MetaDataPrep.R"))

#' All analysis below is based on 3 studies: Rendell, Zenner, Jonsson
#'

glimpse(DR[include=="Yes"])
DR$pregYTBY <- round(DR$pregYTBY, 0)

## Fixed-Effects meta-analysis for TB risk during pregnancy ---- w no HIV data
resFEP <- rma(
  yi = yi, # effect sizes: log(IRR)
  vi = vi, # variance
  data = DR[include == "Yes" & HIV=='no' & Full_study=='Yes'],
  method = "FE" # Fixed-Effects model
)

print(resFEP)
predict(resFEP, transf = exp, digits = 3)

## Random-Effects meta-analysis for TB risk during pregnancy ---- w no HIV data
resREP <- rma(
  yi = yi,
  vi = vi,
  data = DR[include == "Yes" & HIV=='no' & Full_study=='Yes'],
  slab = paste(FA, country, year, sep = ", ")
)

print(resREP)
predict(resREP, transf = exp, digits = 3)

## Random-Effects meta-analysis for TB risk during pregnancy ---- w HIV data
resREPH <- rma(
  yi = yi,
  vi = vi,
  data = DR[include == "Yes" & Full_study=='No' & HIV=='yes'],
  slab = paste(FA, country, year, sep = ", ")
)

print(resREPH)
predict(resREPH, transf = exp, digits = 3)


## Fixed-Effects meta-analysis for TB risk during postpartum ----
resFEPP <- rma(
  yi = yi, # effect sizes: log(IRR)
  vi = vi, # variance
  data = DRPP[include == "Yes"& HIV=='no' & Full_study=='Yes'],
  method = "FE" # Fixed-Effects model
)

print(resFEPP)
predict(resFEPP, transf = exp, digits = 3)

# Random-Effects meta-analysis for TB risk during postpartum ---- no HIV data
resREPP <- rma(
  yi = yi,
  vi = vi,
  data = DRPP[include == "Yes"& HIV=='no' & Full_study=='Yes'],
  slab = paste(FA, country, year, sep = ", ")
)

print(resREPP)
predict(resREPP, transf = exp, digits = 3)

# Random-Effects meta-analysis for TB risk during postpartum ---- w HIV data
resREPPH <- rma(
  yi = yi,
  vi = vi,
  data = DRPP[include == "Yes" & Full_study=='No' & HIV=='yes'],
  slab = paste(FA, country, year, sep = ", ")
)

print(resREPPH)
predict(resREPPH, transf = exp, digits = 3)

#' Make predictions for plot data:
preg <- predict(resREP,transf = exp)
pp <- predict(resREPP,transf = exp)
pregH <- predict(resREPH,transf = exp)
ppH <- predict(resREPPH,transf = exp)

DD1 <- DR |> 
  filter(include == "Yes" & HIV=='no' & Full_study=="Yes") |> 
  select(FA, country, HIV, year, Sample, pregYTBY, pregNTBY,  pregYTBN,  pregNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "Pregnancy")
DD1h <- DR |> 
  filter(include == "Yes" & HIV=='yes' & Full_study=='No') |> 
  mutate(FA = gsub("\\d", "", FA)) |>
  select(FA, country, HIV, year, Sample, pregYTBY, pregNTBY,  pregYTBN,  pregNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "PregnancyHIV")
DD2 <- DRPP |> 
  filter(include == "Yes" & HIV=='no' & Full_study=="Yes") |> 
  select(FA, country, HIV, year, Sample, pregYTBY=ppYTBY, pregNTBY=ppNTBY,  pregYTBN=ppYTBN,  pregNTBN=ppNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "Postpartum")
DD2h <- DRPP |>
  filter(include == "Yes" & HIV=='yes' & Full_study=='No') |> 
  mutate(FA = gsub("\\d", "", FA)) |>
  select(FA, country, HIV, year, Sample, pregYTBY=ppYTBY, pregNTBY=ppNTBY,  pregYTBN=ppYTBN,  pregNTBN=ppNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "PostpartumHIV")

DD <- rbind(DD1, DD2, DD1h, DD2h)
DD$lab <- paste0(DD$FA, ", ", DD$country, ", ", DD$year)


#' Summary data for combined forest plot:
f1 <- function(x)format(round(x,1),nsmall=1)
cnz <- c('Pregnancy',
         'Postpartum',
         'PregnancyHIV',
         'PostpartumHIV')
predz <- data.table(clinical=cnz,
                    `Incidence Risk Ratio` = c(preg$pred,pp$pred,pregH$pred,ppH$pred),
                    mlo = c(preg$ci.lb,pp$ci.lb,pregH$ci.lb,ppH$ci.lb),
                    mhi = c(preg$ci.ub,pp$ci.ub,pregH$ci.ub,ppH$ci.ub),
                    lab=paste0('SUMMARY (',expression(I^2),'=',
                               f1(c(resREP$I2,resREPP$I2,resREPH$I2,resREPPH$I2)),'%)')
)
predz[,SE:=(mhi-mlo)/3.92]
predz[,qty:='summary']
predz[,mid:=`Incidence Risk Ratio`]
predz[,CI:=paste0(f1(mid),' (',f1(mlo),' - ',f1(mhi),')')]
predz[,wt:='100.0%']
predz[,w:=1]

#' Appending plot data to inputs:
DD[,qty:='study']
DD[,mid:=`Incidence Risk Ratio`]
DD[,CI:=paste0(f1(`Incidence Risk Ratio`),' (',f1(mlo),' - ',f1(mhi),')')]
DD[,wt:=1/SE^2]
DD[,wtt:=sum(wt),by=.(clinical)]
DD[,wt:=1e2*wt/wtt]
DD[,wt:=paste0(f1(wt),'%')]
DD[,w:=0]

#' Combined plot data:
B <- rbind(
  DD[,.(lab,`Incidence Risk Ratio`,mlo,mhi,SE,clinical,
        qty,CI,wt,w)],
  predz[,.(lab,`Incidence Risk Ratio`,mlo,mhi,SE,clinical,
           qty,CI,wt,w)]
)


(lbz <- unique(as.character(B$lab)))
lbz2 <- lbz
B[,lab:=factor(lab,levels=lbz2,ordered = TRUE)]
B[,clinical.g:='Pregnancy no HIV']
B[clinical=='Postpartum',clinical.g:='Postpartum no HIV']
B[clinical=='PregnancyHIV',clinical.g:='Pregnancy HIV']
B[clinical=='PostpartumHIV',clinical.g:='Postpartum HIV']

(lbz <- unique(as.character(B$clinical.g)))
lbz <- c('Pregnancy no HIV',
         'Pregnancy HIV',
         'Postpartum no HIV',
         'Postpartum HIV')

lbz_labels <- c(
  "Pregnancy living without HIV",
  "Pregnancy living with HIV",
  "Postpartum living without HIV",
  "Postpartum living with HIV"
)
B[,clinical.g:=factor(clinical.g,levels=lbz, labels = lbz_labels, ordered = TRUE)]
labdat <- B[1]
labdat[,txt:=' Weight (%)']
labdat2 <- B[1]
labdat2[,txt:='IRR (95% confidence interval)']

B$lab <- forcats::fct_rev(factor(B$lab))

B1 <- B |> 
  filter(!(qty == 'summary' & grepl('HIV', clinical)))

B1 <- B1 |> 
  mutate(
    hiv = ifelse(grepl('HIV', clinical), 'yes', 'no'),
    hiv = factor(hiv, levels = c('no', 'yes'))
  )

#' Create publication forest plot figure:
SA <- ggplot(B1,aes(lab,y=`Incidence Risk Ratio`,
                    ymin=mlo,
                    ymax=mhi,
                    col=qty)) +
  geom_point(aes(size=1/SE^2,shape=qty)) +
  geom_errorbar(aes(width=w/2)) +
  scale_y_continuous(limits = c(-1,NA))+
  scale_color_manual(values=c('study'="black",'summary'="blue"))+
  scale_shape_manual(values=c('study'=22,'summary'=23))+
  xlab('') +
  ylab('Incidence Risk Ratio for tuberculosis')+
  facet_grid(clinical.g ~ ., 
             labeller = labeller(clinical.g = function(labels) {
               sapply(labels, function(label) {
                 paste(strwrap(label, width = 15), collapse = "\n") # Adjust width as needed
               })}),
             scales = 'free',space='free',
             switch='x'
  )+
  coord_flip() +
  guides(size='none',color='none',shape='none')+
  theme_classic() +
  theme(
    panel.spacing = unit(1, "lines"),  # Adjust spacing between facets
    strip.background = element_blank(),
    strip.placement = "outside",
    # Increase axis text size
    axis.text.x = element_text(size = 12),  # X-axis text size
    axis.text.y = element_text(size = 12),  # Y-axis text size
    # Increase axis title size
    axis.title.x = element_text(size = 12),  # X-axis title size
    axis.title.y = element_text(size = 12),  # Y-axis title size
    # Increase facet label size
    strip.text = element_text(size = 12)  # Facet label size
  ) +
  geom_text(aes(x = lab, y = 15, label = CI, hjust = 'right')) +
  geom_text(aes(x = lab, y = -0.8, label = wt)) +
  geom_text(data = labdat, aes(x = 4.5, y = -0.5, label = txt), size = 4) +
  geom_text(data = labdat2, aes(x = 4.5, y = 13, label = txt), size = 4) +
  ggpubr::grids()

SA
ggsave(SA,file=here::here('TBrisk/plots/ForestPlot.pdf'),h=13,w=12)
ggsave(SA,file=here::here('TBrisk/plots/ForestPlot.eps'),h=13,w=12)
ggsave(SA,file=here::here('TBrisk/plots/ForestPlot.png'),h=11,w=10)

# Alternative plot with different scales for HIV/no HIV
ordered_levels <- c(
  "Pregnancy living without HIV",
  "Pregnancy living with HIV",
  "Postpartum living without HIV",
  "Postpartum living with HIV"
)

# Ensure factors are ordered in all relevant datasets
labdat <- rbind(
  labdat |> mutate(hiv = 'no', clinical.g = 'Pregnancy living without HIV', x = 4.5, y = -0.2),
  labdat |> mutate(hiv = 'no', clinical.g = 'Postpartum living without HIV', x = 1.5, y = -0.5),
  labdat |> mutate(hiv = 'yes', clinical.g = 'Pregnancy living with HIV', x = 1.55, y = -0.5),
  labdat |> mutate(hiv = 'yes', clinical.g = 'Postpartum living with HIV', x = 1.5, y = -0.5)
) |> distinct()

labdat2 <- rbind(
  labdat2 |> mutate(hiv = 'no', clinical.g = 'Pregnancy living without HIV', x = 4.5, y = 3.8),
  labdat2 |> mutate(hiv = 'no', clinical.g = 'Postpartum living without HIV', x = 1.5, y = 3.8),
  labdat2 |> mutate(hiv = 'yes', clinical.g = 'Pregnancy living with HIV', x = 1.55, y = 14),
  labdat2 |> mutate(hiv = 'yes', clinical.g = 'Postpartum living with HIV', x = 1.5, y = 14)
) |> distinct()

# Set factor levels for proper ordering
labdat$clinical.g <- factor(labdat$clinical.g, levels = ordered_levels, ordered = TRUE)
labdat2$clinical.g <- factor(labdat2$clinical.g, levels = ordered_levels, ordered = TRUE)
B1$clinical.g <- factor(B1$clinical.g, levels = ordered_levels, ordered = TRUE)
B1$hiv <- factor(B1$hiv, levels = c("no", "yes"))
labdat$hiv <- factor(labdat$hiv, levels = c("no", "yes"))
labdat2$hiv <- factor(labdat2$hiv, levels = c("no", "yes"))
labdat2$pp <- ifelse(grepl('Postpartum', labdat2$clinical.g), 'yes', 'no')
labdat$pp <- ifelse(grepl('Postpartum', labdat$clinical.g), 'yes', 'no')
# Plot
SA <- ggplot(B1, aes(lab, y = `Incidence Risk Ratio`, ymin = mlo, ymax = mhi, col = qty)) +
  geom_point(aes(size = 1 / SE^2, shape = qty)) +
  geom_errorbar(aes(width = w / 2)) +
  scale_y_continuous(limits = c(-1, NA)) +
  scale_color_manual(values = c('study' = "black", 'summary' = "blue")) +
  scale_shape_manual(values = c('study' = 22, 'summary' = 23)) +
  xlab('') +
  ylab('Incidence Risk Ratio for tuberculosis') +
  facet_wrap(~ clinical.g, 
             labeller = labeller(clinical.g = function(labels) {
               sapply(labels, function(label) {
                 paste(strwrap(label, width = 30), collapse = "\n")
               })
             }),
             scales = 'free',
             switch = 'x'
  ) +
  coord_flip() +
  guides(size = 'none', color = 'none', shape = 'none') +
  theme_classic() +
  theme(
    panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  geom_text(aes(x = lab, y = ifelse(hiv == 'no', 4.5, 16), label = CI, hjust = 'right')) +
  geom_text(aes(x = lab, y = ifelse(hiv == 'no', -0.1, -0.3), label = wt)) +
  geom_text(data = labdat, aes(x = x, y = y, label = ifelse(pp == 'no', txt, '')), size = 4) +
  geom_text(data = labdat2, aes(x = x, y = y, label = ifelse(pp == 'no', "IRR (95% CI)", '')), size = 4) +
  ggpubr::grids() +
  scale_x_discrete(labels = function(y) stringr::str_wrap(y, width = 15))

SA
ggsave(SA,file=here::here('TBrisk/plots/ForestPlotNew.pdf'),h=8,w=12)
ggsave(SA,file=here::here('TBrisk/plots/ForestPlotNew.eps'),h=8,w=12)
ggsave(SA,file=here::here('TBrisk/plots/ForestPlotNew.png'),h=8,w=14)

# save out data
m0 <- B |> 
  filter(qty == 'summary' & grepl('HIV', clinical)) |> 
  select(period=clinical, m=`Incidence Risk Ratio`, lo=mlo, hi=mhi, SE, description=clinical.g) |>
  mutate(period = ifelse(grepl('Pregnancy',period), 'P', 'PP'))

m0

m1 <- B |> 
  filter(qty == 'summary' & !grepl('HIV', clinical)) |> 
  select(period=clinical, m=`Incidence Risk Ratio`, lo=mlo, hi=mhi, SE, description=clinical.g) |>
  mutate(period = ifelse(grepl('Pregnancy',period), 'PH0', 'PPH0'))

m1

m2 <- DD |> 
  filter(HIV == 'yes') |> 
  select(period=clinical, m=`Incidence Risk Ratio`, lo=mlo, hi=mhi, SE) |>
  mutate(description = ifelse(grepl('Pregnancy',period), 'Pregnancy HIV data', 'Postpartum HIV data'),
         period = ifelse(grepl('Pregnancy',period), 'PH1', 'PPH1')
  )

m2

meta_summary <- rbind(m0, m1, m2) 
meta_summary

## create directory if missing and save out
fn <- here::here('TBrisk/outdata')
if(!file.exists(fn)) dir.create(fn)

meta_summary |> fwrite(here::here('TBrisk/outdata/meta_summary.csv'))

# Sensitivity analysis
# Include studies initially excluded in the primary synthesis: Crampin and Espinal
## Random-Effects meta-analysis for TB risk during pregnancy ---- w no HIV data
resREPSA <- rma(
  yi = yi,
  vi = vi,
  data = DR[HIV=='no' & Full_study=='Yes'],
  slab = paste(FA, country, year, sep = ", ")
)

print(resREPSA)
predict(resREPSA, transf = exp, digits = 3)

## Random-Effects meta-analysis for TB risk during pregnancy ---- w HIV data
resREPHSA <- rma(
  yi = yi,
  vi = vi,
  data = DR[HIV=='yes' & FA %in% c('Crampin', 'Odayar1')],
  slab = paste(FA, country, year, sep = ", ")
)

print(resREPHSA)
predict(resREPHSA, transf = exp, digits = 3)

# Random-Effects meta-analysis for TB risk during postpartum ---- no HIV data
resREPPSA <- rma(
  yi = yi,
  vi = vi,
  data = DRPP[HIV=='no' & Full_study=='Yes'],
  slab = paste(FA, country, year, sep = ", ")
)

print(resREPPSA)
predict(resREPPSA, transf = exp, digits = 3)

# Random-Effects meta-analysis for TB risk during postpartum ---- w HIV data
resREPPHSA <- rma(
  yi = yi,
  vi = vi,
  data = DRPP[HIV=='yes' & FA %in% c('Crampin', 'Odayar1')],
  slab = paste(FA, country, year, sep = ", ")
)

print(resREPPHSA)
predict(resREPPHSA, transf = exp, digits = 3)

#' Make predictions for plot data:
pregSA <- predict(resREPSA,transf = exp)
ppSA <- predict(resREPPSA,transf = exp)
pregHSA <- predict(resREPHSA,transf = exp)
ppHSA <- predict(resREPPHSA,transf = exp)

DD1 <- DR |> 
  filter(HIV=='no' & Full_study=='Yes') |> 
  select(FA, country, HIV, year, Sample, pregYTBY, pregNTBY,  pregYTBN,  pregNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "Pregnancy")
DD1h <- DR |> 
  filter(HIV=='yes' & FA %in% c('Crampin', 'Odayar1')) |> 
  mutate(FA = gsub("\\d", "", FA)) |>
  select(FA, country, HIV, year, Sample, pregYTBY, pregNTBY,  pregYTBN,  pregNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "PregnancyHIV")
DD2 <- DRPP |> 
  filter(HIV=='no' & Full_study=='Yes') |> 
  select(FA, country, HIV, year, Sample, pregYTBY=ppYTBY, pregNTBY=ppNTBY,  pregYTBN=ppYTBN,  pregNTBN=ppNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "Postpartum")
DD2h <- DRPP |>
  filter(HIV=='yes' & FA %in% c('Crampin', 'Odayar1')) |> 
  mutate(FA = gsub("\\d", "", FA)) |>
  select(FA, country, HIV, year, Sample, pregYTBY=ppYTBY, pregNTBY=ppNTBY,  pregYTBN=ppYTBN,  pregNTBN=ppNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "PostpartumHIV")

DD <- rbind(DD1, DD2, DD1h, DD2h)
DD$lab <- paste0(DD$FA, ", ", DD$country, ", ", DD$year)


#' Summary data for combined forest plot:
f1 <- function(x)format(round(x,1),nsmall=1)
cnz <- c('Pregnancy',
         'Postpartum',
         'PregnancyHIV',
         'PostpartumHIV')
predz <- data.table(clinical=cnz,
                    `Incidence Risk Ratio` = c(pregSA$pred,ppSA$pred,pregHSA$pred,ppHSA$pred),
                    mlo = c(pregSA$ci.lb,ppSA$ci.lb,pregHSA$ci.lb,ppHSA$ci.lb),
                    mhi = c(pregSA$ci.ub,ppSA$ci.ub,pregHSA$ci.ub,ppHSA$ci.ub),
                    lab=paste0('SUMMARY (',expression(I^2),'=',
                               f1(c(resREPSA$I2,resREPPSA$I2,resREPHSA$I2,resREPPHSA$I2)),'%)')
)
predz[,SE:=(mhi-mlo)/3.92]
predz[,qty:='summary']
predz[,mid:=`Incidence Risk Ratio`]
predz[,CI:=paste0(f1(mid),' (',f1(mlo),' - ',f1(mhi),')')]
predz[,wt:='100.0%']
predz[,w:=1]

#' Appending plot data to inputs:
DD[,qty:='study']
DD[,mid:=`Incidence Risk Ratio`]
DD[,CI:=paste0(f1(`Incidence Risk Ratio`),' (',f1(mlo),' - ',f1(mhi),')')]
DD[,wt:=1/SE^2]
DD[,wtt:=sum(wt),by=.(clinical)]
DD[,wt:=1e2*wt/wtt]
DD[,wt:=paste0(f1(wt),'%')]
DD[,w:=0]

#' Combined plot data:
B <- rbind(
  DD[,.(lab,`Incidence Risk Ratio`,mlo,mhi,SE,clinical,
        qty,CI,wt,w)],
  predz[,.(lab,`Incidence Risk Ratio`,mlo,mhi,SE,clinical,
           qty,CI,wt,w)]
)


(lbz <- unique(as.character(B$lab)))
lbz2 <- lbz
B[,lab:=factor(lab,levels=lbz2,ordered = TRUE)]
B[,clinical.g:='Pregnancy no HIV']
B[clinical=='Postpartum',clinical.g:='Postpartum no HIV']
B[clinical=='PregnancyHIV',clinical.g:='Pregnancy HIV']
B[clinical=='PostpartumHIV',clinical.g:='Postpartum HIV']

(lbz <- unique(as.character(B$clinical.g)))
lbz <- c('Pregnancy no HIV',
         'Pregnancy HIV',
         'Postpartum no HIV',
         'Postpartum HIV')

lbz_labels <- c(
  "Pregnancy living without HIV",
  "Pregnancy living with HIV",
  "Postpartum living without HIV",
  "Postpartum living with HIV"
)
B[,clinical.g:=factor(clinical.g,levels=lbz, labels=lbz_labels, ordered = TRUE)]
labdat <- B[1]
labdat[,txt:=' Weight (%)']
labdat2 <- B[1]
labdat2[,txt:='IRR (95% confidence interval)']

B$lab <- forcats::fct_rev(factor(B$lab))

B1 <- B 

B1 <- B1 |> 
  mutate(
    hiv = ifelse(grepl('HIV', clinical), 'yes', 'no'),
    hiv = factor(hiv, levels = c('no', 'yes'))
  )

#' Create publication forest plot figure:
ordered_levels <- c(
  "Pregnancy living without HIV",
  "Postpartum living without HIV",
  "Pregnancy living with HIV",
  "Postpartum living with HIV"
)

# Ensure factors are ordered in all relevant datasets
labdat <- rbind(
  labdat |> mutate(hiv = 'no', clinical.g = 'Pregnancy living without HIV', x = 5.4, y = -0.5),
  labdat |> mutate(hiv = 'no', clinical.g = 'Postpartum living without HIV', x = 4.5, y = -0.5),
  labdat |> mutate(hiv = 'yes', clinical.g = 'Pregnancy living with HIV', x = 1.5, y = -0.5),
  labdat |> mutate(hiv = 'yes', clinical.g = 'Postpartum living with HIV', x = 1.5, y = -0.5)
) |> distinct()

labdat2 <- rbind(
  labdat2 |> mutate(hiv = 'no', clinical.g = 'Pregnancy living without HIV', x = 5.4, y = 3.8),
  labdat2 |> mutate(hiv = 'no', clinical.g = 'Postpartum living without HIV', x = 4.5, y = 3.8),
  labdat2 |> mutate(hiv = 'yes', clinical.g = 'Pregnancy living with HIV', x = 1.5, y = 17),
  labdat2 |> mutate(hiv = 'yes', clinical.g = 'Postpartum living with HIV', x = 1.5, y = 12.5)
) |> distinct()

# Set factor levels for proper ordering
labdat$clinical.g <- factor(labdat$clinical.g, levels = ordered_levels, ordered = TRUE)
labdat2$clinical.g <- factor(labdat2$clinical.g, levels = ordered_levels, ordered = TRUE)
B1$clinical.g <- factor(B1$clinical.g, levels = ordered_levels, ordered = TRUE)
B1$hiv <- factor(B1$hiv, levels = c("no", "yes"))
labdat$hiv <- factor(labdat$hiv, levels = c("no", "yes"))
labdat2$hiv <- factor(labdat2$hiv, levels = c("no", "yes"))

# Plot
SA <- ggplot(B1, aes(lab, y = `Incidence Risk Ratio`, ymin = mlo, ymax = mhi, col = qty)) +
  geom_point(aes(size = 1 / SE^2, shape = qty)) +
  geom_errorbar(aes(width = w / 2)) +
  scale_y_continuous(limits = c(-1, NA)) +
  scale_color_manual(values = c('study' = "black", 'summary' = "blue")) +
  scale_shape_manual(values = c('study' = 22, 'summary' = 23)) +
  xlab('') +
  ylab('Incidence Risk Ratio for tuberculosis') +
  facet_wrap(~ clinical.g, 
             labeller = labeller(clinical.g = function(labels) {
               sapply(labels, function(label) {
                 paste(strwrap(label, width = 30), collapse = "\n")
               })
             }),
             scales = 'free',
             switch = 'x'
  ) +
  coord_flip() +
  guides(size = 'none', color = 'none', shape = 'none') +
  theme_classic() +
  theme(
    panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  geom_text(aes(x = lab, y = ifelse(hiv == 'no', 4.5, 16), label = CI, hjust = 'right')) +
  geom_text(aes(x = lab, y = ifelse(hiv == 'no', -0.8, -0.8), label = wt)) +
  geom_text(data = labdat, aes(x = x, y = y, label = ifelse(hiv == 'no', txt, '')), size = 4) +
  geom_text(data = labdat2, aes(x = x, y = y, label = ifelse(hiv == 'no', "IRR (95% CI)", '')), size = 4) +
  ggpubr::grids() +
  scale_x_discrete(labels = function(y) stringr::str_wrap(y, width = 15))

SA
ggsave(SA,file=here::here('TBrisk/plots/ForestPlotSA.pdf'),h=8,w=12)
ggsave(SA,file=here::here('TBrisk/plots/ForestPlotSA.eps'),h=8,w=12)
ggsave(SA,file=here::here('TBrisk/plots/ForestPlotSA.png'),h=6,w=15)


m0 <- B |> 
  filter(qty == 'summary' & grepl('HIV', clinical)) |> 
  select(period=clinical, m=`Incidence Risk Ratio`, lo=mlo, hi=mhi, SE, description=clinical.g) |>
  mutate(period = ifelse(grepl('Pregnancy',period), 'P', 'PP'))

m0

m1 <- B |> 
  filter(qty == 'summary' & !grepl('HIV', clinical)) |> 
  select(period=clinical, m=`Incidence Risk Ratio`, lo=mlo, hi=mhi, SE, description=clinical.g) |>
  mutate(period = ifelse(grepl('Pregnancy',period), 'PH0', 'PPH0'))

m1

m2 <- B |> 
  filter(qty == 'summary' & grepl('HIV', clinical)) |>
  select(period=clinical, m=`Incidence Risk Ratio`, lo=mlo, hi=mhi, SE) |>
  mutate(description = ifelse(grepl('Pregnancy',period), 'Pregnancy HIV data', 'Postpartum HIV data'),
         period = ifelse(grepl('Pregnancy',period), 'PH1', 'PPH1')
  )

m2

meta_summary <- rbind(m0, m1, m2) 
meta_summary

## create directory if missing and save out
fn <- here::here('TBrisk/outdata')
if(!file.exists(fn)) dir.create(fn)

meta_summary |> fwrite(here::here('TBrisk/outdata/meta_summarySA.csv'))

