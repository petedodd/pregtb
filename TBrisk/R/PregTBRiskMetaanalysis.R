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
  data = DR[include == "Yes" & HIV=='no'],
  method = "FE" # Fixed-Effects model
)

print(resFEP)
predict(resFEP, transf = exp, digits = 3)

## Random-Effects meta-analysis for TB risk during pregnancy ---- w no HIV data
resREP <- rma(
  yi = yi,
  vi = vi,
  data = DR[include == "Yes" & HIV=='no'],
  slab = paste(FA, country, year, sep = ", ")
)

print(resREP)
predict(resREP, transf = exp, digits = 3)

## Random-Effects meta-analysis for TB risk during pregnancy ---- w HIV data
resREPH <- rma(
  yi = yi,
  vi = vi,
  data = DR[include == "Yes" & Full_study=='Yes'],
  slab = paste(FA, country, year, sep = ", ")
)

print(resREPH)
predict(resREPH, transf = exp, digits = 3)


## Fixed-Effects meta-analysis for TB risk during postpartum ----
resFEPP <- rma(
  yi = yi, # effect sizes: log(IRR)
  vi = vi, # variance
  data = DRPP[include == "Yes"],
  method = "FE" # Fixed-Effects model
)

print(resFEPP)
predict(resFEPP, transf = exp, digits = 3)

# Random-Effects meta-analysis for TB risk during postpartum ---- no HIV data
resREPP <- rma(
  yi = yi,
  vi = vi,
  data = DRPP[include == "Yes" & HIV=='no'],
  slab = paste(FA, country, year, sep = ", ")
)

print(resREPP)
predict(resREPP, transf = exp, digits = 3)

# Random-Effects meta-analysis for TB risk during postpartum ---- w HIV data
resREPPH <- rma(
  yi = yi,
  vi = vi,
  data = DRPP[include == "Yes" & Full_study=='Yes'],
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
  filter(include == "Yes" & HIV=='no') |> 
  select(FA, country, HIV, year, Sample, pregYTBY, pregNTBY,  pregYTBN,  pregNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "Pregnancy")
DD1h <- DR |> 
  filter(include == "Yes" & Full_study=='Yes') |> 
  select(FA, country, HIV, year, Sample, pregYTBY, pregNTBY,  pregYTBN,  pregNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "PregnancyHIV")
DD2 <- DRPP |> 
  filter(include == "Yes" & HIV=='no') |> 
  select(FA, country, HIV, year, Sample, pregYTBY=ppYTBY, pregNTBY=ppNTBY,  pregYTBN=ppYTBN,  pregNTBN=ppNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "Postpartum")
DD2h <- DRPP |>
  filter(include == "Yes" & Full_study=='Yes') |> 
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
B[,clinical.g:='Pregnancy no HIV data']
B[clinical=='Postpartum',clinical.g:='Postpartum no HIV data']
B[clinical=='PregnancyHIV',clinical.g:='Pregnancy with HIV data']
B[clinical=='PostpartumHIV',clinical.g:='Postpartum with HIV data']

(lbz <- unique(as.character(B$clinical.g)))
lbz <- c('Pregnancy no HIV data',
         'Pregnancy with HIV data',
         'Postpartum no HIV data',
         'Postpartum with HIV data')
B[,clinical.g:=factor(clinical.g,levels=lbz)]
labdat <- B[1]
labdat[,txt:=' Weight (%)']
labdat2 <- B[1]
labdat2[,txt:='IRR (95% confidence interval)']

B$lab <- forcats::fct_rev(factor(B$lab))

#' Create publication forest plot figure:
SA <- ggplot(B,aes(lab,y=`Incidence Risk Ratio`,
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
  geom_text(data = labdat, aes(x = 5.5, y = -0.5, label = txt), size = 4) +
  geom_text(data = labdat2, aes(x = 5.5, y = 13, label = txt), size = 4) +
  ggpubr::grids()

SA
ggsave(SA,file=here::here('TBrisk/plots/ForestPlot.pdf'),h=13,w=12)
ggsave(SA,file=here::here('TBrisk/plots/ForestPlot.eps'),h=13,w=12)
ggsave(SA,file=here::here('TBrisk/plots/ForestPlot.png'),h=10,w=10)


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
  mutate(period = ifelse(grepl('Pregnancy',period), 'PH1', 'PPH1'),
         description = ifelse(grepl('Pregnancy',period), 'Pregnancy HIV data', 'Postpartum HIV data'))

m2

meta_summary <- rbind(m0, m1, m2) 
meta_summary

meta_summary |> fwrite(here::here('TBrisk/outdata/meta_summary.csv'))

