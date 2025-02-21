# Meta analysis restricted to 3 studies (Rendell, Zenner, Jonsson)
library(here)

# Load data
source(here::here("TBrisk/R/MetaDataPrep.R"))

#' All analysis below is based on 3 studies: Rendell, Zenner, Jonsson
#'

glimpse(DR)
DR$pregYTBY <- round(DR$pregYTBY, 0)
DR$pregYTBN <- round(DR$pregYTBN, 0)
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

# pdf(here::here("TBrisk/plots/FEPreg.pdf"))
# forest(resFEP,
#   atransf = exp,
#   slab = paste0(
#     DR[include == "Yes"][, FA], ", ",
#     DR[include == "Yes"][, country], ", ",
#     DR[include == "Yes"][, year]
#   ),
#   xlim = c(-20, 10), at = log(c(0.05, 0.25, 1, 5)),
#   ilab = cbind(
#     DR[include == "Yes"]$pregYTBY, DR[include == "Yes"]$pregYTBN,
#     DR[include == "Yes"]$pregNTBY, DR[include == "Yes"]$pregNTBN
#   ),
#   ilab.xpos = c(-10, -7.5, -5.5, -3),
#   cex = 0.75,
#   ylim = c(-1.5, 6),
#   ilab.pos = 2,
#   xlab = "Incidence Risk Ratio",
#   mlab = "",
#   psize = 1,
#   addcred = TRUE,
#   header = FALSE
# )
# 
# text(-20, -1, pos = 4, cex = 0.75, bquote(paste("Fixed Effects Model")))
# 
# ### set font expansion factor (as in forest() above) and use bold italic
# ### font and save original settings in object 'op'
# op <- par(cex = 0.75, font = 4)
# 
# ### switch to bold font
# par(font = 2)
# 
# ### add column headings to the plot
# text(-20, 5.0, "Author, Country and Year", pos = 4)
# text(10, 5.0, "Incidence Risk Ratio [95% CI]", pos = 2)
# text(c(-10.5, -8, -6, -3.5), 5.0, c("TB+", "TB-", "TB+", "TB-"))
# text(c(-9.25, -4.75), 4.5, c("Pregnant", "Not pregnant"))
# 
# ### set par back to the original settings
# par(op)
# 
# dev.off()


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

# pdf(here::here("TBrisk/plots/REPreg.pdf"))
# png(here::here("TBrisk/plots/REPreg.png"), width = 8, height = 6, units = "in", res = 300)
# forest(resREP,
#   atransf = exp,
#   slab = paste0(
#     DR[include == "Yes"][, FA], ", ",
#     DR[include == "Yes"][, country], ", ",
#     DR[include == "Yes"][, year]
#   ),
#   xlim = c(-16, 7), at = log(c(0.05, 0.25, 1, 5)),
#   ilab = cbind(
#     DR[include == "Yes"]$pregYTBY, DR[include == "Yes"]$pregYTBN,
#     DR[include == "Yes"]$pregNTBY, DR[include == "Yes"]$pregNTBN
#   ),
#   ilab.xpos = c(-9, -7, -5, -2.5),
#   cex = 0.75,
#   ylim = c(-2, 6),
#   ilab.pos = 2,
#   xlab = "Incidence Risk Ratio",
#   mlab = "",
#   psize = 1,
#   addcred = TRUE,
#   header = FALSE
# )
# 
# text(-16, -1, pos = 4, cex = 0.75, bquote(paste(
#   "Random Effects Model for (Q = ",
#   .(formatC(resREP$QE, digits = 2, format = "f")), ", df = ", .(resREP$k - resREP$p),
#   ", p = ", .(formatC(resREP$QEp, digits = 2, format = "f")), "; ", I^2, " = ",
#   .(formatC(resREP$I2, digits = 1, format = "f")), "%)"
# )))
# 
# ### set font expansion factor (as in forest() above) and use bold italic
# ### font and save original settings in object 'op'
# op <- par(cex = 0.75, font = 4)
# 
# ### switch to bold font
# par(font = 2)
# 
# ### add column headings to the plot
# text(-16, 4.5, "Author, Country and Year", pos = 4)
# text(7, 4.5, "Incidence Risk Ratio [95% CI]", pos = 2)
# text(c(-9.5, -8.0, -6, -4.5), 4.5, c("TB+", "TB-", "TB+", "TB-"))
# text(c(-8.8, -5.0), 5.0, c("Postpartum", "Not postpartum"))
# 
# ### set par back to the original settings
# par(op)
# 
# dev.off()


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

# pdf(here::here("TBrisk/plots/FEPP.pdf"))
# forest(resFEPP,
#   atransf = exp,
#   slab = paste0(
#     DRPP[include == "Yes"][, FA], ", ",
#     DRPP[include == "Yes"][, country], ", ",
#     DRPP[include == "Yes"][, year]
#   ),
#   xlim = c(-20, 10), at = log(c(0.05, 0.25, 1, 5)),
#   ilab = cbind(
#     DRPP[include == "Yes"]$ppYTBY, DRPP[include == "Yes"]$ppYTBN,
#     DRPP[include == "Yes"]$ppNTBY, DRPP[include == "Yes"]$ppNTBN
#   ),
#   ilab.xpos = c(-10, -7.5, -5.5, -3),
#   cex = 0.75,
#   ylim = c(-1, 5),
#   ilab.pos = 2,
#   xlab = "Incidence Risk Ratio",
#   mlab = "",
#   psize = 1,
#   addcred = TRUE,
#   header = FALSE
# )
# 
# text(-20, -1, pos = 4, cex = 0.75, bquote(paste("Fixed Effects Model")))
# 
# ### set font expansion factor (as in forest() above) and use bold italic
# ### font and save original settings in object 'op'
# op <- par(cex = 0.75, font = 4)
# 
# ### switch to bold font
# par(font = 2)
# 
# ### add column headings to the plot
# text(-20, 4.0, "Author, Country and Year", pos = 4)
# text(10, 4.0, "Incidence Risk Ratio [95% CI]", pos = 2)
# text(c(-10.5, -8, -6, -3.5), 4.0, c("TB+", "TB-", "TB+", "TB-"))
# text(c(-9.25, -4.75), 3.5, c("Postpartum", "Not postpartum"))
# 
# ### set par back to the original settings
# par(op)
# 
# dev.off()

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

# png(here::here("TBrisk/plots/REPP.png"), width = 8, height = 6, units = "in", res = 300)
# forest(resREPP,
#        atransf = exp,
#        slab = paste0(
#          DRPP[include == "Yes"][, FA], ", ",
#          DRPP[include == "Yes"][, country], ", ",
#          DRPP[include == "Yes"][, year]
#        ),
#        xlim = c(-16, 7), at = log(c(0.05, 0.25, 1,2,5)),
#        ilab = cbind(
#          DRPP[include == "Yes"]$ppYTBY, DRPP[include == "Yes"]$ppYTBN,
#          DRPP[include == "Yes"]$ppNTBY, DRPP[include == "Yes"]$ppNTBN
#        ),
#        ilab.xpos = c(-9, -6.5, -4.5, -2.0),
#        cex = 0.75,
#        ylim = c(-2, 5),
#        ilab.pos = 2,
#        xlab = "Incidence Risk Ratio",
#        mlab = "",
#        psize = 1,
#        addcred = TRUE,
#        header = FALSE
# )
# 
# text(-16, -1, pos = 4, cex = 0.75, bquote(paste(
#   "Random Effects Model for (Q = ",
#   .(formatC(resREPP$QE, digits = 2, format = "f")), ", df = ", .(resREPP$k - resREPP$p),
#   ", p = ", .(formatC(resREPP$QEp, digits = 2, format = "f")), "; ", I^2, " = ",
#   .(formatC(resREPP$I2, digits = 1, format = "f")), "%)"
# )))
# 
# ### set font expansion factor (as in forest() above) and use bold italic
# ### font and save original settings in object 'op'
# op <- par(cex = 0.75, font = 4)
# 
# ### switch to bold font
# par(font = 2)
# 
# ### add column headings to the plot
# text(-16, 3.5, "Author, Country and Year", pos = 4)
# text(7, 3.5, "Incidence Risk Ratio [95% CI]", pos = 2)
# text(c(-9.3, -7.2, -5.0, -2.5), 3.5, c("TB+", "TB-", "TB+", "TB-"))
# text(c(-8.5, -4.0), 4.0, c("Postpartum", "Not postpartum"))
# 
# ### set par back to the original settings
# par(op)
# 
# dev.off()

#' Make predictions for plot data:
preg <- predict(resREP,transf = exp)
pp <- predict(resREPP,transf = exp)

DD1 <- DR |> 
  filter(include == "Yes") |> 
  select(FA, country, year, Sample, pregYTBY, pregNTBY,  pregYTBN,  pregNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "Pregnancy")
DD2 <- DRPP |> 
  filter(include == "Yes") |> 
  select(FA, country, year, Sample, pregYTBY=ppYTBY, pregNTBY=ppNTBY,  pregYTBN=ppYTBN,  pregNTBN=ppNTBN, `Incidence Risk Ratio`=m, mlo, mhi, yi, SE=si, vi) |> 
  mutate(clinical = "Postpartum")

DD <- rbind(DD1, DD2)
DD$lab <- paste0(DD$FA, ", ", DD$country, ", ", DD$year)


#' Summary data for combined forest plot:
f1 <- function(x)format(round(x,1),nsmall=1)
cnz <- c('Pregnancy',
         'Postpartum')
predz <- data.table(clinical=cnz,
                    `Incidence Risk Ratio` = c(preg$pred,pp$pred),
                    mlo = c(preg$ci.lb,pp$ci.lb),
                    mhi = c(preg$ci.ub,pp$ci.ub),
                    lab=paste0('SUMMARY (',expression(I^2),'=',
                               f1(c(resREP$I2,resREPP$I2)),'%)')
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
lbz <- unique(as.character(B$lab))
lbz2 <- lbz[1:4]
B[,lab:=factor(lab,levels=lbz2,ordered = TRUE)]
B[,clinical.g:='Pregnancy']
B[clinical=='Postpartum',clinical.g:='Postpartum']

B[,clinical.g:=factor(clinical.g,levels=unique(clinical.g))]
labdat <- B[1]
labdat[,txt:=' Weight (%)']
labdat2 <- B[1]
labdat2[,txt:='IRR (95% confidence interval)']

B$lab <- fct_rev(factor(B$lab))

#' Create publication forest plot figure:
SA <- ggplot(B,aes(lab,y=`Incidence Risk Ratio`,
                   ymin=mlo,
                   ymax=mhi,
                   col=qty)) +
  geom_point(aes(size=1/SE^2,shape=qty)) +
  geom_errorbar(aes(width=w/2)) +
  scale_y_continuous(limits = c(0,NA))+
  scale_color_manual(values=c('study'="black",'summary'="blue"))+
  scale_shape_manual(values=c('study'=22,'summary'=23))+
  xlab('') +
  ylab('Incidence Risk Ratio for tuberculosis')+
  facet_grid(clinical.g ~ .,
             scales = 'free',space='free',
             switch='x'
  )+
  coord_flip() +
  guides(size='none',color='none',shape='none')+
  theme_classic() +
  theme(
    panel.spacing = unit(2, "lines"),  # Adjust spacing between facets
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
  geom_text(aes(x = lab, y = 5, label = CI, hjust = 'right')) +
  geom_text(aes(x = lab, y = 0.0, label = wt)) +
  geom_text(data = labdat, aes(x = 4.4, y = 0.15, label = txt), size = 4) +
  geom_text(data = labdat2, aes(x = 4.4, y = 4.2, label = txt), size = 4) +
  ggpubr::grids()

SA
ggsave(SA,file=here::here('TBrisk/plots/ForestPlot.pdf'),h=13,w=12)
ggsave(SA,file=here::here('TBrisk/plots/ForestPlot.eps'),h=13,w=12)
ggsave(SA,file=here::here('TBrisk/plots/ForestPlot.png'),h=6,w=8)