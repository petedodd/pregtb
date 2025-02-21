# Random-Effects Model - Bothamley data included 
# Generated a summary measure of Bothamley paper & used it in the Pregnancy MA
# Includes studies with HIV


# setwd("U:/Documents/GitHub/pregtb")
# setwd("~/Documents/GitHub/pregTB")

source(here::here("metaanalysis", "R", "1.meta_data_prep.R"))

setkey(DR, Full_study)
# dev.off()
### decrease margins so the full space is used
par(mar=c(4,4,1,2))

### fit random-effects model (use slab argument to define study labels)
res <- rma(yi=yi,vi=vi,data=DR,
           slab=paste(FA, country, year, sep=", "), method="REML")

# pdf(file="U:/Documents/GitHub/pregtb/plots/Bothamley included (data combined first)_preg.pdf")
pdf(file=here('metaanalysis/plots/ForestPlotWithBothamleyPREG.pdf'))
### rows argument is used to specify exactly in which rows the outcomes will be plotted)
forest(res, xlim = c(-18,12), at=log(c(0.001,0.025, 0.5, 7.39, 54.6)), atransf=exp,  cex=0.75, ylim=c(-1, 28),
       rows=c(3:14,  24, 23, 21, 20, 19, 22), xlab="Incidence Risk Ratio", mlab="", psize=1, addcred = TRUE)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-18, -1, pos=4, cex=0.75, bquote(paste("RE Model for All Studies (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=4)

### add text for the subgroups
text(-18, c(15,25), pos=4, c("Bothamley",
                               "Without Bothamley"))

### switch to bold font
par(font=2)

### add column headings to the plot
text(-18,                27, "Author(s), Country and Year",  pos=4)
text(10,                 27, "Incidence Risk Ratio [95% CI]", pos=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
res.b <- rma(yi=yi,vi=vi,data=DR,
             subset=(Full_study=="No"))
res.wb <- rma(yi=yi,vi=vi,data=DR,
              subset=(Full_study=="Yes"))


### add summary polygons for the three subgroups
addpoly(res.b, row=1.5, cex=0.75, atransf=exp, mlab="", addcred = TRUE)
addpoly(res.wb, row= 17.5, cex=0.75, atransf=exp, mlab="", addcred = TRUE)


### add text with Q-value, dfs, p-value, and I^2 statistic for subgroups

text(-18, 1.5, pos=4, cex=0.75, bquote(paste("RE Model for Subgroup (Q = ",
                                             .(formatC(res.b$QE, digits=2, format="f")), ", df = ", .(res.b$k - res.b$p),
                                             ", p = ", .(formatC(res.b$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(res.b$I2, digits=1, format="f")), "%)")))
text(-18, 17.5, pos=4, cex=0.75, bquote(paste("RE Model for Subgroup (Q = ",
                                             .(formatC(res.wb$QE, digits=2, format="f")), ", df = ", .(res.wb$k - res.wb$p),
                                             ", p = ", .(formatC(res.wb$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(res.wb$I2, digits=1, format="f")), "%)")))

 dev.off()
