# Random-Effects Model - Bothamley data included 
# REM for Bothamley data & REM for other studies
# Includes studies with HIV


# setwd("U:/Documents/GitHub/pregtb")
# setwd("~/Documents/GitHub/pregTB")

source(here::here("metaanalysis", "R", "1.meta_data_prep.R"))

dev.off()
# while (!is.null(dev.list()))  dev.off()
# dev.set(dev.next())
### decrease margins so the full space is used
par(mar=c(4,4,1,2))

### fit random-effects model (use slab argument to define study labels)
res.FE <- rma(yi=yi,vi=vi,data=DR, subset=(Full_study=="Yes"),
           slab=paste(FA, country, year, sep=", "), method="FE")

pdf(file=here('metaanalysis/plots/ForestPlotWithoutBothamleyPREG.pdf'))
### rows argument is used to specify exactly in which rows the outcomes will be plotted)
forest(res.FE, xlim = c(-20,10), at=log(c(0.05, 0.25, 1, 5)), atransf=exp,
       ilab=cbind(DR$pregYTBY, DR$pregYTBN, DR$pregNTBY, DR$pregNTBN),
       ilab.xpos=c(-10,-7.5,-5.5,-3), cex=0.75, ylim=c(-2, 9),
       ilab.pos = 2,
       xlab="Incidence Risk Ratio", mlab="", psize=1, addcred = TRUE)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-20, -1, pos=4, cex=0.75, bquote(paste("Fixed Effects Model"))) 
# for (Q = ",
#                                             .(formatC(res.FE$QE, digits=2, format="f")), ", df = ", .(res.FE$k - res.FE$p),
#                                             ", p = ", .(formatC(res.FE$QEp, digits=2, format="f")), "; ", I^2, " = ",
#                                             .(formatC(res.FE$I2, digits=1, format="f")), "%)")))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=4)

### switch to bold font
par(font=2)

### add column headings to the plot
text(-20,                    8.5, "Author(s), Country and Year",  pos=4)
text(10,                     8.5, "Incidence Risk Ratio [95% CI]", pos=2)
text(c(-10.5,-8,-6,-3.5),    8.5, c("TB+", "TB-", "TB+", "TB-"))
text(c(-9.25,-4.75),         9.0, c("Pregnant", "Not pregnant"))

### set par back to the original settings
par(op)

### fit fixed-effects model in the three subgroups
res.RE <- rma(yi=yi,vi=vi,data=DR,
             subset=(Full_study=="Yes"), method = "REML")

### add summary polygons for the three subgroups
addpoly(res.RE, row=-1.5, cex=0.75, atransf=exp, mlab="", addcred = TRUE)

### add text with Q-value, dfs, p-value, and I^2 statistic for subgroups
text(-20, -1.5, pos=4, cex=0.75, bquote(paste("Random Effects Model for (Q = ",
                                             .(formatC(res.RE$QE, digits=2, format="f")), ", df = ", .(res.RE$k - res.RE$p),
                                             ", p = ", .(formatC(res.RE$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(res.RE$I2, digits=1, format="f")), "%)")))
dev.off()
