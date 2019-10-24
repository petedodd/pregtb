# Random-Effects Model - Bothamley data excluded 
# Post-partum data
# Includes studies with HIV


# setwd("U:/Documents/GitHub/pregtb")
# setwd("~/Documents/GitHub/pregTB")

source(here::here("metaanalysis","meta_data prep.R"))

dev.off()
### decrease margins so the full space is used
par(mar=c(4,4,1,2))

### fit random-effects model (use slab argument to define study labels)
res.FE <- rma(yi=yi,vi=vi,data=DRPP, subset=(Full_study=="Yes"),
           slab=paste(FA, country, year, sep=", "), method="FE")

pdf(file=here::here("metaanalysis/plots/Forest Plot withoutBothamley_pp.emp"))
### rows argument is used to specify exactly in which rows the outcomes will be plotted)
forest(res.FE, xlim = c(-20,10), at=log(c(0.02, 0.25, 1, 3, 7)), atransf=exp,
       ilab=cbind(DRPP$ppYTBY, DRPP$ppYTBN, DRPP$ppNTBY, DRPP$ppNTBN),
       ilab.xpos=c(-11,-8.5,-6.5,-4), cex=0.75, ylim=c(-2, 8),
       ilab.pos = 2,
       xlab="Incidence Risk Ratio", mlab="", psize=1, addcred = TRUE)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-20, -1, pos=4, cex=0.75, bquote(paste("Fixed Effects Model"))) 
                                            # for (Q = ",
                                            # .(formatC(res.RE$QE, digits=2, format="f")), ", df = ", .(res.RE$k - res.RE$p),
                                            # ", p = ", .(formatC(res.RE$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            # .(formatC(res.RE$I2, digits=1, format="f")), "%)")))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=4)

### switch to bold font
par(font=2)

### add column headings to the plot
text(-20,                   6.5, "Author(s), Country and Year",  pos=4)
text(10,                     6.5, "Incidence Risk Ratio [95% CI]", pos=2)
text(c(-11.5,-9,-7,-4.5),    6.5, c("TB+", "TB-", "TB+", "TB-"))
text(c(-10.25,-5.75),        7, c("Postpartum", "Notpostpartum"))

### set par back to the original settings
par(op)

### fit fixed-effects model in the three subgroups
res.RE <- rma(yi=yi,vi=vi,data=DRPP,
              subset=(Full_study=="Yes"), method = "REML")

### add summary polygons for the three subgroups
addpoly(res.RE, row=-1.5, cex=0.75, atransf=exp, mlab="", addcred = TRUE)

### add text with Q-value, dfs, p-value, and I^2 statistic for subgroups
text(-20, -1.5, pos=4, cex=0.75, bquote(paste("Random Effects Model for (Q = ",
                                              .(formatC(res.RE$QE, digits=2, format="f")), ", df = ", .(res.RE$k - res.RE$p),
                                              ", p = ", .(formatC(res.RE$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                              .(formatC(res.RE$I2, digits=1, format="f")), "%)")))
dev.off()
