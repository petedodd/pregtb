# Random-Effects Model - Bothamley data included - post-partum
# REM for Bothamley data & REM for other studies
# Includes studies with HIV


setwd("U:/Documents/GitHub/pregtb")
# setwd("~/Documents/GitHub/pregTB")

source("meta_data prep.R")

dev.off()
### decrease margins so the full space is used
par(mar=c(4,4,1,2))

### fit random-effects model (use slab argument to define study labels)
res <- rma(yi=yi,vi=vi,data=DRPP,
           slab=paste(FA, country, year, sep=", "))

### rows argument is used to specify exactly in which rows the outcomes will be plotted)
forest(res, xlim = c(-16,10), at=log(c(0.0001,0.02, 0.14, 7.39, 54.6)), atransf=exp,  cex=0.75, ylim=c(-1, 28),
       order=order(DRPP$Full_study),rows=c(3:16,21:24), xlab="Incidence Rate Ratio", mlab="", psize=1)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-16, -1, pos=4, cex=0.75, bquote(paste("RE Model for All Studies (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=4)

### add text for the subgroups
text(-16, c(17,25), pos=4, c("Bothamley",
                               "Without Bothamley"))

### switch to bold font
par(font=2)

### add column headings to the plot
text(-16,                27, "Author(s), Country and Year",  pos=4)
text(10,                 27, "Incidence Rate Ratio [95% CI]", pos=2)

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
res.b <- rma(yi=yi,vi=vi,data=DRPP,
             subset=(Full_study=="No"))
res.wb <- rma(yi=yi,vi=vi,data=DRPP,
              subset=(Full_study=="Yes"))


### add summary polygons for the three subgroups
addpoly(res.b, row=1.5, cex=0.75, atransf=exp, mlab="")
addpoly(res.wb, row= 18.5, cex=0.75, atransf=exp, mlab="")


### add text with Q-value, dfs, p-value, and I^2 statistic for subgroups

text(-16, 1.5, pos=4, cex=0.75, bquote(paste("RE Model for Subgroup (Q = ",
                                             .(formatC(res.b$QE, digits=2, format="f")), ", df = ", .(res.b$k - res.b$p),
                                             ", p = ", .(formatC(res.b$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(res.b$I2, digits=1, format="f")), "%)")))
text(-16, 18.5, pos=4, cex=0.75, bquote(paste("RE Model for Subgroup (Q = ",
                                             .(formatC(res.wb$QE, digits=2, format="f")), ", df = ", .(res.wb$k - res.wb$p),
                                             ", p = ", .(formatC(res.wb$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(res.wb$I2, digits=1, format="f")), "%)")))