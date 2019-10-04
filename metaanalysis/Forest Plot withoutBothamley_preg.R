# Random-Effects Model - Bothamley data included 
# REM for Bothamley data & REM for other studies
# Includes studies with HIV


setwd("U:/Documents/GitHub/pregtb")
# setwd("~/Documents/GitHub/pregTB")

source("meta_data prep.R")

dev.off()
### decrease margins so the full space is used
par(mar=c(4,4,1,2))

### fit random-effects model (use slab argument to define study labels)
res.RE <- rma(yi=yi,vi=vi,data=DR, subset=(Full_study=="Yes"),
           slab=paste(FA, country, year, sep=", "), method="REML")

pdf(file="U:/Documents/GitHub/pregtb/plots/Forest Plot withoutBothamley_preg.pdf")
### rows argument is used to specify exactly in which rows the outcomes will be plotted)
forest(res.RE, xlim = c(-9,6), at=log(c(0.05,0.37, 1, 2.72)), atransf=exp,  cex=0.75, ylim=c(-1, 8),
       xlab="Incidence Risk Ratio", mlab="", psize=1)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-9, -1, pos=4, cex=0.75, bquote(paste("RE Model for (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=4)

# ### add text for the subgroups
# text(-16, c(15,24), pos=4, c("Bothamley",
#                                "Without Bothamley"))

### switch to bold font
par(font=2)

### add column headings to the plot
text(-9,                6.5, "Author(s), Country and Year",  pos=4)
text(6,                 6.5, "Incidence Risk Ratio [95% CI]", pos=2)

### set par back to the original settings
par(op)

dev.off()