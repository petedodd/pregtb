# Random-Effects Model - Bothamley data included 
# Bothamley data - each site included as a study
# Includes studies with HIV
# Post- partum data


setwd("U:/Documents/GitHub/pregtb")
# setwd("~/Documents/GitHub/pregTB")

source("meta_data prep.R")

dev.off()
### decrease margins so the full space is used
par(mar=c(4,4,1,2))

### fit random-effects model (use slab argument to define study labels)
b_res.RE <- rma(yi=yi,vi=vi,data=DRPP,
           slab=paste(FA, country, year, sep=", "), method="REML")

pdf(file="U:/Documents/GitHub/pregtb/plots/Bothamley included (each site as a study)_post-partum.pdf")
### rows argument is used to specify exactly in which rows the outcomes will be plotted)
forest(b_res.RE, xlim = c(-16,10), at=log(c(0.0001,0.02, 0.14, 7.39, 54.6)), atransf=exp,  cex=0.75, ylim=c(-1, 20),
       xlab="Incidence Risk Ratio", mlab="", psize=1, addcred = TRUE)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-16, -1, pos=4, cex=0.75, bquote(paste("RE Model for (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
# op <- par(cex=0.75, font=4)

# ### add text for the subgroups
# text(-16, c(15,24), pos=4, c("Bothamley",
#                                "Without Bothamley"))

### switch to bold font
par(font=2)

### add column headings to the plot
text(-16,                19, "Author(s), Country and Year",  pos=4)
text(10,                 19, "Incidence Risk Ratio [95% CI]", pos=2)

### set par back to the original settings
par(op)

dev.off()