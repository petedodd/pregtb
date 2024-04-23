
# Meta analysis excluding data from Bothamley study
library(here)

source(here("metaanalysis/R/1.meta_data_prep.R"))


# Fixed-Effects Model - Bothamley data excluded
# Includes studies with HIV
res.FE <- rma(yi=yi,vi=vi,data=DR, method = "FE", subset=(Full_study=="Yes"))
print(res.FE)
predict(res.FE, transf=exp, digits=3)

pdf('res.FE.pdf')
meta::forest(res.FE,slab=paste0(DR[Full_study=="Yes"][,FA],', ',DR[Full_study=="Yes"][,country],', ', DR[Full_study=="Yes"][,year]),atransf = exp, showweights = T)
dev.off()

# Random-Effects Model - Bothamley data excluded
res.RE <- rma(yi=yi,vi=vi,data=DRF, subset=(Full_study=="Yes"), slab=paste(FA, country, year, sep=", "))
print(res.RE)
predict(res.RE, transf=exp, digits=3)

pdf('res.RE.pdf')
meta::forest(res.RE,slab=paste0(DR[Full_study=="Yes"][,FA],', ',DR[Full_study=="Yes"][,country],', ', DR[Full_study=="Yes"][,year]),atransf = exp, showweights = T , 
             xlab="Incidence Risk Ratio", mlab="mmm")
dev.off()

forest(res.RE,
       
       xlim = c(-16,10),
       rightlabs = c("IRR","95% CI","weight"),
       leftlabs = c("FA", "country","year"),
     
       pooled.totals = FALSE,
       smlab = "",
       text.random = "Overall effect",
       print.tau2 = FALSE,
       col.diamond = "blue",
       col.diamond.lines = "black",
       col.predict = "black",
       print.I2.ci = TRUE,
       digits.sd = 2
)

forest(res.RE,
       layout = "JAMA",
       text.predict = "95% PI",
       col.predict = "black",
       colgap.forest.left = unit(15,"mm"))
forest(res.RE,
       layout = "RevMan5",
       digits.sd = 2)
