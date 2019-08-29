# Meta analysis excluding data from Bothamley study

setwd("U:/Documents/GitHub/pregtb")
# setwd("~/Documents/GitHub/pregTB")

source("meta_data prep.R")


# Fixed-Effects Model - Bothamley data excluded
# Includes studies with HIV
res.FE <- rma(yi=yi,vi=vi,data=DR, method = "FE", subset=(Full_study=="Yes"))
print(res.FE)
predict(res.FE, transf=exp, digits=3)

pdf('res.FE.pdf')
meta::forest(res.FE,slab=paste0(DR[Full_study=="Yes"][,FA],', ',DR[Full_study=="Yes"][,country],', ', DR[Full_study=="Yes"][,year]),atransf = exp, showweights = T)
dev.off()

# Random-Effects Model - Bothamley data excluded
res.RE <- rma(yi=yi,vi=vi,data=DRF, subset=(Full_study=="Yes"))
print(res.RE)
predict(res.RE, transf=exp, digits=3)

pdf('res.RE.pdf')
meta::forest(res.RE,slab=paste0(DR[Full_study=="Yes"][,FA],', ',DR[Full_study=="Yes"][,country],', ', DR[Full_study=="Yes"][,year]),atransf = exp, showweights = T)
dev.off()