# Meta analysis excluding data from Bothamley study - post-partum

setwd("U:/Documents/GitHub/pregtb")
# setwd("~/Documents/GitHub/pregTB")

source("meta_data prep.R")


# Fixed-Effects Model - Bothamley data excluded
# Includes studies with HIV
pp_res.FE <- rma(yi=yi,vi=vi,data=DRPP, method = "FE", subset=(Full_study=="Yes"))
print(pp_res.FE)
predict(pp_res.FE, transf=exp, digits=3)

pdf('res.FE.pdf')
meta::forest(pp_res.FE,slab=paste0(DRPP[Full_study=="Yes"][,FA],', ',DRPP[Full_study=="Yes"][,country],', ', DRPP[Full_study=="Yes"][,year]),atransf = exp, showweights = T)
dev.off()

# Random-Effects Model - Bothamley data excluded
pp_res.RE <- rma(yi=yi,vi=vi,data=DRPP, subset=(Full_study=="Yes"))
print(pp_res.RE)
predict(pp_res.RE, transf=exp, digits=3)

pdf('res.RE.pdf')
meta::forest(pp_res.RE,slab=paste0(DRPP[Full_study=="Yes"][,FA],', ',DRPP[Full_study=="Yes"][,country],', ', DRPP[Full_study=="Yes"][,year]),atransf = exp, showweights = T)
dev.off()