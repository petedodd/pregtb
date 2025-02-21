# Meta analysis including data from Bothamley study

setwd("U:/Documents/GitHub/pregtb")
# setwd("~/Documents/GitHub/pregTB")

source(here::here("metaanalysis", "R", "1.meta_data_prep.R"))

# Fixed-Effects Model - Bothamley included (each site as a study)
# Includes studies with HIV
b_res.FE <- rma(yi=yi,vi=vi,data=DR, 
                slab=paste(FA, country, year, sep=", "), method="REML")
print(b_res.FE)
predict(b_res.FE, transf=exp, digits=3)

pdf('res.FE.pdf')
meta::forest(b_res.FE,slab=paste0(DR[,FA],', ',DR[,country],', ', DR[,year]),atransf = exp, showweights = T, addcred = TRUE)
dev.off()

# Random-Effects Model - Bothamley included (each site as a study)
b_res.RE <- rma(yi=yi,vi=vi,data=DR)
print(b_res.RE)
predict(b_res.RE, transf=exp, digits=3)

pdf('res.RE.pdf')
meta::forest(b_res.RE,slab=paste0(DR[,FA],', ',DR[,country],', ', DR[,year]),atransf = exp, showweights = T)
dev.off()