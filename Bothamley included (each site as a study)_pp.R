# Meta analysis including data from Bothamley study - post-partum

setwd("U:/Documents/GitHub/pregtb")
# setwd("~/Documents/GitHub/pregTB")

source("meta_data prep.R")

# Fixed-Effects Model - Bothamley included (each site as a study)
# Includes studies with HIV
bpp_res.FE <- rma(yi=yi,vi=vi,data=DRPP, method = "FE")
print(bpp_res.FE)
predict(bpp_res.FE, transf=exp, digits=3)

pdf('res.FE.pdf')
meta::forest(bpp_res.FE,slab=paste0(DRPP[,FA],', ',DRPP[,country],', ', DRPP[,year]),atransf = exp, showweights = T)
dev.off()

# Random-Effects Model - Bothamley included (each site as a study)
b_res.RE <- rma(yi=yi,vi=vi,data=DRPP)
print(bpp_res.FE)
predict(bpp_res.FE, transf=exp, digits=3)

pdf('res.RE.pdf')
meta::forest(bpp_res.FE,slab=paste0(DRPP[,FA],', ',DRPP[,country],', ', DRPP[,year]),atransf = exp, showweights = T)
dev.off()