## syntax testing
BL <- births[rep(1:nrow(births),each=5),]
BL$yearnow <- as.numeric(BL$from) + rep(0:4,nrow(births))
head(as.data.frame(BL))
