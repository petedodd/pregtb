
# Install package (library) if not installed
usePackage <- function(p) {
  if (!is.element(p, installed.packages()))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("metafor") # Load "metafor" package
usePackage("data.table")
usePackage("meta")
usePackage("dplyr")
## usePackage("forestplot")

setwd("U:/Documents/GitHub/pregtb")
# setwd("~/Documents/GitHub/pregTB")

D <- fread('all_datan.csv')
DPP <- fread('datapp.csv')
names(D)

# All studies pregnancy
DR <- D[,.(FA,Full_study, country, year,HIV,IRR,m,mlo,mhi)]
DR <- DR[!is.na(m)]
DR

DR[,yi:=log(m)]
DR[,si:=(log(mhi)-log(mlo))/3.92]
DR[,vi:=si^2]

# No HIV studies pregnancy
DRH <- DR[HIV!='yes']
DRF <- DR[Full_study!="No"]
DRB <- DR[Full_study!="Yes"]

# All studies postpartum
DRPP <- DPP[,.(FA, year,HIV,IRR,m,mlo,mhi)]
DRPP <- DRPP[!is.na(m)]
DRPP

DRPP[,yi:=log(m)]
DRPP[,si:=(log(mhi)-log(mlo))/3.92]
DRPP[,vi:=si^2]

# No HIV studies postpartum
DRPPH <- DRPP[HIV!='yes']

#  descriptives about the effect size estimates per year of publication
round(aggregate(yi ~ year, data=DR, FUN=function(x) c(mean=mean(x), sd=sd(x), min=min(x), max=max(x))), 3)

DR %>% group_by(FA, country) %>% summarise_if(is.numeric, funs(mean))



# cummulative meta
all_resh.cum <- rma(yi=yi,vi=vi,data=DRH, slab = paste(FA, country, year, sep = "")) 
meta2cum <- cumul(all_resh.cum, order= order(DRH$year))
forest(meta2cum,atransf = exp, showweights = T)

# sub-group analysis - without HIV
all_resh.sub <- rma(yi=yi,vi=vi,data=DR, subset = (HIV == "no"))
forest(all_resh.sub,atransf = exp, showweights = T)

# meta-regression
all_resh.mr <- rma(yi=yi,vi=vi,data=DR, mods=~IRR) 
print(all_resh.mr)
exp(c(all_resh$b, all_resh.mr$b[1]))
c(all_resh$I2, all_resh.mr$I2)

# model comparison 
anova(all_resh, all_resh.sub)


DRH$IRR <- ifelse(DRH$IRR == "yes", 1, 0) 
DRH$Full_study <- ifelse(DRH$Full_study=="yes",1,0)
DRH$year <- DRH$year - 1996

all_resh.mr <- rma(yi=yi,vi=vi,data=DRH, mods = DRH[, "Full_study"]) 
print(all_resh.mr)
exp(c(all_resh$b, all_resh.mr$b[1]))
c(all_resh$I2, all_resh.mr$I2)

all_resh.mr <- rma(yi=yi,vi=vi,data=DRH, mods = DRH[, c("year","IRR", "Full_study")]) 
print(all_resh.mr)
exp(c(all_resh$b, all_resh.mr$b[1]))
c(all_resh$I2, all_resh.mr$I2)

#  diagnostics 
#  the influence that each study has on the overall result
influence <- influence(all_resh)
plot(influence)

#  identify the effects that any one study has on the result
leave1 <- leave1out(all_resh)
which(leave1$I2 == min(leave1$I2))
sum(leave1$I2 < 30) # Number with low heterogeneity
cbind(exp(leave1$estimate), leave1$pval < 0.05)

#  investigating publication bias
funnel(all_resh)
trimfill(all_resh)
funnel(trimfill(all_resh))
value <- fsn(y = all_resh$yi, v = all_resh$vi)
value$fsnum
value$alpha # Target Significance Level
regtest(all_resh, model="lm") # p > .05(non-significant)indicates the funnel plot is NOT asymmetry(No publication bias).
# 
qqnorm(all_resh)

# Baujat plot
#  contribution of each study to the Q-test result plotted against the contribution of each study to the overall meta-analysis result
baujat(all_resh)

m.all<-metagen(yi,
                vi,
                data=DRH,
                studlab=paste(FA,",",country, ",", year),
                comb.fixed = TRUE,
                comb.random = TRUE,
                method.tau = "SJ",
                hakn = TRUE,
                prediction=TRUE,
                sm="logirr")
m.all

country.subgroup<-update.meta(m.all, 
                             byvar=country, 
                             comb.random = TRUE, 
                             comb.fixed = FALSE)
country.subgroup

PPres <- rma(yi=yi,vi=vi,data=DRPP)
print(PPres)

PPresH <- rma(yi=yi,vi=vi,data=DRPPH)
print(PPresH)

# multilevel random-effects model
full.model<-rma.mv(yi, 
                   vi, 
                   random = list(~ 1 | country, 
                                 ~ 1 | FA), 
                   tdist = TRUE, 
                   data = DRH,
                   method = "REML")

summary(full.model)

#  multivariate random-effects model 
full.model1<-rma.mv(yi, 
                   vi, 
                   random = list(~ factor(country) | FA), 
                   tdist = TRUE, 
                   data = DRH,
                   method = "REML")

summary(full.model1)

# Intraclass Correlation of the True Effects
round(full.model$sigma2[1] / sum(full.model$sigma2), 3)
round(sum(full.model$sigma2), 3)


# profile likelihood plots
par(mfrow=c(2,1))
profile(full.model, sigma2=1)
profile(full.model, sigma2=2)

model.l2.removed<-rma.mv(yi, 
                   vi, 
                   random = list(~ 1 | country, 
                                 ~ 1 | FA), 
                   tdist = TRUE, 
                   data = DRH,
                   method = "REML",
                   sigma2 = c(0,NA))

summary(model.l2.removed)
        

anova(full.model, model.l2.removed)


model.l3.removed<-rma.mv(yi, 
                         vi, 
                         random = list(~ 1 | country, 
                                       ~ 1 | FA), 
                         tdist = TRUE, 
                         data = DRH,
                         method = "REML",
                         sigma2 = c(NA,0))

summary(model.l3.removed)


anova(full.model, model.l3.removed)


# Multivariate Random-Effects Model
multi_res <- rma.mv(yi, vi, mods = ~ HIV - 1, random = ~ HIV | FA, struct="UN", data=DR, method="ML")
print(multi_res, digits=3)

# Multivariate Random-Effects Model
multi_res_mreg <- rma.mv(yi, vi, mods = ~ I(year-1996) - 1, random = ~ HIV | FA, struct="UN", data=DR, method="ML")
print(multi_res_mreg, digits=3)

# Using metagen
# a<-metagen(TE=yi, seTE = vi, data = DRH, studlab = paste(FA), comb.fixed = FALSE, comb.random = TRUE, method.tau = "SJ", hakn = TRUE, sm = "SMD")

pdf('forest1bothamley.pdf')
meta::forest(bothamley,slab=paste0(DRB[,FA],', ',DRB[,country], DRB[,year]),atransf = exp, showweights = T)
dev.off()

pdf('forest2.pdf')
meta::forest(no_bothamley,slab=paste0(DRF[,FA],', ',DRF[,year]),atransf = exp, showweights = T)
dev.off()

## funnel(res)



pdf('forest1nohiv.pdf')
forest(all_resh,slab=paste0(DRH[,FA],', ',DRH[,country],', ', DRH[,year]),atransf = exp, order = "prec")
dev.off()

## funnel(resh)

pdf('forest1nohiv.pdf')
forest(PPres,slab=paste0(DRPP[,FA],', ',DRPP[,year]),atransf = exp)
dev.off()


## funnel(resh)

pdf('forest2nohiv.pdf')
forest(PPresH,slab=paste0(DRPPH[,FA],', ',DRPPH[,year]),atransf = exp)
dev.off()

## funnel(resh)
