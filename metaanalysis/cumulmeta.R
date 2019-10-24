library(here)
# cummulative metaanalysis
source(here::here("metaanalysis","meta_data prep.R"))

### fit random-effects model (use slab argument to define study labels)
res.RE <- rma.uni(yi=yi,vi=vi,data=DRF, 
              slab=paste(FA, country, year, sep=", "))

res.RE_b <- rma(yi=yi,vi=vi,data=DR, 
              slab=paste(FA, country, year, sep=", "))

### cumulative meta-analysis (in the order of publication year)
tmp <- cumul(res.RE,  order=order(DRF$year))
tmp_b <- cumul(res.RE_b,  order=order(DR$year))

dev.off()
### cumulative forest plot
forest(tmp, xlim = c(-18,10), at=log(c(0.001,0.025, 0.5, 7.39, 54.6)), atransf=exp, digits=c(2,3), cex=0.75, addcred = T)

### switch to bold font
par(cex=0.75, font=2)

### add column headings to the plot
text(-18, 6.5, "Author(s) and Year",  pos=4)
text( 8, 6.5, "Risk Ratio [95% CI]", pos=2)

dev.off()
### cumulative forest plot_b
forest(tmp_b, xlim = c(-18,10), at=log(c(0.001,0.025, 0.5, 7.39, 54.6)), atransf=exp, digits=c(2,3), cex=0.75)

### switch to bold font
par(cex=0.75, font=2)

### add column headings to the plot
text(-18, 19, "Author(s) and Year",  pos=4)
text( 8, 19, "Risk Ratio [95% CI]", pos=2)
dev.off()
##############################################alternatively using the meta package #########################################
# pregnancy
m.all<-metagen(yi,
                vi,
                data=DRF,
                studlab=paste(FA,",",country, ",", year),
                comb.fixed = TRUE,
                comb.random = TRUE,
                method.tau = "SJ",
                hakn = TRUE,
                prediction=TRUE,
                sm="IRR")
                
m.cum <- metacum(m.all)

forest(m.all, atransf=exp, leftcols = "studlab", layout = "meta", comb.fixed = TRUE
       ,label.right = "Increased risk of TB", col.label.right = "red",
       rightcols = cbind(DR$pregYTBY, DR$pregYTBN, DR$pregNTBY, DR$pregNTBN),
       label.left = "Reduced risk of TB", col.label.left = "green",
       prediction = TRUE)

forest(m.cum, atransf=exp, leftcols = "studlab", layout = "meta", comb.random = TRUE
       ,label.right = "Increased risk of TB", col.label.right = "red",
       label.left = "Reduced risk of TB", col.label.left = "green",
       prediction = TRUE)

forest(res, xlim=c(-4,2), at=log(c(0.125, 0.25, 0.5, 1, 2)), 
       atransf=exp,  cex=0.75)

m.all_b<-metagen(yi,
               vi,
               data=DR,
               studlab=paste(FA,",",country, ",", year),
               comb.fixed = TRUE,
               comb.random = TRUE,
               method.tau = "SJ",
               hakn = TRUE,
               prediction=TRUE,
               sm="IRR")

m.cum_b <- metacum(m.all_b)

forest(m.all_b, atransf=exp, leftcols = "studlab", layout = "meta", comb.fixed = TRUE
       ,label.right = "Increased risk of TB", col.label.right = "red",
       label.left = "Reduced risk of TB", col.label.left = "green",
       prediction = TRUE)

forest(m.cum_b, atransf=exp, leftcols = "studlab", layout = "meta", comb.random = TRUE
       ,label.right = "Increased risk of TB", col.label.right = "red",
       label.left = "Reduced risk of TB", col.label.left = "green",
       prediction = TRUE)

#postpartum
### fit random-effects model (use slab argument to define study labels)
res.RE.pp <- rma.uni(yi=yi,vi=vi,data=DRPP, subset=(Full_study=="Yes"),
                  slab=paste(FA, country, year, sep=", "))

res.RE_b.pp <- rma(yi=yi,vi=vi,data=DRPP, 
                slab=paste(FA, country, year, sep=", "))

### cumulative meta-analysis (in the order of publication year)
tmp <- cumul(res.RE.pp,  order=order(DRPP$year[DRPP$Full_study=="Yes"]))
tmp_b <- cumul(res.RE_b.pp ,  order=order(DRPP$year))

dev.off()
### cumulative forest plot
forest(tmp, xlim = c(-18,10), at=log(c(0.001,0.025, 0.5, 7.39, 54.6)), atransf=exp, digits=c(2,3), cex=0.75, addcred = T)

### switch to bold font
par(cex=0.75, font=2)

### add column headings to the plot
text(-18, 6.5, "Author(s) and Year",  pos=4)
text( 8, 6.5, "Risk Ratio [95% CI]", pos=2)

dev.off()
### cumulative forest plot_b
forest(tmp_b, xlim = c(-18,10), at=log(c(0.001,0.025, 0.5, 7.39, 54.6)), atransf=exp, digits=c(2,3), cex=0.75)

### switch to bold font
par(cex=0.75, font=2)

### add column headings to the plot
text(-18, 19, "Author(s) and Year",  pos=4)
text( 8, 19, "Risk Ratio [95% CI]", pos=2)
dev.off()
m.all.pp<-metagen(yi,
               vi,
               data=DRPP, subset=(Full_study=="Yes"),
               studlab=paste(FA,",",country, ",", year),
               comb.fixed = TRUE,
               comb.random = TRUE,
               method.tau = "SJ",
               hakn = TRUE,
               prediction=TRUE,
               sm="IRR")

m.cum.pp <- metacum(m.all.pp)

forest(m.all.pp, atransf=exp, leftcols = "studlab", layout = "meta", comb.fixed = TRUE
       ,label.right = "Increased risk of TB", col.label.right = "red",
       label.left = "Reduced risk of TB", col.label.left = "green",
       prediction = TRUE)

forest(m.cum.pp, atransf=exp, leftcols = "studlab", layout = "meta", comb.random = TRUE
       ,label.right = "Increased risk of TB", col.label.right = "red",
       label.left = "Reduced risk of TB", col.label.left = "green",
       prediction = TRUE)

m.all_b<-metagen(yi,
                 vi,
                 data=DR,
                 studlab=paste(FA,",",country, ",", year),
                 comb.fixed = TRUE,
                 comb.random = TRUE,
                 method.tau = "SJ",
                 hakn = TRUE,
                 prediction=TRUE,
                 sm="IRR")

m.cum_b <- metacum(m.all_b)

forest(m.all_b, atransf=exp, leftcols = "studlab", layout = "meta", comb.fixed = TRUE
       ,label.right = "Increased risk of TB", col.label.right = "red",
       label.left = "Reduced risk of TB", col.label.left = "green",
       prediction = TRUE)

forest(m.cum_b, atransf=exp, leftcols = "studlab", layout = "meta", comb.random = TRUE
       ,label.right = "Increased risk of TB", col.label.right = "red",
       label.left = "Reduced risk of TB", col.label.left = "green",
       prediction = TRUE)


library(metaviz)
viz_forest(x=DRF[,c("yi", "vi")], x_trans_function = exp, summary_label = "Summary effect", xlab = "Incidence Rate Ratio", variant = "thick"
           ,study_labels = DRF[, "FA"], type = "cumulative"
           ,annotate_CI = T)


## Meta-analysis using metabin()
preg_meta <-
        metabin(data        = DR,  # Data
                event.e     = pregYTBY,  # Events in exposed
                n.e         = pregYTBN,     # Number in exposed
                event.c     = pregNTBY,  # Events in control
                n.c         = pregNTBN,     # Number in control
                sm          = "RR",     # Summary measure
                studlab     = paste(FA,",",country, ",", year), # Study labels
                comb.fixed  = TRUE,     # Fixed effects yes
                comb.random = TRUE     # Random effects no
        )

## Show results
preg_meta

forest.meta(preg_meta)
