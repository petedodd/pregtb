#' ---
#' title: Analysis of TB in pregnancy data
#' author: Pete Dodd
#' date: Feb, 2025
#' output:
#'    pdf_document:
#'      toc: true
#'      highlight: pygments
#' ---
#'
#' # Pre-amble
#' (Last updated: `r Sys.Date()`)
#'
#' This explores the data to general plausible IRRs for TB in PLHIV.
#'
#' N.B. This file should render to PDF with `rmarkdown::render(filename,output_format='pdf_document')`
#'
#' # Load libraries and data
#'
#' Relevant libraries for loading and manipulating data:

library(haven)      #for reading dta
library(data.table) #general data manipulating
library(epitools)   #IRRs etc

#' Read in the data:
D <- read_dta("TB preg modelling dataset.dta") #read data NOTE IDP not in repo due to permissions
D <- as.data.table(D)                          #convert
summary(D)                                     #look


#' # Person-time
#'
#' To calculate incidence and IRRs we need to calculate some person times.
#'
#'
#' # Person-time during pregnancy
#'
#' For those with a valid delivery date, how long where they pregnant.
#' For those who are still pregnant, use the maximum date for that patient.
D[!is.na(del_dt),PT.preg := del_dt - conception_date]      #delivered
D[is.na(del_dt),PT.preg := all_max_date - conception_date] #still pregnant
max(D$PT.preg)                                             #check

#' # Person-time post-partum
#'
#' For those delivered, time since.
#' For those still pregnany, zero.
D[!is.na(del_dt),PT.pp := all_max_date - del_dt]           #delivered
D[is.na(del_dt),PT.pp := 0.0 ]                             #still pregnant
D[,max(PT.pp)]                                             #check

#' Our definition of PP is up to 6 months, so introduce a cap in PP time.
mo6 <- difftime("2024-07-01","2024-01-01",units="days") #6 months as a time
D[,PT.pp:=pmin(PT.pp,mo6)]                              #cap
D[,max(PT.pp)]                                          #check

#'
#' # Person-time not-PPP
#'
#'
#' Assumption from birth:
#'
D[,PT.prior := ageyrs * 365.25 - (all_max_date - conception_date)]


#' # Event counts
#'
#' We will use previous TB in calculating incidence during the not-PPP period.
#' We want to count TB that is prevalent at enrolment but started treatment before conception as being incident during this period.
D[prevalent_tb==1 & previous_tb==0 & any_fst_dt_curr_tb_start<conception_date,
  previous_tb:=1] # ensure prevalence with onset prior to pregnancy => previous

#' Does this matter given numbers? Check NAs for timing.
#'
#' Classify `incident_tb` as during pregnancy or PP using the TB treatment start date:
D[,c("tb.preg","tb.pp"):=0]                                      #default as 0
D[incident_tb>0 & any_fst_dt_curr_tb_start >= del_dt, tb.pp:=1]  #post-partum
D[incident_tb>0 & any_fst_dt_curr_tb_start < del_dt, tb.preg:=1] #preg

#' # Incidences & IRRs
#'
#'
#' More than half of the women are HIV+ at enrolment.
D[,table(hiv_status_enrolment)]

#' We can replicate the above stratified by this variable.
#' First stratify the aggregated event counts and person time:
SH <- D[,.(PT.prior=as.numeric(sum(PT.prior)),
          PT.preg=as.numeric(sum(PT.preg)),
          PT.pp=as.numeric(sum(PT.pp)),
          tb.prior=sum(previous_tb),
          tb.preg = sum(tb.preg),
          tb.pp=sum(tb.pp)),
        by = hiv_status_enrolment]
SH                                         #examine

#' Then do calculations by HIV status. For PLHIV:
MP <- matrix(as.integer(SH[hiv_status_enrolment==2, #HIV+
                           .(tb.prior,tb.preg,tb.pp,PT.prior,PT.preg,PT.pp)]),nrow=3,ncol=2)
rownames(MP) <- c("not-PPP","pregnant","PP")
rateratio(MP)$measure
## rate ratio with 95% C.I. estimate     lower     upper
##                 not-PPP  1.000000        NA        NA
##                 pregnant 5.728346 2.6431459 10.940670
##                 PP       3.577219 0.8467287  9.632884


#' NOTE use the above as our main analysis.
#'
#' Then for HIV-uninfected to compare with other results
MN <- matrix(as.integer(SH[hiv_status_enrolment==1, #HIV-
                           .(tb.prior,tb.preg,tb.pp,PT.prior,PT.preg,PT.pp)]),nrow=3,ncol=2)
rownames(MN) <- c("not-PPP","pregnant","PP")
rateratio(MN)$measure
## rate ratio with 95% C.I. estimate      lower     upper
##                 not-PPP  1.000000         NA        NA
##                 pregnant 1.439228 0.06109441  6.661936
##                 PP       2.680539 0.11378739 12.407752

#' Interestingly, relative and absolute size of effect is quite consistent with other data.
#'
#' ## For PLHIV with variant duration assumption
#'
#' Assume person time 'prior' for PLHIV starts at age 15
D[,PT.prior3 := (ageyrs-15) * 365.25 - (all_max_date - conception_date)]
S3 <- D[hiv_status_enrolment==2, #HIV+ only
        .(PT.prior=as.numeric(sum(PT.prior3)), #NOTE difference here
          PT.preg=as.numeric(sum(PT.preg)),
          PT.pp=as.numeric(sum(PT.pp)),
          tb.prior=sum(previous_tb),           #NOTE no difference here
          tb.preg = sum(tb.preg),
          tb.pp=sum(tb.pp))]

#' IRR calculations (now for HIV only):
M3 <- matrix(as.integer(S3[,.(tb.prior,tb.preg,tb.pp,PT.prior,PT.preg,PT.pp)]),nrow=3,ncol=2)
rownames(M3) <- c("not-PPP","pregnant","PP")
rateratio(M3)$measure

## rate ratio with 95% C.I. estimate    lower    upper
##                 not-PPP  1.000000       NA       NA
##                 pregnant 2.711587 1.251167 5.178908
##                 PP       1.693323 0.400810 4.559850


#' construct output table:
formatter <- function(IT){
  M <- rateratio(IT)$measure
  out <- paste0(formatC(M[,1],digits=3)," (",formatC(M[,2],digits=3)," to ",formatC(M[,3],digits=3),")")
  out <- gsub("\\(  NA to   NA\\)","",out)
  out
}

formatter(M3) #test

DS <- data.table(
  group=rep(c("PLHIV","HIV-negative","PLHIV (variant assumption)"),each=3),
  state = rep(c("not-PPP", "pregnant", "PP"),3),
  `Incident TB` = c(MP[,1],MN[,1],M3[,1]),
  `person-time` = c(MP[,2],MN[,2],M3[,2]),
  `IRR (95% CI)` = c(formatter(MP),formatter(MN),formatter(M3))
)

fwrite(DS,here("TBrisk/outdata/IRRsHIV.csv"))
