

rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(multcomp)
library(here)

source(here("meat_trial_functions.R"))


load(here("data/analysis_dataset.Rdata"))



#-------------------------------------------------------
# Primary outcomes - unadjusted
#-------------------------------------------------------

res_Y1_unadj <- meat.glm(Yname=Yvars[1], Ws=NULL, data=d, family="gaussian")
res_Y2_unadj <- meat.glm(Yname=Yvars[2], Ws=NULL, data=d, family="gaussian")
res_Y3_unadj <- meat.glm(Yname=Yvars[3], Ws=NULL, data=d, family="gaussian")

save(Yvars, res_Y1_unadj , res_Y2_unadj , res_Y3_unadj , 
  file=here("results/unadjusted_regression_results.rdata"))



#-------------------------------------------------------
# Sensitivity analysis - ordered logistic regressions
#-------------------------------------------------------

ord_Y1<- polr_format(Yvar=Yvars[4], Ws=NULL, df=d)
ord_Y2<- polr_format(Yvar=Yvars[5], Ws=NULL, df=d)
ord_Y3<- polr_format(Yvar=Yvars[6], Ws=NULL, df=d)

save(Yvars, ord_Y1, ord_Y2, ord_Y3, 
     file=here("results/unadjusted_ordered_logistic_results.rdata"))

#-------------------------------------------------------
# Secondary outcomes - unadjusted
#-------------------------------------------------------


res_sec <- NULL
for(i in sec_Yvars){
  temp <- meat.glm(Yname=i, Ws=NULL, data=d, family="gaussian")
  res_sec <- rbind(res_sec, temp)  
}

save(res_sec, 
     file=here("results/unadjusted_secondary_results.rdata"))


#-------------------------------------------------------
#Estimate models for all outcomes - adjusted
#-------------------------------------------------------


Wdf <- d %>% subset(., select=Wvars)

res_Y1 <- meat.glm(Yname=Yvars[1], Ws=Wdf, data=d, family="gaussian")
res_Y2 <- meat.glm(Yname=Yvars[2], Ws=Wdf, data=d, family="gaussian")
res_Y3 <- meat.glm(Yname=Yvars[3], Ws=Wdf, data=d, family="gaussian")

save(Yvars, res_Y1, res_Y2, res_Y3, 
     file=here("results/adjusted_regression_results.rdata"))


#-------------------------------------------------------
# Secondary outcomes - adjusted
#-------------------------------------------------------


res_sec_adj <- NULL
for(i in sec_Yvars){
  temp <- meat.glm(Yname=i, Ws=Wdf, data=d, family="gaussian")
  res_sec_adj <- rbind(res_sec_adj, temp)  
}

save(res_sec_adj, 
     file=here("results/adjusted_secondary_results.rdata"))


#-------------------------------------------------------
# Sensitivity analysis - adjusted ordered logistic regressions
#-------------------------------------------------------

ord_Y1<- polr_format(Yvar=Yvars[4], Ws=Wdf, df=d)
ord_Y2<- polr_format(Yvar=Yvars[5], Ws=Wdf, df=d)
ord_Y3<- polr_format(Yvar=Yvars[6], Ws=Wdf, df=d)

save(Yvars, ord_Y1, ord_Y2, ord_Y3, 
     file=here("results/unadjusted_ordered_logistic_results.rdata"))
