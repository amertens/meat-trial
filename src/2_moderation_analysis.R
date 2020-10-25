

rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(SmartEDA)
library(here)
library(multcomp)
library(broom)
source(here("meat_trial_functions.R"))


load(here("data/analysis_dataset.Rdata"))

# Beh_intention_reason and Beh_intentio_reason2 by condition
# Also message reaction by condition
# 
# To explore additional barriers and motivations not addressed 
# in messages, could also examine Pre behav expl 
# (why they are/are not limiting animal products before receiving message)

Vvars
table(d$ave_cov_att)
table(d$pre.plant.attitudes)


resfull <- NULL
for(i in Vvars){
  print(i)
  Y1res1<-glm_mod_format(d=d,Yvar=Yvars[1], Wvars=Wvars, family="gaussian", V=i)
  Y1res2<-glm_mod_format(d=d,Yvar=Yvars[1], Wvars=Wvars, family="gaussian", V=i, control="covid", contrasts=c("welfare", "env", "health"))
  Y1res3<-glm_mod_format(d=d,Yvar=Yvars[1], Wvars=Wvars, family="gaussian", V=i, control="welfare", contrasts=c("env", "health"))
  Y1res4<-glm_mod_format(d=d,Yvar=Yvars[1], Wvars=Wvars, family="gaussian", V=i, control="env", contrasts=c("health"))
  
  Y2res1<-glm_mod_format(d=d,Yvar=Yvars[2], Wvars=Wvars, family="gaussian", V=i)
  Y2res2<-glm_mod_format(d=d,Yvar=Yvars[2], Wvars=Wvars, family="gaussian", V=i, control="covid", contrasts=c("welfare", "env", "health"))
  Y2res3<-glm_mod_format(d=d,Yvar=Yvars[2], Wvars=Wvars, family="gaussian", V=i, control="welfare", contrasts=c("env", "health"))
  Y2res4<-glm_mod_format(d=d,Yvar=Yvars[2], Wvars=Wvars, family="gaussian", V=i, control="env", contrasts=c("health"))
  
  resfull <- bind_rows(resfull, res1, res1_sub1, res1_sub2, res2, res2_sub1, res2_sub2,  res3, res3_sub1, res3_sub2)
}





save(Yvars, 
     resfull,
     file=here("results/adjusted_moderation_analysis_results.rdata"))



#Run unadjusted
d$W=rep(1, nrow(d))


resfull_unadj <- NULL
for(i in Vvars){
  print(i)
  Y1res1<-glm_mod_format(d=d,Yvar=Yvars[1], Wvars=Wvars, family="gaussian", V=i)
  Y1res2<-glm_mod_format(d=d,Yvar=Yvars[1], Wvars=Wvars, family="gaussian", V=i, control="covid", contrasts=c("welfare", "env", "health"))
  Y1res3<-glm_mod_format(d=d,Yvar=Yvars[1], Wvars=Wvars, family="gaussian", V=i, control="welfare", contrasts=c("env", "health"))
  Y1res4<-glm_mod_format(d=d,Yvar=Yvars[1], Wvars=Wvars, family="gaussian", V=i, control="env", contrasts=c("health"))
  
  Y2res1<-glm_mod_format(d=d,Yvar=Yvars[2], Wvars=Wvars, family="gaussian", V=i)
  Y2res2<-glm_mod_format(d=d,Yvar=Yvars[2], Wvars=Wvars, family="gaussian", V=i, control="covid", contrasts=c("welfare", "env", "health"))
  Y2res3<-glm_mod_format(d=d,Yvar=Yvars[2], Wvars=Wvars, family="gaussian", V=i, control="welfare", contrasts=c("env", "health"))
  Y2res4<-glm_mod_format(d=d,Yvar=Yvars[2], Wvars=Wvars, family="gaussian", V=i, control="env", contrasts=c("health"))
  
  resfull_unadj <- bind_rows(resfull_unadj, res1, res1_sub1, res1_sub2, res2, res2_sub1, res2_sub2,  res3, res3_sub1, res3_sub2)
}

res_unadj <- resfull_unadj %>% distinct( control, treatment, outcome, V, int.p)

save(Yvars, res_unadj,
     resfull_unadj, 
     file=here("results/unadjusted_moderation_analysis_results.rdata"))




