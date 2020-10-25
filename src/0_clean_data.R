


rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(here)

#Read in the data
d <- read.csv(here("data/Final data _10_4_2020.csv"))


#Attention check 1: animal adv eval_5= 5; Attention check 2: planteating_beliefs_6=3
dim(d)
d <- d %>% filter(animal.adv.eval_5==5 | planteating_beliefs_6==3)
dim(d)

# 1)	Examine whether messages that use the COVID-19 pandemic to highlight the risk of disease transmission from factory farms or
# use the COVID-19 pandemic to highlight the threat to worker's health created by factory farms are more effective at changing attitudes 
# and behavioral intentions towards plant-based eating, as compared to more traditional messages that highlight the environmental, health,
# or animal welfare implications of factory farmed meat consumption (Phase 1). 
# 
# IV= "condition"
table(d$condition)
d$tr <- factor(d$condition, labels = c("worker","covid","welfare","env","health"))

# Primary DVs= Beh_intentions1, Beh_intentions2, Beh_intentions3
d <- d %>%
  rename(
    Y1=Beh_intentions1,
    Y2=Beh_intentions2,
    Y3=Beh_intentions3
  ) %>%
  mutate(
    Y1_ord = factor(Y1, labels = c("Not at all likely","Somewhat likely","Moderately likely","Very likely","Extremely likely")),
    Y2_ord = factor(Y2, labels = c("Not at all likely","Somewhat likely","Moderately likely","Very likely","Extremely likely")),
    Y3_ord = factor(Y3, labels = c("Not at all likely","Somewhat likely","Moderately likely","Very likely","Extremely likely"))
  )

Yvars <-c("Y1","Y2","Y3","Y1_ord","Y2_ord","Y3_ord")

# Potential adjustment co-variates: Average of (COVID attitudes_1, COVID attitudes_2, COVID attitudes_3), pre plant attitudes,
# Pre behavioral int, Age, Gender, Race, Education, income, urbanrural, political affiliatio, pets
d$ave_cov_att <- (d$COVID.attitudes_1 + d$COVID.attitudes_2 + d$COVID.attitudes_3)/3
d$ave_cov_att[is.na(d$COVID.attitudes_1) | is.na(d$COVID.attitudes_2) | is.na(d$COVID.attitudes_3)] <- NA

summary(d$ave_cov_att)

#Clean covariates and collapse sparse categories
# table(d$Age)
# d$Age[d$Age==11] <- 10
  
table(d$Gender)
d$Gender[d$Gender>2] <- 3

table(d$Race)
d$Race[!(d$Race %in% c(2,3))] <- 99


table(d$political.affiliatio)
table(is.na(d$political.affiliatio))
d$political.affiliatio[is.na(d$political.affiliatio)] <- 3


# table(d$Education)
# d$Education[d$Education==1] <- 2
# 
# table(d$income)
# d$income[d$income==1] <- 2
# d$income[d$income==6] <- 5
# 
# table(d$urbanrural)
# table(d$political.affiliatio)

table(d$pets)
d$pets <- ifelse(d$pets==4, 0, 1)



#Set as factors
#d$Age <- factor(d$Age)
d$Gender <- factor(d$Gender)
d$Race <- factor(d$Race)
# d$Education <- factor(d$Education) #treat as continious? Along with income and education?
# d$income <- factor(d$income) #treat as continious? Along with income and education?
# d$urbanrural <- factor(d$urbanrural) #treat as continious? Along with income and education?
d$political.affiliatio <- factor(d$political.affiliatio)

Wvars <- c("ave_cov_att","pre.plant.attitudes","Pre.behavioral.int","Age","Gender",
           "Race","Education","income","urbanrural","political.affiliatio","pets")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for(i in Wvars){
  cat(i, "\n")
  print(summary(d[[i]]))
  # if(class(d[[i]])=="numeric"){
  #   d[is.na(d[[i]]),i] <- median(d[[i]], na.rm=T)
  # }else{
  #   d[is.na(d[[i]]),i] <- Mode(d[[i]])
  # }
}

# Secondary exploratory DVs= messenger trust, animal adv eval_1, animal adv eval_2, animal adv eval_3, animal adv eval_4, animal adv 
# eval_6, animal adv eval_7, planteating_beliefs_1, planteating_beliefs_2, planteating_beliefs_3, planteating_beliefs_4, 
# planteating_beliefs_5, planteating_beliefs_7, planteating_beliefs_8, planteating_beliefs_9, planteating_beliefs_10, 
# planteating_beliefs_11, coalition support

colnames(d)
sec_Yvars <-c("messenger.trust","animal.adv.eval_1","animal.adv.eval_2","animal.adv.eval_3","animal.adv.eval_4","animal.adv.eval_6",
              "animal.adv.eval_7","planteating_beliefs_1","planteating_beliefs_2","planteating_beliefs_3","planteating_beliefs_4",
              "planteating_beliefs_5","planteating_beliefs_7","planteating_beliefs_8","planteating_beliefs_9","planteating_beliefs_10",
              "planteating_beliefs_11","coalition.support")

for(i in sec_Yvars){
  cat(i, "\n")
  print(summary(d[[i]]))
}

# 2)	Examine what sub-groups various messages may be most effective for (Phase 1).
# Hypothesized Moderators:
#   1)	Average of COVID attitudes_1, COVID attitudes_2, COVID attitudes_3
# 2)	pre plant attitudes

Vvars <- c("ave_cov_att",	"pre.plant.attitudes")


#subset to needed variables
d <- d %>% subset(.,select=c("tr", "Y1", "Y2", "Y3", "Y1_ord", "Y2_ord", "Y3_ord", sec_Yvars, Vvars, Wvars))



#Save cleaned analysis data
save(d, Yvars, sec_Yvars, Wvars, Vvars, file=here("data/analysis_dataset.Rdata"))
