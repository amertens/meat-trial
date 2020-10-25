


rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(here)
library(rcompanion)
source(here("meat_trial_functions.R"))


load(here("data/analysis_dataset.Rdata"))

full_res <- NULL
for(i in sec_Yvars){
  form <- paste0(i, " ~ tr")
  res <- groupwiseMean(as.formula(form),
                       data   = d,
                       conf   = 0.95,
                       percent =T,
                       boot = T)
  full_res <- bind_rows(full_res,
                  data.frame(outcome=i,res))  
}


df <- full_res %>% 
  rename(Y=Mean, ci.lb=Percentile.lower, ci.ub=Percentile.upper) %>%
  subset(., select =c(outcome, tr, Y, ci.lb, ci.ub)) %>%
  mutate(outcome = case_when(
           outcome=="messenger.trust"~"Trust in messenger",
           outcome=="animal.adv.eval_1"~"Messenger is honest",
           outcome=="animal.adv.eval_2"~"Messenger is annoying",
           outcome=="animal.adv.eval_3"~"Messenger is truthful",
           outcome=="animal.adv.eval_4"~"Messenger is misleading",
           outcome=="animal.adv.eval_5"~"test",
           outcome=="animal.adv.eval_6"~"Messenger is accurate",
           outcome=="animal.adv.eval_7"~"Messenger is similar to me",
           outcome=="Beh_intentions1"~"Likelihood of replacing animal products",
           outcome=="Beh_intentions2"~"Likelihood of reducing meat consumption",
           outcome=="Beh_intentions3"~"Likelihood of encouraging others",
           outcome=="planteating_beliefs_1"~"Link with coronavirus",
           outcome=="planteating_beliefs_2"~"Link with climate change",
           outcome=="planteating_beliefs_3"~"Link with pandemics",
           outcome=="planteating_beliefs_4"~"Link with antibiotic resistance",
           outcome=="planteating_beliefs_5"~"Not linked with health outcomes",
           outcome=="planteating_beliefs_6"~"test 2",
           outcome=="planteating_beliefs_7"~"Link with worker safety",
           outcome=="planteating_beliefs_8"~"Support for factory farming ban",
           outcome=="planteating_beliefs_9"~"Meat consumption should not be goal",
           outcome=="planteating_beliefs_10"~"Personal responsibility to choose plant based ",
           outcome=="planteating_beliefs_11"~"Others would react negatively",
           outcome=="coalition.support"~"Support for plant based coalition"))

df$tr <- str_to_title(df$tr)
df$tr[df$tr=="Env"] <- "Env."

p <- ggplot(df, aes(y=Y, x=tr, color=tr)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci.lb, ymax=ci.ub)) +
  facet_wrap(~outcome, labeller = labeller(outcome = label_wrap_gen(20))) +
  ylab("Outcome Mean") + xlab("Message group")+ 
  theme(axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=0.5))


ggsave(p, file=here("figures/unadj/secondary_means.png"), width = 12, height=8)


p <- ggplot(df, aes(y=Y, x=tr, color=tr)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci.lb, ymax=ci.ub)) +
  facet_wrap(~outcome, scales="free", labeller = labeller(outcome = label_wrap_gen(20))) +
  ylab("Outcome Mean") + xlab("Message group") + 
  theme(axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=0.5))


ggsave(p, file=here("figures/unadj/secondary_means_alt.png"), width = 12, height=8)

