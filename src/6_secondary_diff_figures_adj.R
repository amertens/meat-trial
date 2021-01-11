


rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(here)
library(rcompanion)
source(here("meat_trial_functions.R"))

#---------------------------------------------------------------------------
# Unadjusted
#---------------------------------------------------------------------------

load(here("results/adjusted_secondary_results.rdata"))
df <- res_sec_adj 

df$intervention <- str_to_title(df$intervention)
df$intervention[df$intervention=="Env"] <- "Env."

df$reference <- str_to_title(df$reference)
df$reference[df$reference=="Env"] <- "Env."



df <- df %>% 
  mutate(ci.lb = est - 1.96*se,
         ci.ub = est + 1.96*se) %>%
  #arrange(reference, reference) %>%
  mutate(contrast = paste0(intervention, " (ref: ", reference,")"), 
         contrast = factor(contrast, levels = rev(unique(contrast))),
         Y = case_when(
           Y=="messenger.trust"~"Trust in messenger",
           Y=="animal.adv.eval_1"~"Animal advocates are honest",
           Y=="animal.adv.eval_2"~"Animal advocates are annoying",
           Y=="animal.adv.eval_3"~"Animal advocates are truthful",
           Y=="animal.adv.eval_4"~"Animal advocates\nare misleading",
           Y=="animal.adv.eval_5"~"test",
           Y=="animal.adv.eval_6"~"Animal advocates are accurate",
           Y=="animal.adv.eval_7"~"Animal advocates\nare similar to me",
           Y=="Beh_intentions1"~"Likelihood of replacing animal products",
           Y=="Beh_intentions2"~"Likelihood of reducing meat consumption",
           Y=="Beh_intentions3"~"Likelihood of encouraging others",
           Y=="planteating_beliefs_1"~"Link with coronavirus",
           Y=="planteating_beliefs_2"~"Link with climate change",
           Y=="planteating_beliefs_3"~"Link with pandemics",
           Y=="planteating_beliefs_4"~"Link with antibiotic resistance",
           Y=="planteating_beliefs_5"~"Not linked with health outcomes",
           Y=="planteating_beliefs_6"~"test 2",
           Y=="planteating_beliefs_7"~"Link with worker safety",
           Y=="planteating_beliefs_8"~"Support for factory farming ban",
           Y=="planteating_beliefs_9"~"Meat consumption should\nnot be the goal",
           Y=="planteating_beliefs_10"~"Personal responsibility to\nchoose plant based alternatives",
           Y=="planteating_beliefs_11"~"Others would react negatively",
           Y=="coalition.support"~"Support for plant based coalition"))

p <- ggplot(df, aes(y=est, x=contrast, color=contrast)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci.lb, ymax=ci.ub)) +
  #facet_wrap(~str_wrap(Y, width=60), ncol=3) +
  facet_wrap(~Y, ncol=3) +
  scale_color_manual(values=tableau10[rev(c(1,1,1,1,2,2,2,3,3,4))]) +
  coord_flip(ylim = c(min(df$ci.lb), max(df$ci.ub))) +
  scale_y_continuous(expand = c(0,0)) +
  geom_hline(yintercept=0) +
  theme(strip.text.x = element_text(size = 11)) +
  ylab("Mean difference") + xlab("Message group")
p

ggsave(p, file=here("figures/secondary_adj_diff.png"), width = 8.5, height=10)

