


rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(here)
library(rcompanion)
source(here("meat_trial_functions.R"))


load(here("data/analysis_dataset.Rdata"))



Y1 <- groupwiseMean(Y1 ~ tr,
              data   = d,
              conf   = 0.95,
              percent =T,
              boot = T)

Y2 <- groupwiseMean(Y2 ~ tr,
                    data   = d,
                    conf   = 0.95,
                    percent =T,
                    boot = T)

Y3 <- groupwiseMean(Y3 ~ tr,
                    data   = d,
                    conf   = 0.95,
                    percent =T,
                    boot = T)

df <- bind_rows(
  data.frame(outcome="Y1",Y1), 
  data.frame(outcome="Y2",Y2), 
  data.frame(outcome="Y3",Y3))

df <- df %>% 
  rename(Y=Boot.mean, ci.lb=Percentile.lower, ci.ub=Percentile.upper) %>%
  subset(., select =c(outcome, tr, Y, ci.lb, ci.ub)) %>%
  mutate(
         outcome = case_when(outcome=="Y1" ~ "How likely are you to try replacing animal products with plant-based alternatives (e.g. impossible burger, vegan cheese) in some of your meals in the next month?",
                             outcome=="Y2" ~ "How likely are you to try to reduce your overall meat consumption in the next month?",
                             outcome=="Y3" ~ "How likely are you to encourage your friends or family to try plant-based alternatives to animal products in the next month?"))
df$tr <- str_to_title(df$tr)
df$tr[df$tr=="Env"] <- "Env."



p <- ggplot(df, aes(y=Y, x=tr, color=tr)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci.lb, ymax=ci.ub)) +
  facet_wrap(~outcome, ncol=1, labeller = labeller(outcome = label_wrap_gen(60))) +
  ylab("Outcome Mean") + xlab("Message group")


ggsave(p, file=here("figures/unadj/primary_means.png"), width = 6, height=8)


