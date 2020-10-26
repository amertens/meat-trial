


rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(here)
library(rcompanion)
source(here("meat_trial_functions.R"))


#---------------------------------------------------------------------------
# Adjusted
#---------------------------------------------------------------------------

load(here("results/adjusted_regression_results.rdata"))



df <- bind_rows(
  res_Y1, 
  res_Y2, 
  res_Y3)
head(df)

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
         Y = case_when(Y=="Y1" ~ "How likely are you to try replacing animal products with plant-based alternatives (e.g. impossible burger, vegan cheese) in some of your meals in the next month?",
                       Y=="Y2" ~ "How likely are you to try to reduce your overall meat consumption in the next month?",
                       Y=="Y3" ~ "How likely are you to encourage your friends or family to try plant-based alternatives to animal products in the next month?"))

p <- ggplot(df, aes(y=est, x=contrast, color=contrast)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci.lb, ymax=ci.ub)) +
  facet_wrap(~str_wrap(Y), ncol=1) +
  scale_color_manual(values=tableau10[rev(c(1,1,1,1,2,2,2,3,3,4))]) +
  coord_flip() +
  geom_hline(yintercept=0) +
  theme(strip.text.x = element_text(size = 9)) +
  ylab("Mean difference") + xlab("Message group")
p
ggsave(p, file=here("figures/primary_adj_diff.png"), width = 8, height=8)

