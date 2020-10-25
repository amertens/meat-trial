


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

load(here("results/unadjusted_regression_results.rdata"))

df <- bind_rows(
                res_Y1, 
                res_Y2, 
                res_Y3)
head(df)

df <- df %>% 
  mutate(ci.lb = est - 1.96*se,
         ci.ub = est + 1.96*se) %>%
  #arrange(reference, reference) %>%
  mutate(contrast = paste0(intervention, " vs. ", reference), 
         contrast = factor(contrast, levels = rev(unique(contrast))),
         Y = case_when(Y=="Y1" ~ "How likely are you to try replacing animal products with plant-based alternatives (e.g. "impossible burger," "vegan cheese") in some of your meals in the next month?",
                       Y=="Y2" ~ "How likely are you to try to reduce your overall meat consumption in the next month?",
                       Y=="Y3" ~ "How likely are you to encourage your friends or family to try plant-based alternatives to animal products in the next month?"))

p <- ggplot(df, aes(y=est, x=contrast, color=contrast)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci.lb, ymax=ci.ub)) +
  facet_wrap(~str_wrap(Y), ncol=1) +
  scale_color_manual(values=tableau10[rev(c(1,1,1,1,2,2,2,3,3,4))]) +
  coord_flip() +
  geom_hline(yintercept=0) +
  theme(strip.text.x = element_text(size = 9))
p
ggsave(p, file=here("figures/unadj/primary_unadj_diff.png"), width = 8, height=8)


#---------------------------------------------------------------------------
# Adjusted
#---------------------------------------------------------------------------

load(here("results/adjusted_regression_results.rdata"))



df <- bind_rows(
  res_Y1, 
  res_Y2, 
  res_Y3)
head(df)

df <- df %>% 
  mutate(ci.lb = est - 1.96*se,
         ci.ub = est + 1.96*se) %>%
  #arrange(reference, reference) %>%
  mutate(contrast = paste0(intervention, " vs. ", reference), 
         contrast = factor(contrast, levels = rev(unique(contrast))),
         Y = case_when(Y=="Y1" ~ "How likely are you to try replacing animal products with plant-based alternatives (e.g. "impossible burger," "vegan cheese") in some of your meals in the next month?",
                       Y=="Y2" ~ "How likely are you to try to reduce your overall meat consumption in the next month?",
                       Y=="Y3" ~ "How likely are you to encourage your friends or family to try plant-based alternatives to animal products in the next month?"))

p <- ggplot(df, aes(y=est, x=contrast, color=contrast)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci.lb, ymax=ci.ub)) +
  facet_wrap(~str_wrap(Y), ncol=1) +
  scale_color_manual(values=tableau10[rev(c(1,1,1,1,2,2,2,3,3,4))]) +
  coord_flip() +
  geom_hline(yintercept=0) +
  theme(strip.text.x = element_text(size = 9))
p
ggsave(p, file=here("figures/primary_adj_diff.png"), width = 8, height=8)


# 
# p <- ggplot(df,aes(y=est,x=agecat)) +
#   geom_errorbar(aes(color=region, ymin=lb, ymax=ub), width = 0) +
#   geom_point(aes(fill=region, color=region), size = 2) +
#   geom_text(aes(x = agecat, y = est, label = round(est)), hjust = 1.5) +
#   scale_color_manual(values=tableau11, drop=TRUE, limits = levels(df$measure)) +
#   xlab(xlabel)+
#   ylab(ylabel) +
#   
#   # add space to the left and right of points on x axis
#   # to accommodate point estimate labels
#   scale_x_discrete(expand = expand_scale(add = 1)) +
#   
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 10))  +
#   
#   theme(
#     axis.text.x = element_text(margin =
#                                  margin(t = 0, r = 0, b = 0, l = 0),
#                                size = 14)) +
#   theme(axis.title.y = element_text(size = 14)) +
#   
#   ggtitle("")
