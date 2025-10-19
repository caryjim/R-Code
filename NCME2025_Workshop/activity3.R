# Exercise 3: Bayesian Model Evaluation ----

## Load packages ----
library(lavaan)
library(blavaan)
library(tidyverse)

# Load data ----
Thurstone <- read.csv("data/thurstone.csv")

# Load Bayesian output ----
load("data/Bayesian_eval.RData")


# Learn about the model ----

## Bifactor CFA
bifactor_syntax <- '
comp =~ Sentences + Vocabulary + Sent.Completion
flue =~ First.Letters + Four.Letter.Words + Suffixes 
reas =~ Letter.Series + Pedigrees + Letter.Group

g =~ Sentences + Vocabulary + Sent.Completion + First.Letters + 
Four.Letter.Words + Suffixes + Letter.Series + Pedigrees + Letter.Group
'

## Frequentist evaluation ----
fit_bifactor <- cfa(model = bifactor_syntax, 
                    data = Thurstone, orthogonal = TRUE)

fitmeasures(fit_bifactor, fit.measures = c("chisq", "df", "pvalue", 
                                      "cfi", 
                                      "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                                      "srmr"))

# Question: Does this model fit the data well?

## Posterior Predictive Model Checking of Data Characteristics ----

## Easy functions to try: sd, var, IQR, mean, median, min, max
## Can be modify by replacing sd with other summary statistics

# extract posterior predictive distributions of SDs for each item

ppred_SD <- data.frame(t(sapply(postpred_bifactor, function(x) apply(x, 2, sd) )))
colnames(ppred_SD) <- colnames(Thurstone)

# compare posterior predictive distribution to observed SD
ppred_SD %>%
  pivot_longer(Sentences:Letter.Group) %>%
  full_join(data.frame(obs_sd = apply(Thurstone, 2, sd), # compute observed SD for each item
                       name = colnames(Thurstone))) %>%
  ggplot(aes(x = value, group = name)) +
  geom_density() +
  geom_vline(aes(xintercept = obs_sd), color = "red") +
  facet_wrap(vars(name), scales = "free_x")

# Question: Try a few summary statistics. Does the model accurately capture these summaries?

### Advanced: Posterior Predictive Model Checking of Bivariate Data Characteristics ----

## Observed correlations in sample
obs_Cor <- data.frame(obs_cor = (cor(Thurstone)[lower.tri(cor(Thurstone))]),
                      name = c(paste0("x1_x", 2:9), paste0("x2_x", 3:9),
                               paste0("x3_x", 4:9), paste0("x4_x", 5:9),
                               paste0("x5_x", 6:9), paste0("x6_x", 7:9),
                               paste0("x7_x", 8:9), paste0("x8_x", 9)))

## Bifactor
ppred_Cor <- data.frame(t(sapply(postpred_bifactor, function(x) cor(x)[lower.tri(cor(x))] )))
colnames(ppred_Cor) <- c(paste0("x1_x", 2:9), paste0("x2_x", 3:9),
                          paste0("x3_x", 4:9), paste0("x4_x", 5:9),
                          paste0("x5_x", 6:9), paste0("x6_x", 7:9),
                          paste0("x7_x", 8:9), paste0("x8_x", 9))

ppred_Cor %>%
  pivot_longer(x1_x2:x8_x9) %>%
  full_join(obs_Cor) %>%
  mutate(sim_LessThan_obs = ifelse(value < obs_cor, 1, 0)) %>%
  group_by(name) %>%
  mutate(PPP_sim_LessThan_obs = mean(sim_LessThan_obs)) %>%
  ggplot(aes(x = value, group = name)) +
  geom_density() +
  geom_vline(aes(xintercept = obs_cor, 
                 color = ifelse(PPP_sim_LessThan_obs > .95 |PPP_sim_LessThan_obs < .05, TRUE, FALSE)),
             show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw()

# Question: Does the model accurately capture correlations between variables? Think about what
# a CFA is meant to model, does what the plot shows make sense?

## Bayesian Model Evaluation: Posterior Predictive Model Checking with fit indices ----

## Bifactor
plot(ppmc_bifactor, element = "chisq")
plot(ppmc_bifactor, element = "cfi")
plot(ppmc_bifactor, element = "rmsea")
plot(ppmc_bifactor, element = "srmr")

# Question: Based on posterior predictive model checks of fit, does the model fit well?
# How does your conclusion compare to the one based on frequentist results?

# Bonus: Try an alternative model ----

## Three Correlated Factor CFA
cfa_syntax <- '
comp =~ Sentences + Vocabulary + Sent.Completion
flue =~ First.Letters + Four.Letter.Words + Suffixes 
reas =~ Letter.Series + Pedigrees + Letter.Group
'

## Frequentist evaluation ----
fit_cfa <- cfa(model = cfa_syntax, data = Thurstone)

fitmeasures(fit_cfa, fit.measures = c("chisq", "df", "pvalue", 
                                      "cfi", 
                                      "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                                      "srmr"))

## Posterior Predictive Model Checking of Data Characteristics ----

## Easy functions to try: sd, var, IQR, mean, median, min, max

## CFA
# extract posterior predictive distributions of SDs for each item
ppred2_SD <- data.frame(t(sapply(postpred_cfa, function(x) apply(x, 2, sd) )))
colnames(ppred2_SD) <- colnames(Thurstone)

# compare posterior predictive distribution to observed SD
ppred2_SD %>%
  pivot_longer(Sentences:Letter.Group) %>%
  full_join(data.frame(obs_sd = apply(Thurstone, 2, sd), # compute observed SD for each item
                       name = colnames(Thurstone))) %>%
  ggplot(aes(x = value, group = name)) +
  geom_density() +
  geom_vline(aes(xintercept = obs_sd), color = "red") +
  facet_wrap(vars(name), scales = "free_x")

### Advanced: Posterior Predictive Model Checking of Bivariate Data Characteristics ----
## CFA
ppred2_Cor <- data.frame(t(sapply(postpred_cfa, function(x) cor(x)[lower.tri(cor(x))] )))
colnames(ppred2_Cor) <- c(paste0("x1_x", 2:9), paste0("x2_x", 3:9),
                          paste0("x3_x", 4:9), paste0("x4_x", 5:9),
                          paste0("x5_x", 6:9), paste0("x6_x", 7:9),
                          paste0("x7_x", 8:9), paste0("x8_x", 9))

ppred2_Cor %>%
  pivot_longer(x1_x2:x8_x9) %>%
  full_join(obs_Cor) %>%
  mutate(sim_LessThan_obs = ifelse(value < obs_cor, 1, 0)) %>%
  group_by(name) %>%
  mutate(PPP_sim_LessThan_obs = mean(sim_LessThan_obs)) %>%
  ggplot(aes(x = value, group = name)) +
  geom_density() +
  geom_vline(aes(xintercept = obs_cor, 
                 color = ifelse(PPP_sim_LessThan_obs > .95 |PPP_sim_LessThan_obs < .05, TRUE, FALSE)),
             show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw()


## Bayesian Model Evaluation: Posterior Predictive Model Checking using fit indices ----

## CFA
plot(ppmc_cfa, element = "chisq")
plot(ppmc_cfa, element = "cfi")
plot(ppmc_cfa, element = "rmsea")
plot(ppmc_cfa, element = "srmr")
