# Exercise 1: Frequentist Model Evaluation ----

## Load packages ----

library(tidyverse)
library(lavaan)
library(corrplot)


## Load data ----

cormatrices <- readRDS("data/activity1_cormatrices.RDS")

## Specify model ----

cfa_model1 <- '
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6'

## Estimate model for three samples ----

cfa_fit1 <- cfa(cfa_model1, sample.cov = cormatrices[[1]], sample.nobs = 500)
cfa_fit2 <- cfa(cfa_model1, sample.cov = cormatrices[[2]], sample.nobs = 500)
cfa_fit3 <- cfa(cfa_model1, sample.cov = cormatrices[[3]], sample.nobs = 500)

## Compare Model fit ----

fitMeasures(cfa_fit1, fit.measures = c("chisq", "df", "pvalue", 
                                       "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                                       "srmr"))

fitMeasures(cfa_fit2, fit.measures = c("chisq", "df", "pvalue", 
                                       "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                                       "srmr"))

fitMeasures(cfa_fit3, fit.measures = c("chisq", "df", "pvalue", 
                                       "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                                       "srmr"))

# Question: Based on fit information, would you say this model fits these data samples?

## Look at parameter estimates ----

# Extract standardized estimates
est1 <- standardizedSolution(cfa_fit1)[,c("lhs", "op", "rhs", "est.std")]
est2 <- standardizedSolution(cfa_fit2)[,c("lhs", "op", "rhs", "est.std")]
est3 <- standardizedSolution(cfa_fit3)[,c("lhs", "op", "rhs", "est.std")]

# Combine in one data frame and compare in a table
est_all <- bind_rows(est1, est2, est3, .id = "sample")
est_all %>% pivot_wider(names_from = "sample", 
                        values_from = "est.std")

# Compare using a plot
est_all %>%
  filter(op == "=~") %>%
  ggplot(aes(x = est.std, y = rhs, 
             group = sample, color = sample)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous("Standardized Estimate", 
                     limits = c(-1, 1), 
                     breaks = seq(-1, 1, by = .5)) +
  labs(y = "Item") +
  theme_bw()

# Question: Now that you've seen the parameter estimates, do you still think that this model
# represents all three of these data samples well?

# Look at the actual data ----

# Create correlation plots, where the color and size of a 
# circle communicates a correlation's sign and magnitude
corrplot(cormatrices[[1]], method = "circle", type = "full", 
         tl.col = "black", tl.cex = 1.25, cl.cex = 1.2)
corrplot(cormatrices[[2]], method = "circle", type = "full", 
         tl.col = "black", tl.cex = 1.25, cl.cex = 1.2)
corrplot(cormatrices[[3]], method = "circle", type = "full", 
         tl.col = "black", tl.cex = 1.25, cl.cex = 1.2)
