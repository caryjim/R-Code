# Exercise 2: Bayesian Parameter Interpretation ----

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

# Check Convergence ----

# Using the PSRF < 1.05 check:
blavInspect(bfit_bifactor, what = "PSRF")
blavInspect(bfit_bifactor, what = "PSRF") < 1.05

# Question: did the Bayesian estimator converge?

# Look at posterior estimates ----

# Numerically:
summary(bfit_bifactor)

# Question: Using the estimate and lower and upper 
# credible interval bound: are any of these estimates "not significant"?

# Visually:
post_plot <- plot(bfit_bifactor, plot.type = "hist")
post_plot

# Question: Do any of the posterior intervals look "weird"? How does that relate to
# the posterior estimate summary above?