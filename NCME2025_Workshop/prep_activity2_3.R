# Frequentist Theory Testing

# Load packages ----
library(blavaan)
library(tidyverse)
library(psych)

# Load data ----
#Thurstone, Louis Leon and Thurstone, Thelma (Gwinn). 
# (1941) Factorial studies of intelligence. The University of Chicago Press. Chicago, Il.
# N = 213
# three factors: Verbal Comprehension, Word Fluency, and Reasoning.
data(Thurstone)

Thurstone <- MASS::mvrnorm(n = 213, mu = rep(0, 9), Sigma = Thurstone, empirical = TRUE)
write.csv(Thurstone, file = "data/thurstone.csv", row.names = FALSE)


## Specify Model ----
model_syntax <- '
g =~ Sentences + Vocabulary + Sent.Completion + First.Letters + 
Four.Letter.Words + Suffixes + Letter.Series + Pedigrees + Letter.Group

comp =~ Sentences + Vocabulary + Sent.Completion
flue =~ First.Letters + Four.Letter.Words + Suffixes 
reas =~ Letter.Series + Pedigrees + Letter.Group
'

model_syntax <- '
comp =~ Sentences + Vocabulary + Sent.Completion
flue =~ First.Letters + Four.Letter.Words + Suffixes 
reas =~ Letter.Series + Pedigrees + Letter.Group
'

fit <- lavaan::cfa(model = model_syntax, Thurstone, orthogonal = TRUE)
fit <- lavaan::cfa(model = model_syntax, Thurstone)

bfit <- bcfa(model = model_syntax, 
             Thurstone,
             #sample.cov = Thurstone, sample.nobs = 213, 
             orthogonal = TRUE, 
             save.lvs = TRUE,
             burnin = 2000, sample = 2500,
             dp = dpriors(lambda = "normal(1,5)", nu = "normal(0,10)"))

## Evaluate Model fit ----
lavaan::fitMeasures(fit, fit.measures = c("chisq", "df", "pvalue", 
                                       "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                                       "srmr"))

bfits1 <- blavFitIndices(bfit, pD = "loo")
summary(bfits1)


discFUN <- list(global = function(fit) {
  fitMeasures(fit, fit.measures = c("cfi","rmsea","srmr","chisq", "logl"))
},
std.cov.resid = function(fit) lavResiduals(fit, 
                                           type = "cor.bollen",
                                           zstat = FALSE,
                                           summary = FALSE)$cov)


ppmc_out1 <- ppmc(bfit, discFUN = discFUN, thin = 10)

summary(ppmc_out1)
plot(ppmc_out1, element = "cfi")
plot(ppmc_out1, element = "rmsea")
plot(ppmc_out1, element = "srmr")
plot(ppmc_out1, element = "chisq")
plot(ppmc_out1, element = "logl")

summary(ppmc_out1, discFUN = "std.cov.resid", 
        central.tendency = "Median", to.data.frame = TRUE,
        sort.by = "PPP_sim_LessThan_obs")

plot(ppmc_out1, discFUN = "std.cov.resid", element = c("Four.Letter.Words", "Letter.Group"))




# Posterior Predictive Samples ----
## 3-factor CFA
postpred_cfa <- blavPredict(bfit_cfa, type="ypred")
postpred_bifactor <- blavPredict(bfit_bifactor, type="ypred")


y_colM <- data.frame(t(sapply(postpred1, function(x) colMeans(x) )))
colnames(y_colM) <- colnames(Thurstone)

y_colM %>%
  pivot_longer(Sentences:Letter.Group) %>%
  full_join(data.frame(obs_mean = colMeans(Thurstone),
                       name = colnames(Thurstone))) %>%
  ggplot(aes(x = value, group = name)) +
  geom_density() +
  geom_vline(aes(xintercept = obs_mean), color = "red") +
  facet_wrap(vars(name), scales = "free_x")



y_colSD <- data.frame(t(sapply(postpred1, function(x) apply(x, 2, sd) )))
colnames(y_colSD) <- colnames(Thurstone)

y_colSD %>%
  pivot_longer(Sentences:Letter.Group) %>%
  full_join(data.frame(obs_sd = apply(Thurstone, 2, sd),
                       name = colnames(Thurstone))) %>%
  ggplot(aes(x = value, group = name)) +
  geom_density() +
  geom_vline(aes(xintercept = obs_sd), color = "red") +
  facet_wrap(vars(name), scales = "free_x")



### plot correlations
y_colCor <- data.frame(t(sapply(postpred1, function(x) cor(x)[lower.tri(cor(x))] )))
colnames(y_colCor) <- c(paste0("x1_x", 2:9), paste0("x2_x", 3:9),
                        paste0("x3_x", 4:9), paste0("x4_x", 5:9),
                        paste0("x5_x", 6:9), paste0("x6_x", 7:9),
                        paste0("x7_x", 8:9), paste0("x8_x", 9))
# obscor
y_obsCor <- data.frame(obs_cor = (cor(Thurstone)[lower.tri(cor(Thurstone))]),
                       name = c(paste0("x1_x", 2:9), paste0("x2_x", 3:9),
                                paste0("x3_x", 4:9), paste0("x4_x", 5:9),
                                paste0("x5_x", 6:9), paste0("x6_x", 7:9),
                                paste0("x7_x", 8:9), paste0("x8_x", 9)))

y_colCor %>%
  pivot_longer(x1_x2:x8_x9) %>%
  full_join(y_obsCor) %>%
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

lavResiduals(fit, type = "cor.bollen")


## 3-factor bifactor model
postpred2 <- blavPredict(bcfa_fit2, type="ypred")


y_colM <- data.frame(t(sapply(postpred2, function(x) colMeans(x) )))
colnames(y_colM) <- paste0("x", c(1,2,3,9,4,5,6,7,8))

y_colM %>%
  pivot_longer(x1:x8) %>%
  full_join(data.frame(obs_mean = colMeans(HolzingerSwineford1939[,7:15]),
                       name = colnames(HolzingerSwineford1939[,7:15]))) %>%
  ggplot(aes(x = value, group = name)) +
  geom_density() +
  geom_vline(aes(xintercept = obs_mean), color = "red") +
  facet_wrap(vars(name), scales = "free_x")



y_colSD <- data.frame(t(sapply(postpred2, function(x) apply(x, 2, sd) )))
colnames(y_colSD) <- paste0("x", c(1,2,3,9,4,5,6,7,8))

y_colSD %>%
  pivot_longer(x1:x8) %>%
  full_join(data.frame(obs_sd = apply(HolzingerSwineford1939[,7:15], 2, sd),
                       name = colnames(HolzingerSwineford1939[,7:15]))) %>%
  ggplot(aes(x = value, group = name)) +
  geom_density() +
  geom_vline(aes(xintercept = obs_sd), color = "red") +
  facet_wrap(vars(name), scales = "free_x")



### plot correlations
item_order <- c(1,2,3,9,4,5,6,7,8)

y_colCor <- data.frame(t(sapply(postpred2, function(x) cor(x)[lower.tri(cor(x))] )))
colnames(y_colCor) <- c(paste0("x1_x", item_order[2:9]), paste0("x2_x", item_order[3:9]),
                        paste0("x3_x", item_order[4:9]), paste0("x9_x", item_order[5:9]),
                        paste0("x4_x", item_order[6:9]), paste0("x5_x", item_order[7:9]),
                        paste0("x6_x", item_order[8:9]), paste0("x7_x", item_order[9]))
# obscor


y_obsCor <- data.frame(obs_cor = (cor(HolzingerSwineford1939[,c(7:9, 15,10:14)])[lower.tri(cor(HolzingerSwineford1939[,c(7:9, 15,10:14)]))]),
                       name = c(paste0("x1_x", item_order[2:9]), paste0("x2_x", item_order[3:9]),
                                paste0("x3_x", item_order[4:9]), paste0("x9_x", item_order[5:9]),
                                paste0("x4_x", item_order[6:9]), paste0("x5_x", item_order[7:9]),
                                paste0("x6_x", item_order[8:9]), paste0("x7_x", item_order[9])))

y_colCor %>%
  pivot_longer(x1_x2:x7_x8) %>%
  full_join(y_obsCor) %>%
  mutate(sim_LessThan_obs = ifelse(value < obs_cor, 1, 0)) %>%
  group_by(name) %>%
  mutate(PPP_sim_LessThan_obs = mean(sim_LessThan_obs)) %>%
  ggplot(aes(x = value, group = name)) +
  geom_density() +
  geom_vline(aes(xintercept = obs_cor, 
                 color = ifelse(PPP_sim_LessThan_obs > .95 |PPP_sim_LessThan_obs < .05, TRUE, FALSE)),
             show.legend = FALSE) +
  facet_wrap(vars(name), scales = "free_x") +
  theme_bw()

lavResiduals(cfa_fit2, type = "cor.bollen")

y_colAlpha <- data.frame((sapply(postpred2, function(x) psych::alpha(cor(x[,5:7]), warnings = FALSE)$total$raw_alpha )))
colnames(y_colAlpha) <- "post_alpha"

y_colAlpha %>%
  bind_cols(data.frame(obs_alpha = obs_alpha$total$raw_alpha)) %>%
  ggplot(aes(x = post_alpha)) +
  geom_density() +
  geom_vline(aes(xintercept = obs_alpha))



## list of draws
## (one list entry per chain):
draws <- blavInspect(bcfa_fit2, "mcmc")

## convert the list to a matrix
## (each row is a sample,
##  each column is a parameter)
draws <- do.call("rbind", draws)

plot(bcfa_fit2, plot.type = "scatter", pars = c(6,7))

save(Thurstone, bfit, bfits1, postpred1, ppmc_out1, file = "activity2.RData")


save(bfit_bifactor, bfit_cfa, 
     ppmc_bifactor, ppmc_cfa,
     postpred_bifactor, postpred_cfa,
     file = "data/Bayesian_eval.RData")
