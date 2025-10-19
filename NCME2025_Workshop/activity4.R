library(ockhamSEM)

#------ Skip to line 45 --------

# Set up a temporary covariance matrix
p <- 9 # number of variables
temp_mat <- diag(p) # identity matrix
colnames(temp_mat) <- rownames(temp_mat) <- paste0("v", seq(1, p))

# Define two models using lavaan syntax
mod1 <- 'g =~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9
  f1 =~ v1 + v2 + v3
  f2 =~ v4 + v5 + v6
  f3 =~ v7 + v8 + v9
  g ~~ 0*f1
  g ~~ 0*f2
  g ~~ 0*f3
  f1 ~~ 0*f2
  f1 ~~ 0*f3 
  f2 ~~ 0*f3'

mod2 <- 'g =~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9
  f1 =~ v1 + v2 + v3 + v4 + v5
  f2 =~ v6 + v7 + v8 + v9
  g ~~ 0*f1
  g ~~ 0*f2
  f1 ~~ 0*f2'

# Fit both models using lavaan
# Adjust optimization specifications
mod1.fit <- sem(mod1, sample.cov=temp_mat, sample.nobs=500,
                control=list(iter.max=1500,bounds="pos.var"),
                optim.force.converged=T)

mod2.fit <- sem(mod2, sample.cov=temp_mat, sample.nobs=500,
                control=list(iter.max=1500,bounds="pos.var"),
                optim.force.converged=T)

# Run fitting propensity analysis
res <- run.fitprop(mod1.fit, mod2.fit, 
                   fit.measure=c("chisq","cfi","rmsea"),
                   rmethod="onion",
                   reps=1000)

# ------ Examine FP results --------

res <- readRDS("data/FPresults.RDS")

# Question: Which model has higher fitting propensity based on CFI?

# Numerical results
summary(res)

# Graphical results
plot(res,
     type="ecdf",
     whichfit="cfi",
     cutoff = .6,
     lower.tail = F) #Empirical cumulative distribution function

plot(res,
     type="euler",
     whichfit="cfi",
     cutoff=c(.6),
     lower.tail = F) #Euler plot for CFI

     