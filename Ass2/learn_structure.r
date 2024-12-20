library(lavaan)
library(dagitty)
library(bnlearn)
library(parallel)
library(ggdag)
library(ggplot2)
library(tidyverse)
library(lavaanExtra)
library(fastDummies)
library(ggcorrplot)
library(MASS)

library(readr)
library(here)

source("./project_utilities.R")

# Data --------------------------------------------------------------------------
# get data
data <- read.csv("data_clean.csv")
data <- data[,-1] # drop index

# Structure Learning ------------------------------------------------------------
# the structure of our data-set is incompatible with bnlearn - it doesn't want to construct
# the edge Chol -> HD, because Chol is numeric and HD binary.
# therefore, we first compute polychoric correlations of our data, using lavaan
M <- lavCor(data)

# after that, we simulate a dataset with the same statistical properties as our dataset:
n <- ncol(data)
mu <- rep(0, n)
sdat <- as.data.frame(mvrnorm(n=n, mu=mu, Sigma=M, empirical=TRUE))

# sdat <- sdat[, c("AGE", "SEX", "Chol", "ANGe", "Thal", "HD")]

# make whitelist for focal relationship
wlist <- matrix(c("Chol", "HD"), ncol=2, byrow=TRUE,
                dimnames=list(NULL, c("from", "to")))

# blacklist for some things that don't make sense
# blist <- matrix(c("Chol", "SEX", "Chol", "AGE", "STd", "AGE"), ncol=2, byrow=TRUE,
#                 dimnames=list(NULL, c("from", "to")))

# set up parallel processing cluster
cl = makeCluster(2)
# pdag <- si.hiton.pc(sdat, cluster = cl, whitelist = wlist, test="mc-cor", alpha=0.000001)
pdag <- hc(sdat, whitelist = wlist, score = "loglik-g")
# pdag <- mmhc(sdat, whitelist = wlist)
stopCluster(cl)
plot(pdag)

dag <- as.dagitty(pdag)
