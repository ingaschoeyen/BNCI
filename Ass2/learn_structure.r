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
blist <- matrix(c("Chol", "SEX", "Chol", "AGE", "STd", "AGE"), ncol=2, byrow=TRUE,
                 dimnames=list(NULL, c("from", "to")))

# set up parallel processing cluster
# cl = makeCluster(2)
# pdag <- si.hiton.pc(sdat, cluster = cl, whitelist = wlist, test="mi-g")
# pdag <- tabu(sdat, whitelist = wlist, score = "aic-g")
pdag <- hc(sdat, whitelist = wlist, blacklist = blist, score = "aic-g")
# pdag <- hc(sdat, whitelist = wlist, blacklist = blist, score = "loglik-g")
# pdag <- mmhc(sdat, whitelist = wlist)
# stopCluster(cl)

pdag <- orientPDAG(pdag)
pdag <- as.dagitty(pdag)

dag_str <- sub("pdag", "dag", pdag)
dag <- dagitty(dag_str)

dagplot <- ggdag(dag)
print(dagplot)

# save final dag plot
ggsave(filename = "learned_dag_aic.png", plot = dagplot, path = "./plots", width = 6, height = 4, units = "in", dpi = 300)


