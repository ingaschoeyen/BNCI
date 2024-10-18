library(lavaan)
library(dagitty)
library(ggdag)
library(ggplot2)
library(tidyverse)

# Setup functions -----------------------------------------------------------------------------
load_dag <- function(filepath) {
  dagtxt <- read_file(filepath)
  dag <- dagitty(dagtxt)
  return(dag)
}


local_tests_utility <- function(dag, M, data) {
  # local chi-square tests
  t <- localTests(x = dag, sample.cov = M, sample.nobs = nrow(data))
  ttest_fig <- plotLocalTestResults(t)
  return(list(test = t, fig = ttest_fig))
}

# Load Data -----------------------------------------------------------------------------------
data <- read.csv("data.csv")
# not specifying column data types here, because:
# 1) we don't need to distinguish between doubles and ints except for optimization (which we could eventually do, sure)
# 2) I'll be changing to factors in the next chunk anyway

# drop NA
data <- data[complete.cases(data),]
# drop index
data <- data[,-1]

# changing categorical variables to R factors
# all ordered for now, subject to change
data <- data |>
  mutate(
    sex     = factor(sex, ordered=TRUE),
    cp      = factor(cp, ordered=TRUE) ,
    fbs     = factor(fbs, ordered=TRUE),
    restecg = factor(restecg, ordered=TRUE),
    exang   = factor(exang, ordered=TRUE),
    slope   = factor(slope, ordered=TRUE),
    thal    = factor(thal, ordered=TRUE),
    num     = factor(num, ordered = TRUE)
  )

print("test 4.1")

# scale appropriate variables for comparisons
data <- data |>
  mutate(
    age      = scale(age),
    trestbps = scale(trestbps),
    chol     = scale(chol),
    thalach  = scale(thalach),
    oldpeak  = scale(oldpeak),
    ca       = scale(ca)
  )


print(summary(data))
# make descriptive plots

# histograms for continuous variables

# violin plots for continuous variables

# bar plots for categorical variables

# construct the DAG

model1 <- '
# latent variables

# regressions

# residual covariances
'

# fit the model
fit <- sem(model1, data = data)

summary(fit)

# DO-operator analysis