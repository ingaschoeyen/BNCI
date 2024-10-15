library(lavaan)
library(dagitty)
library(tidyverse)
library(ggdag)
library(ggplot2)

data <- read_csv("data.csv")
# not specifying column data types here, because:
# 1) we don't need to distinguish between doubles and ints except for optimization (which we could eventually do, sure)
# 2) I'll be changing to factors in the next chunk anyway

# changing categorical variables to R factors
data <- data |>
  mutate(
    sex = factor(sex),
    cp  = factor(cp) ,
    fbs = factor(fbs),
    restecg = factor(restecg),
    exang = factor(exang),
    slope = factor(slope),
    thal  = factor(thal)
  )

# clean data

# get measures of data distribution
summary(data)

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