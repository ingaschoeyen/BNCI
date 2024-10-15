library(lavaan)
library(dagitty)
library(tidyverse)
library(ggdag)

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


# make descriptive plots



# construct the DAG


# fit the model


# DO-operator analysis