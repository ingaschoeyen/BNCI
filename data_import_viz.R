library(lavaan)
library(dagitty)
library(ggdag)
library(tidyverse)

# Setup functions -----------------------------------------------------------------------------
load_dag <- function(filepath) {
  dagtxt <- read_file(filepath)
  dag <- dagitty(dagtxt)
  ggdag(dag)
  
  return(dag)
}



# Load Data -----------------------------------------------------------------------------------
data <- read.csv("data.csv")
# not specifying column data types here, because:
# 1) we don't need to distinguish between doubles and ints except for optimization (which we could eventually do, sure)
# 2) I'll be changing to factors in the next chunk anyway

# drop NA
data <- data[complete.cases(data),]
# drop index
data <- subset(data, select=-c(X))

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

# Getting lavaan correlation matrix
M <- lavCor(data)
varTable(data)

# local chi-square tests
t <- localTests(x=dag0, sample.cov=M, sample.nobs=nrow(d))
plotLocalTestResults(t)


