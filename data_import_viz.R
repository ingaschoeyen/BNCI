library(lavaan)
library(dagitty)
library(tidyverse)
library(ggdag)

# dag from file
filepath = "DAGcode_old_var_names.txt"
dagtxt <- read_file(filepath)
dag <- dagitty(dagtxt)
ggdag(dag)

data <- read.csv("data.csv")
# not specifying column data types here, because:
# 1) we don't need to distinguish between doubles and ints except for optimization (which we could eventually do, sure)
# 2) I'll be changing to factors in the next chunk anyway

# drop NA
data <- data[complete.cases(data),]
# drop index
data <- subset(data, select=-c(X))
# scale everything for comparisons
data <- apply(data, 2, scale)


# changing categorical variables to R factors
data <- data |>
  mutate(
    sex = factor(sex, ordered=TRUE),
    cp  = factor(cp, ordered=TRUE) ,
    fbs = factor(fbs, ordered=TRUE),
    restecg = factor(restecg, ordered=TRUE),
    exang = factor(exang, ordered=TRUE),
    slope = factor(slope, ordered=TRUE),
    thal  = factor(thal, ordered=TRUE)
  )

M <- lavCor(data)

varTable(data)

# local chi-square tests
t <- localTests(x=dag, sample.cov=M, sample.nobs=nrow(data))
plotLocalTestResults(t)
