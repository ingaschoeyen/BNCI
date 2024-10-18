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
data <- data[-1, ]

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
# change var names to match DAG
colnames(data) <- c("ID", "AGE", "SEX", "CP", "BPr", "Cho", "FBS", "ECr", "HRm", "ANe", "STd", "STs", "Tha", "CA","HD")

# Getting lavaan correlation matrix
M <- lavCor(data)
print(varTable(data))

# Run tests -----------------------------------------------------------------------------------
dagpaths = c("stian_dag.txt", "DAGcode_old_var_names.txt")
daglist <- c()
i <- 1

for (dagpath in dagpaths) {
  dag <- load_dag(dagpath)
  dag_fig <- ggdag(dag)
  ggsave(filename = paste("dag_plot", i, ".png", sep = "_"), plot = dag_fig, path = "./plots/", width = 6, height = 4, units = "in", dpi = 300)
  daglist <- c(daglist, dag)
  ttest_results <- local_tests_utility(dag, M, data)
  i <- i + 1
}
