library(lavaan)
library(dagitty)
library(ggdag)
library(ggplot2)
library(tidyverse)
# library(fastDummies)
# library(CCP)

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

test_independences <- function(dagpath, i) {
  dag <- load_dag(dagpath)
  dag_fig <- ggdag(dag)
  ggsave(filename = paste("dag_plot", i, ".png", sep = "_"), plot = dag_fig, path = "./plots/", width = 6, height = 4, units = "in", dpi = 300)
  # daglist <- c(daglist, dag)
  ttest_results <- local_tests_utility(dag, M, data)
  return(ttest_results)
}

plot_fit <- function(dag, fit) {
  cg <- coordinates(g)
  fg <- lavaanToGraph(fit, digits=2)
  coordinates(fg) <- cg
  plot(fg, show.coefficients=TRUE)
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
    sex     = as.numeric(factor(sex, ordered=TRUE)),
    cp      = as.numeric(factor(ifelse(cp[] > 1, 1, 0))),
    fbs     = as.numeric(factor(fbs, ordered=TRUE)),
    restecg = factor(restecg, ordered=TRUE),
    exang   = as.numeric(factor(exang, ordered=TRUE)),
    slope   = as.numeric(factor(ifelse(slope[] > 1, 1, 0))),
    ca      = factor(ca, ordered = TRUE),
    num     = as.numeric(factor(num, ordered = TRUE)),
  )

data$thal <- ordered(case_match(
  data$thal,
  3 ~ 1,
  7 ~ 2,
  6 ~ 3
  ))

# scale appropriate variables for comparisons
data <- data |>
  mutate(
    age      = scale(age),
    trestbps = scale(trestbps),
    chol     = scale(chol),
    thalach  = scale(thalach),
    oldpeak  = scale(oldpeak),
)

print(summary(data))
# change var names to match DAG
# old colnames
# c <- c("Age", "Sex", "CPtype", "BPrest", "Chol", "BSfast", "ECGrest", "HRmax", "ExIndAng" , "ExIndStDep", "slope", "Thal", "ca", "HDDiag")
colnames(data) <- c("AGE", "SEX", "CP", "BPr", "Cho", "FBS", "ECr", "HRm", "ANe", "STd", "STs","CA", "Tha", "HD")


# Getting lavaan polychoric correlation matrix
M <- lavCor(data)
print(varTable(data))

# Run tests -----------------------------------------------------------------------------------
dagpaths = c("stian_dag.txt", "DAGcode_old_var_names.txt")
# dagpaths = c("stian_dag.txt")
daglist <- c()

i <- 0
for (dagpath in dagpaths) {
  daglist <- c(daglist, test_independences(dagpath, i))
  i <- i+1
}

# Fit model ------------------------------------------------------------------------------------
# fit model (after running tests!)
g <- load_dag()
fit <- sem(toString(g, "lavaan"), sample.cov = M, sample.nobs = nrow(data))
plot_fit(dag, fit)
