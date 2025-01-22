## Causal Inference Analysis using Covariate Adjustment
library(tidyverse)
library(lavaan)
library(dagitty)
library(ggdag)
library(ggplot2)
# functions

covAdjustment <- function(data, treatment, outcome, covariates){
  # Fit the model without covariate adjustment
  model <- paste(treatment, "~", outcome)
  fit <- sem(model, data = data)  
  
  est1 <- parameterEstimates(fit)
  est1 <- est1[est1$op == "~",]

  print(est1)
  # Fit the model with covariate adjustment
  model <- paste(treatment, "~", outcome, "+", paste(covariates, collapse = "+"))
  fit <- sem(model, data = data)
  
  # Estimate the treatment effect
  est2 <- parameterEstimates(fit)
  est2 <- est2[est2$op == "~",]
  print(est2)
  # Return the treatment effect
  return(est1, est2)
}

# Load the data

data <- read.csv("../data.csv")


data <- data[,-1]
# change column names
colnames(data) <- c("AGE", "SEX", "CP", "BPr", "Chol", "FBS", "ECGr", "HRmax", "ANGe", "STd", "STs","CA", "Thal", "HD")


# scale data
data <- data |>
  mutate(
    AGE   = scale(AGE),
    BPr   = scale(BPr),
    Chol  = scale(Chol),
    HRm   = scale(HRm),
    STd   = scale(STd)
  )

# load learned DAG

dag_path <- "/../dags/dag_learned_hc_loglik-g.txt"
dag_txt <- readLines(dag_path)
dag <- dagitty::readDAG(dag_txt)

# get the treatment and outcome variables
outcome <- "HD"
predictor <- c("Chol")

# get the covariates
covariates <- adjustmentSets(dag, exposure = predictor, outcome = outcome)

# perform covariate adjustment
results <- covAdjustment(data, predictor, outcome, covariates)

est_no_covadj <- results$est1
est_cog_adj <- results$est2

print(est_no_covadj)
print(est_cog_adj)

