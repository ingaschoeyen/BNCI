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

# Setup functions -----------------------------------------------------------------------------
load_dag <- function(filepath) {
  dagtxt <- read_file(filepath)
  dag <- dagitty(dagtxt)
  return(dag)
}

local_tests_utility <- function(dag, M, data) {
  # local chi-square tests
  t <- localTests(x = dag, sample.cov = M, sample.nobs = nrow(data))
  conIndT_plot <- plotLocalTestResults(t)
#   save_png(conIndT_plot, filename = "local_tests.png")
  return(t)
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

fit_then_plot <- function(dag, M, data) {
  fit <- sem(toString(g, "lavaan"), sample.cov = M, sample.nobs = nrow(data))
  plot_fit(dag, fit)
  return(fit)
}

# automated covariate adjustment glm
cov_adjust_glm <- function(dag, predictor, outcome, data, family="binomial") {
  str <- adjustmentSets(dag, predictor, outcome)
  
  vars <- unlist(str)
  vars <- c(predictor, vars)
  vars <- paste(vars, collapse = "+")
  outcome <- paste(outcome, "~")
  
  formula <- paste(outcome, vars)
  reg <- glm(as.formula(formula), data, family=family)
  
  return(reg)
}
