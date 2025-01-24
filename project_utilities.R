library(lavaan)
library(dagitty)
# library(bnlearn)
library(parallel)
library(ggdag)
library(ggplot2)
library(tidyverse)
library(lavaanExtra)
library(fastDummies)
# library(ggcorrplot)
library(MASS)
library(readr)
# library(here)

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

# automated dag adjustment:
# with inspiration from Andrew Schroeder
# removes the edge between var_a and var_b from the dag
adjust_dag <- function(dagpath, new_dag, var_a, var_b) {
  dag_lines <- readLines(dagpath)
  pattern <- paste0(var_a, " -> ", var_b)
  dag_lines <- paste0(dag_lines[!grepl(pattern, dag_lines)])
  
  print(pattern)
  
  writeLines(dag_lines, file.path("./dags/", new_dag))
  message("Created file: ", new_dag)
}

# Example usage:
# adjust_dag("./dags/dag_learned_hc_loglik-g.txt", "dag_learned_man_fix.txt", "Chol", "SEX")


plot_cov_adj_forest <- function(fits){
  for (i in seq_along(fits)){
    fit <- fits[[i]]
    glm_summary <- summary(glm_fit)
    glm_coef <- coef(glm_summary)
    glm_ci <- confint(glm_fit)

    # Create a dataframe for the forest plot
    forest_data <- data.frame(
      Predictor = rownames(glm_coef),
      OR = exp(glm_coef[, "Estimate"]),  # Exponentiate to get odds ratios
      CI_lower = exp(glm_ci[, 1]),       # Lower bound of CI
      CI_upper = exp(glm_ci[, 2]),       # Upper bound of CI
      P_value = glm_coef[, "Pr(>|z|)"]   # P-values
    )
  }
  # Remove the intercept (if you don't want to include it in the plot)
  forest_data <- forest_data %>% filter(Predictor != "(Intercept)")

  # Create a text table for the forest plot
  text_table <- cbind(
    forest_data$Predictor,
    sprintf("%.2f", forest_data$OR),
    sprintf("(%.2f to %.2f)", forest_data$CI_lower, forest_data$CI_upper),
    sprintf("%.3f", forest_data$P_value)
  )

  # Add column labels
  header <- c("Predictor", "Odds Ratio", "95% CI", "P-value")
  text_table <- rbind(header, text_table)

  # Create the forest plot
  plot <- forestplot(
    labeltext = text_table,
    mean = c(NA, forest_data$OR),      # Odds ratios
    lower = c(NA, forest_data$CI_lower), # Lower CI
    upper = c(NA, forest_data$CI_upper), # Upper CI
    is.summary = c(TRUE, rep(FALSE, nrow(forest_data))), # Header row is a summary row
    xlab = "Odds Ratio (95% CI)",
    zero = 1,                          # Reference line at OR = 1
    boxsize = 0.2,                     # Size of the boxes
    col = fpColors(box = "blue", line = "darkblue", summary = "red"),
    txt_gp = fpTxtGp(label = gpar(cex = 0.9), xlab = gpar(cex = 1.0))
  )
}

# Function to extract metrics from a glm fit and add to the dataframe
add_glm_results <- function(results_df, glm_fit, adjustment_set_name) {
  # Extract metrics from the glm fit
  aic <- AIC(glm_fit)
  bic <- BIC(glm_fit)
  loglik <- logLik(glm_fit)
  deviance <- deviance(glm_fit)
  
  # Extract the p-value of the exposure coefficient
  exposure_p_value <- coef(summary(glm_fit))["Pr(>|z|)"]
  
  # Check if the p-value meets the significance threshold
  if (exposure_p_value < 0.05) {
    # Add a new row to the dataframe
    results_df <- results_df %>%
      add_row(
        Adjustment_Set = adjustment_set_name,
        AIC = aic,
        BIC = bic,
        LogLikelihood = loglik,
        Deviance = deviance,
        P_value = exposure_p_value
      )
  }
  
  return(results_df)
}



plot_sem_forest <- function(fit) {
  # Extract parameter estimates from the lavaan fit object
  est <- parameterEstimates(fit)
  
  # Filter to keep only regression relationships (op == "~")
  reg_results <- est[est$op == "~", ]
  
  # Extract relevant columns: predictor, outcome, estimate, std.error, ci.lower, ci.upper, p-value
  forest_data <- reg_results[, c("lhs", "rhs", "est", "se", "ci.lower", "ci.upper", "pvalue")]
  
  # Filter out relationships with p-value > 0.05
  forest_data <- forest_data[forest_data$pvalue < 0.05, ]

  # Rename columns for clarity
  colnames(forest_data) <- c("Outcome", "Predictor", "Estimate", "SE", "CI_lower", "CI_upper", "P_value")
  
  # Create a text table for the forest plot
  text_table <- cbind(
    paste(forest_data$Predictor, "->", forest_data$Outcome), # Predictor -> Outcome
    sprintf("%.2f", forest_data$SE),                  # Estimate
    sprintf("%.2f", forest_data$CI_lower),
    sprintf("%.2f", forest_data$CI_upper), # Confidence Interval
    sprintf("%.2f", forest_data$Estimate), # Standard Error
    ifelse(forest_data$P_value < 0.001, "< .1%", sprintf("%.3f", forest_data$P_value)) # P-value
  )
  # Add column labels for the forest plot
  header <- c("Formula", "Std. Error", "95% CI (lower)", "95% CI (upper)", "\U1D5D", "P-value")
  
  # Combine header with the text table
  text_table <- rbind(header, text_table)
  
  # Create the forest plot
  plot <- forestplot(
    labeltext = text_table,
    mean = c(NA, forest_data$Estimate), # Add NA for the header row
    lower = c(NA, forest_data$CI_lower), # Add NA for the header row
    upper = c(NA, forest_data$CI_upper), # Add NA for the header row
    is.summary = c(TRUE, rep(FALSE, nrow(forest_data))), # Header row is a summary row
    xlab = "Regression Coefficient (95% CI)",
    zero = 0,
    boxsize = 0.2,
    col = fpColors(box = "blue", 
                  line = "lightblue", 
                  summary = "red"),
    txt_gp = fpTxtGp(label = gpar(cex = 0.9), 
                    xlab = gpar(cex = 1.0)),
    graph.pos = 5, # Position of the graph (4 = after the 4th column)
    colgap = unit(2, "mm"), # Gap between columns
    lineheight = unit(0.7, "cm"), # Height of each row
    graphwidth = unit(12, "cm") # Width of the graph area
  ) |> fp_add_lines(h_2 = gpar(lty = 2, columns = 1:7))
  return(plot)
}


