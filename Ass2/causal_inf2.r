## Causal Inference Analysis using Covariate Adjustment

# ------------------- Analysis 1 with AIC dag -------------------

# load libraries
library(tidyverse)
library(lavaan)
library(dagitty)
library(ggdag)
library(ggplot2)
library(forestplot)
library(lavaanExtra)
library(fastDummies)


# load functions
source("./project_utilities.R")

# Load the data
data <- read.csv("data_clean.csv")
data <- data[,-1]
# change column names
colnames(data) <- c("AGE", "SEX", "CP", "BPr", "Chol", "FBS", "ECGr", "HRmax", "ANGe", "STd", "STs","CA", "Thal", "HD")


# scale data
data <- data |>
  mutate(
    AGE   = scale(AGE),
    BPr   = scale(BPr),
    Chol  = scale(Chol),
    HRmax   = scale(HRmax),
    STd   = scale(STd)
  )

# bin categorical data
unordered_vars <- c("CP", "Thal", "STs")

data <- data |>
  mutate(
    CP   = as.numeric(factor(CP, ordered = TRUE))-1,
    Thal = as.numeric(factor(Thal, ordered = TRUE))-1,
    STs  = as.numeric(factor(STs, ordered = TRUE))-1,
    ECGr   = as.numeric(factor(ECGr, ordered = TRUE))-1,
    CA     = as.numeric(factor(CA, ordered = TRUE))-1,
  )

# compute polychoric correlations
M <- lavCor(data)



# load learned DAG
dag_path <- "dags/dag_learned_hc_aic-g.txt"
dag <- load_dag(dag_path)

fit_sem <- sem(toString(dag, "lavaan"), sample.cov = M, sample.nobs = nrow(data))
print(fit_sem)

# plot the SEM forest
forest_plot <- plot_sem_forest(fit_sem)
print(forest_plot)

# get the treatment and outcome variables
outcome <- "HD"
exposure <- "Chol"

# get the covariates with the different types of adjustment
covs <- adjustmentSets(dag, exposure, outcome, type="minimal") # returns empty set
covs2 <- adjustmentSets(dag, exposure, outcome, type="canonical") # returns AGE, CA
covs3 <- adjustmentSets(dag, exposure, outcome, type="all") # returns 128 sets


# baseline model
baseline <- glm(HD~Chol, data, family="binomial")

fits <- data.frame(
  formula = 'HD ~ Chol',
  AIC = AIC(baseline),
  logLik = logLik(baseline),
  OR = exp(coef(baseline)[2]),
  CI_lower = exp(confint(baseline)[2,1]),
  CI_upper = exp(confint(baseline)[2,2]),
  coef_chol = coef(baseline)[2],
  pvalue = summary(baseline)$coefficients[2,4]
)

formula_canon <- paste(outcome, "~", exposure, "+", paste(covs2[[1]], collapse = "+"))

canonical_fit <- glm(formula = formula_canon, data = data, family = "binomial")

fits <- fits %>% add_row(
  formula = formula_canon,
  AIC = AIC(canonical_fit),
  logLik = logLik(canonical_fit)[1],
  OR = exp(coef(canonical_fit)[2]),
  CI_lower = exp(confint(canonical_fit)[2,1]),
  CI_upper = exp(confint(canonical_fit)[2,2]),
  coef_chol = coef(canonical_fit)[2],
  pvalue = summary(canonical_fit)$coefficients[2,4]
)

# loop over all the sets and get the coefficients of the significant sets
for (i in seq_along(covs3)){
  covs <- covs3[[i]]
  if (length(covs) > 0){
    formula <- paste(outcome, "~", exposure, "+", paste(covs, collapse = "+"))
    fit <- glm(formula, data = data, family = "binomial") # fit the model
    coef <- coef(summary(fit)) # get the coefficients
    if (all(coef[, "Pr(>|z|)"] < 0.1)){
      print(paste(covs, collapse="+"))  
      fits <- fits %>% add_row(
        formula = formula,
        AIC = AIC(fit),
        logLik = logLik(fit)[1],
        OR = exp(coef[2]),
        CI_lower = exp(confint(fit)[2,1]),
        CI_upper = exp(confint(fit)[2,2]),
        coef_chol = coef[2,1],
        pvalue = coef[2,4]
      )
    }
  }
}

# [1] "SEX"
# [1] "BPr+SEX"
# [1] "CP+SEX"
# [1] "BPr+CP+SEX"
# [1] "ECGr+SEX"
# [1] "FBS+SEX"
# [1] "CP+FBS+SEX"

covs_final <- c("SEX", "BPr+SEX", "CP+SEX", "BPr+CP+SEX", "ECGr+SEX", "FBS+SEX", "CP+FBS+SEX")

text_table <- cbind(
  fits$formula,
  sprintf("%.2f", fits$AIC),
  sprintf("%.2f", fits$logLik),
  sprintf("%.2f", fits$coef_chol),
  sprintf("%.4f", fits$pvalue)
)

header <- c("Formula", "AIC", "log(L)", "\U1D5D (Chol)", "p (Chol)")
text_table <- rbind(header, text_table)


cov_adj_plot <- forestplot(
  text_table,
  title = "Covariate Adjustment",
  mean = c(NA, fits$OR),
  lower = c(NA,fits$CI_lower),
  upper = c(NA, fits$CI_upper),
  xlab = "HD ~ Chol Odds Ratio (95% CI)",
  zero = 1,  
  xticks = NULL,
  xlog = TRUE,
  boxsize = 0.2,  
  is.summary = c(TRUE, rep(FALSE, nrow(fits))),  
  col = fpColors(box = "blue", 
              line = "lightblue",),
  txt_gp = fpTxtGp(label = gpar(fill=c("white", "darkolivegreen3", "darkorange1", rep("white", nrow(fits)-2)), 
                  xlab = gpar(cex = 1.0))),                    
  graph.pos = 4,
  colgap = unit(2,"mm"),
  lineheight = unit(7,"mm"),
  graphwidth = unit(12,"cm"),
) |> fp_add_lines(h_2=gpar(lty=2, columns=1:6)) 
plot(cov_adj_plot)
