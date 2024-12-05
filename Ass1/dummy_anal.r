library(lavaan)
library(dagitty)
library(ggdag)
library(ggplot2)
library(tidyverse)
library(lavaanExtra)
library(fastDummies)
library(ggcorrplot)
library(nnet)

data_path <- "data_clean.csv"

# Load Data -----------------------------------------------------------------------------------

data <- read.csv(data_path)

data <- data[complete.cases(data),]
data <- data[,-1]

colnames(data) <- c("AGE", "SEX", "CP", "BPr", "Chol", "FBS", "ECGr", "HRmax", "ANGe", "STd", "STs","CA", "Thal", "HD")

print(head(data))
# Scale continuous variables
data <- data |>
  mutate(
    AGE   = scale(AGE),
    BPr   = scale(BPr),
    Chol  = scale(Chol),
    HRmax = scale(HRmax),
    STd   = scale(STd)
  )

# encode unordered categorical as dummy vars

unordered_vars <- c("CP", "Thal", "STs")

data <- data |>
  fastDummies::dummy_cols(select_columns = unordered_vars, 
                          remove_selected_columns = TRUE, 
                          remove_most_frequent = TRUE) |>
  mutate(
    ECGr   = as.numeric(factor(ECGr, ordered = TRUE))-1,
    CA     = as.numeric(factor(CA, ordered = TRUE))-1,
    HD     = as.numeric(factor(HD, ordered = TRUE))
  )

print(head(data))

print(varTable(data))

# Getting lavaan polychoric correlation matrix
M <- lavCor(data)
p.mat <- cor_pmat(data)

p.mat.adjusted <- p.adjust(p.mat, method = "fdr")
p.mat.adj.matr <- matrix(p.mat.adjusted, nrow = ncol(data), ncol = ncol(data))

cor_plot <- ggcorrplot::ggcorrplot(M, p.mat = p.mat.adj.matr)
print(cor_plot)
# save plot
ggsave("plots/cor_plot_dummy.png", cor_plot, width = 8, height = 8, dpi = 300, units = "in")



# Fit the model --------------------------------------------------------------------------------


# load the dag

dummy_dag_path <- "dags/pruned_dummy_dag.txt"
dummy_dag_txt <- read_file(dummy_dag_path)
dummy_dag <- dagitty(dummy_dag_txt)

# Fit model
fit <- sem(toString(dummy_dag, "lavaan"), sample.cov = M, sample.nobs = nrow(data))

print(fit)

# # Plot the model
# fit_plot <- coordinates(dummy_dag)
# coords <- dagitty::lavaanToGraph(fit, digits=2)
# coordinates(fit_plot) <- coords
# dummy_fit_plot <- plot(fit_plot, show.coefficients=TRUE)

# # save plot
# ggsave("plots/dummy_fit_plot.png", dummy_fit_plot, width = 8, height = 8, dpi = 300, units = "in")

M <- lavCor(data)
dummy_t <- localTests(x=dummy_dag, sample.cov=M, sample.nobs=nrow(data))

significant_results_dummy <- dummy_t[dummy_t$p.value < 0.001,]

# AGE _||_ HRmx | FBS, HD                -0.3448833 7.982874e-10 -0.4428957 -0.24019782
# ANGe _||_ HRmx | CP_1, HD              -0.2615906 4.729882e-06 -0.3653413 -0.15193678
# CP_1 _||_ CP_3 | ANGe, HD              -0.2340368 4.606626e-05 -0.3394820 -0.12313168
# CP_2 _||_ CP_3 | ANGe, HD              -0.4042661 2.366340e-13 -0.4977898 -0.30428700
# CP_2 _||_ STd | ECGr, HD               -0.1936207 8.055160e-04 -0.3013302 -0.08121887
# HRmx _||_ ST_2 | HD, STd               -0.3074888 5.633839e-08 -0.4081789 -0.20034708
# HRmx _||_ ST_2 | ECGr, HD              -0.3292150 5.120675e-09 -0.4283668 -0.22345436
# ST_2 _||_ ST_3 | HD, STd               -0.4236057 1.117543e-14 -0.5156320 -0.32538796
# Th_6 _||_ Th_7 | AGE, CA, HD, SEX, STd -0.3409801 1.556313e-09 -0.4397647 -0.23545906

print(significant_results_dummy)
plotLocalTestResults(significant_results_dummy)


coeffs <- lavaan_reg(fit)
print(coeffs)
rems <- subset(coeffs, abs(b)<0.01)

print("Coefficients with less than 0.01 influence")
print(rems)

# Covariate Adjusted Model ---------------------------------------------------------------------    

# First for effect of Chol -> HD

adjustmentSets(dummy_dag, "Chol", "HD") # Age, Sex
chol_model <- multinom(HD ~ Chol + AGE + SEX, data)
summary(chol_model)
# Coefficients:
#   (Intercept)      Chol       AGE      SEX
# 2   -2.242123 0.2136882 0.4407852 1.661861
# 3   -2.819084 0.3834052 0.7582583 1.705632
# 4   -2.469758 0.1256771 0.4850403 1.403155
# 5   -4.175637 0.2608540 1.0234324 2.020580

# Std. Errors:
#   (Intercept)      Chol       AGE       SEX
# 2   0.3741702 0.1759730 0.1776296 0.4248825
# 3   0.4600152 0.1944235 0.2245415 0.5038299
# 4   0.4162829 0.2064573 0.2079619 0.4772583
# 5   0.8085042 0.3075488 0.3559190 0.8374642

# Residual Deviance: 708.2647 
# AIC: 740.2647 

# Second, FBS -> HD
adjustmentSets(dummy_dag, "FBS", "HD") # Chol
fbs_model <- multinom(HD ~ FBS + Chol, data)
summary(fbs_model)

# # weights:  20 (12 variable)
# initial  value 478.003060 
# iter  10 value 378.170124
# final  value 375.511273 
# converged
# Call:
# multinom(formula = HD ~ FBS + Chol, data = data)

# Coefficients:
#   (Intercept)        FBS       Chol
# 2  -0.9853774 -1.0520536 0.11700714
# 3  -1.6484977  0.5611345 0.31028194
# 4  -1.6211130  0.5663420 0.05897531
# 5  -2.4378869 -0.7057079 0.19325220

# Std. Errors:
#   (Intercept)       FBS      Chol
# 2   0.1642404 0.6356725 0.1602804
# 3   0.2143989 0.4644565 0.1752345
# 4   0.2107066 0.4614401 0.1932076
# 5   0.3026386 1.0654314 0.2769905

# Residual Deviance: 751.0225 
# AIC: 775.0225 

# Third, Age -> HD
adjustmentSets(dummy_dag, "AGE", "HD")
age_model <- multinom(HD ~ AGE, data)
summary(age_model)

# weights:  15 (8 variable)
# initial  value 478.003060 
# iter  10 value 372.230503
# final  value 371.598922 
# converged
# Call:
# multinom(formula = HD ~ AGE, data = data)

# Coefficients:
#   (Intercept)       AGE
# 2   -1.071108 0.3428351
# 3   -1.591261 0.6707427
# 4   -1.511311 0.3898784
# 5   -2.683598 0.8796914

# Std. Errors:
#   (Intercept)       AGE
# 2   0.1591548 0.1645132
# 3   0.2020326 0.2085310
# 4   0.1895229 0.1964706
# 5   0.3415112 0.3353958

# Residual Deviance: 743.1978 
# AIC: 759.1978 

# Fourth, Sex -> HD
adjustmentSets(dummy_dag, "SEX", "HD") 
sex_model <- multinom(HD ~ SEX, data)
summary(sex_model)

# weights:  15 (8 variable)
# initial  value 478.003060 
# iter  10 value 369.333703
# final  value 369.318634 
# converged
# Call:
# multinom(formula = HD ~ SEX, data = data)

# Coefficients:
#   (Intercept)      SEX
# 2   -2.065443 1.383456
# 3   -2.316741 1.160286
# 4   -2.316736 1.160290
# 5   -3.569512 1.478745

# Std. Errors:
#   (Intercept)       SEX
# 2   0.3538291 0.3983130
# 3   0.3961547 0.4515409
# 4   0.3961537 0.4515396
# 5   0.7169918 0.7849991

# Residual Deviance: 738.6373 
# AIC: 754.6373 

# Canonical correlation analysis

# Select predictor set (continuous and dummy-encoded variables)
predictor_vars <- data %>%
  select(CP_1, CP_2, CP_3, STs_2, STs_3, Thal_6, Thal_7)

# Select outcome set (ordered categorical variables)
outcome_HD_dummy <- dummy_cols(data[, "HD"], remove_most_frequent = TRUE, remove_selected_columns = TRUE)
colnames(outcome_HD_dummy) <- c("HD_1", "HD_2", "HD_3", "HD_4")
# Perform Canonical Correlation Analysis
cca_result <- cancor(predictor_vars, outcome_HD_dummy)
print("Canonical Correlation Analysis")
print(cca_result)

predictor_scores <- as.matrix(predictor_vars) %*% cca_result$xcoef
outcome_scores <- as.matrix(outcome_HD_dummy) %*% cca_result$ycoef

# Canonical loadings
predictor_loadings <- cor(as.matrix(predictor_vars), predictor_scores)
outcome_loadings <- cor(as.matrix(outcome_HD_dummy), outcome_scores)


par(mfrow = c(2, 2))  # 2x2 grid
# Barplot of canonical correlations
barplot(cca_result$cor, main = "Canonical Correlations", col = "steelblue", 
        xlab="Canonical Variable", 
        ylab="Correlation")

# Scatterplot of first canonical variables
plot(predictor_scores[, 1], outcome_scores[, 1],
     main = "First Canonical Variables",
     xlab = "Predictor Canonical Variable 1",
     ylab = "Outcome Canonical Variable 1",
     col = "darkorange", pch = 16)
abline(lm(outcome_scores[, 1] ~ predictor_scores[, 1]), col = "blue")

# Barplot of predictor loadings
barplot(predictor_loadings[, 1],
        main = "Predictor Loadings (First Canonical Variable)",
        col = "skyblue", names.arg = colnames(predictor_vars), las = 2)

# Barplot of outcome loadings
barplot(outcome_loadings[, 1],
        main = "Outcome Loadings (First Canonical Variable)",
        col = "lightgreen", names.arg = colnames(outcome_HD_dummy), las = 2)

par(mfrow = c(1, 1))  # Reset layout
