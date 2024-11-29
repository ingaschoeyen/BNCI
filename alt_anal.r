library(lavaan)
library(dagitty)
library(ggdag)
library(ggplot2)
library(tidyverse)
library(lavaanExtra)
library(fastDummies)
library(ggcorrplot)

# Load Data -----------------------------------------------------------------------------------

data <- read.csv(data_path)

data <- data[complete.cases(data),]
data <- data[,-1]

colnames(data) <- c("AGE", "SEX", "CP", "BPr", "Chol", "FBS", "ECGr", "HRmax", "ANGe", "STd", "STs","CA", "Thal", "HD")

# two alternatives - only HD dummy vs HD + unordered vars (ECGr, Thal, STs)

# Scale continuous variables
data <- data |>
  mutate(
    AGE   = scale(AGE),
    BPr   = scale(BPr),
    Chol  = scale(Chol),
    HRmax = scale(HRmax),
    STd   = scale(STd)
  )

# Create dataset 1: HD as dummy, others as ordered factors
data1 <- data |>
  mutate(
    SEX     = as.numeric(factor(SEX, ordered = TRUE)) - 1,
    CP      = as.numeric(factor(ifelse(CP > 1, 1, 0))) - 1,
    FBS     = as.numeric(factor(FBS, ordered = TRUE)) - 1,
    ECGr    = factor(ECGr, ordered = TRUE),
    ANGe    = as.numeric(factor(ANGe, ordered = TRUE)) - 1,
    STs     = as.numeric(factor(ifelse(STs > 1, 1, 0))) - 1,
    CA      = factor(CA, ordered = TRUE),
    Thal    = factor(Thal, ordered = TRUE),
    HD      = as.numeric(factor(HD)) - 1  # Dummy encode HD
  )

print(head(data1))

# Create dataset 2: HD and unordered categorical variables as dummy codes
unordered_vars <- c("SEX", "CP", "Thal", "STs")
categ_var <- c("SEX", "CP", "FBS", "ECGr", "ANGe", "STs", "CA", "Thal", "HD")

data2 <- data |>
  # Dummy encode HD and unordered variables
  fastDummies::dummy_cols(select_columns = c("HD", unordered_vars), 
                          remove_selected_columns = TRUE) |>
  # Retain ordered factors for other categorical variables
  mutate(
    SEX     = as.numeric(factor(SEX, ordered = TRUE)) - 1,
    CP      = as.numeric(factor(ifelse(CP > 1, 1, 0))) - 1,
    FBS     = as.numeric(factor(FBS, ordered = TRUE)) - 1,
    ANGe    = as.numeric(factor(ANGe, ordered = TRUE)) - 1,
    CA      = factor(CA, ordered = TRUE)
  )



print(head(data2))


# ---------- load and test dag 1 ------------------------------------------------------------
dag1_path <- "dags/dag_HD_dummy_others_ordered.txt"
dag1txt <- read_file(dag1_path)
dag1 <- dagitty(dag1txt)

# plot dag
dag_fig <- ggdag(dag)
print(dag_fig)

# plot correlation matrix
cor_mat <- cor(data)
M <- lavCor(data)
p.mat <- cor_pmat(data)
corr_plot <- ggcorrplot(round(M, 2), p.mat = p.mat)+theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust = .9))
print(corr_plot)

# fit the model

print(M)
fit <- sem(toString(dag, "lavaan"), sample.cov = M, sample.nobs = nrow(data))   # fit the model
print(varTable(fit))  # print the results

# test the implied conditional independencies of the model
t <- localTests(x = dag, sample.cov = M, sample.nobs = nrow(data))
print(t)

# plot the results
conIndT_plot <- plotLocalTestResults(t)
print(conIndT_plot)

# ---------- load and test dag 2 ------------------------------------------------------------
dag2_path <- "dags/dummy_dag_HD_ord.txt"
dag2txt <- read_file(dag2_path)
dag2 <- dagitty(dag2txt)