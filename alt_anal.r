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

colnames(data) <- c("subject_id", "AGE", "SEX", "CP", "BPr", "Chol", "FBS", "ECGr", "HRmax", "ANGe", "STd", "STs","CA", "Thal", "HD")

# scale continuous and dummy code categorical variables
categ_var <- c("SEX", "CP", "FBS", "ECGr", "ANGe", "STs", "CA", "Thal", "HD")

# dummy code categorical variables
data <- fastDummies::dummy_cols(data, select_columns=categ_var, remove_most_frequent = TRUE)

# scale continuous variables
data <- data |>
  mutate(
    AGE = scale(AGE),
    BPr = scale(BPr),
    Chol = scale(Chol),
    HRmax  = scale(HRmax),
    STd  = scale(STd)
)

# remove original categorical variables (optional)
data <- data[,setdiff(colnames(data), categ_var)]

print(head(data))


# load and test dag
dag_path <- "dags/stian_dag.txt"
dagtxt <- read_file(dag_path)
dag <- dagitty(dagtxt)

# plot dag
dag_fig <- ggdag(dag)
print(dag_fig)

# plot correlation matrix
cor_mat <- cor(data[,-1])
p.mat <- cor_pmat(data[,-1])
corr_plot <- ggcorrplot(round(cor_mat, 2), p.mat = p.mat) 
print(corr_plot)

# fit the model
M <- lavCor(data)

fit <- sem(toString(dag, "lavaan"), sample.cov = M, sample.nobs = nrow(data))   # fit the model
print(varTable(fit))  # print the results

# test the implied conditional independencies of the model
t <- localTests(x = dag, sample.cov = M, sample.nobs = nrow(data))
print(t)

# plot the results
conIndT_plot <- plotLocalTestResults(t)
print(conIndT_plot)