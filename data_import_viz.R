library(lavaan)
library(dagitty)
library(ggdag)
library(ggplot2)
library(tidyverse)
library(lavaanExtra)
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
    sex     = as.numeric(factor(sex, ordered=TRUE))-1,
    cp      = as.numeric(factor(ifelse(cp[] > 1, 1, 0)))-1,
    fbs     = as.numeric(factor(fbs, ordered=TRUE))-1,
    restecg = as.numeric(factor(restecg, ordered=TRUE)),
    exang   = as.numeric(factor(exang, ordered=TRUE))-1,
    slope   = as.numeric(factor(ifelse(slope[] > 1, 1, 0)))-1,
    ca      = as.numeric(factor(ca, ordered = TRUE)),
    num     = as.numeric(factor(ifelse(num[] > 1, 1, 0)))-1,
  )

data$thal <- as.numeric(ordered(case_match(
  data$thal,
  3 ~ 0,
  7 ~ 1,
  6 ~ 2
  )))

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
colnames(data) <- c("AGE", "SEX", "CP", "BPr", "Chol", "FBS", "ECGr", "HRmax", "ANGe", "STd", "STs","CA", "Thal", "HD")


# Getting lavaan polychoric correlation matrix
M <- lavCor(data)
print(varTable(data))

# commenting this out for now, as it gave an error: Error in cor(data) : 'x' must be numeric

p.mat <- cor_pmat(data)
p.mat.adjusted <- p.adjust(p.mat, method = "fdr")
p.mat.adj.matr <- matrix(p.mat.adjusted, nrow = ncol(data), ncol = ncol(data))
corr_plot <- ggcorrplot(M, p.mat = p.mat.adj.matr)+theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust = .9))
print(corr_plot)


dagpath <- "dags/my_dag.txt"
g <- load_dag(dagpath)

# Fit model
fit <- fit_then_plot(g, M, data)

# Run conditional independence assumption tests -----------------------------------------------------------------------------------

test <- local_tests_utility(g, M, data)
ests <- test[ do.call(order, abs(test)), ]
imp_violations <- tail(ests, 8)
imp_violations

# the important violations mostly seem to have to do with SEX and THA, so we add a direct path here
dagpath <- "dags/my_dag_tested.txt"
g <- load_dag(dagpath)

# re-run conditional independence assumptions tests
test <- local_tests_utility(g, M, data)
ests <- test[ do.call(order, abs(test)), ]
imp_violations <- tail(ests, 8)
imp_violations

# now, there seems to be some issues relating to Age and CA. Here, we actually have two possibilities.
# either add a direct arrow Age -> CA, or an arrow Cho -> CA. I will try both.

dagpath <- "dags/my_experimental_dag_1.txt"
g <- load_dag(dagpath)
test <- local_tests_utility(g, M, data)

dagpath <- "dags/my_experimental_dag_2.txt"
g <- load_dag(dagpath)
test <- local_tests_utility(g, M, data)

# 1 seems not to have fixed the issue, so we go with 2, which is the one that added the link Age->CA


# Fit new model ------------------------------------------------------------------------------------
# fit model (after running conditional independence tests!)

dagpath <- "dags/my_experimental_dag_2.txt"
g <- load_dag(dagpath)

# Fit model
fit <- fit_then_plot(g, M, data)
coeffs <- lavaan_reg(fit)
# remove any that are between 0.01 and -0.01, in the b column
rems <- subset(coeffs, abs(b)<0.01)

# Now I go to dagitty and make this change in our graph, which results in:
dagpath <- "dags/my_pruned_dag.txt"
g <- load_dag(dagpath)
# Fit model
fit <- fit_then_plot(g, M, data)
# do independence tests
test <- local_tests_utility(g, M, data)

# only plot significant results
significant_results <- test[test$p.value < 0.05,]
plotLocalTestResults(significant_results)

# Covariate adjustment set tests -------------------------------------------------------------------

# First for effect of Chol -> HD
adjustmentSets(g, "Chol", "HD")
chol_lm <- glm(HD ~ Chol + AGE + SEX, data, family="binomial")
coef(chol_lm)
# results:
# (Intercept)        Chol         AGE         SEX 
# -1.8760673   0.1995548   0.5364117   1.1917547 


# Second, FBS -> HD
adjustmentSets(g, "FBS", "HD")
fbs_lm <- glm(HD ~ FBS + Chol, data, family="binomial")
coef(fbs_lm)
# results:
# (Intercept)         FBS        Chol 
# -1.0520065   0.6196554   0.1621217 

# Third, Age -> HD
adjustmentSets(g, "AGE", "HD")
age_lm <- glm(HD ~ AGE, data, family="binomial")
coef(age_lm)
# results:
# (Intercept)         AGE 
# -0.9983001   0.4860253 

# Fourth, Sex -> HD
adjustmentSets(g, "SEX", "HD")
sex_lm <- glm(HD ~ SEX, data, family="binomial")
coef(sex_lm)
# results:
# (Intercept)         SEX 
# -1.6094379   0.9162907 


