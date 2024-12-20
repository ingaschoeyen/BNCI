library( stddiff )
library( forestplot )
library( MatchIt )
# load utility functions
source("./project_utilities.R")

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
    restecg = factor(restecg, ordered=TRUE),
    exang   = as.numeric(factor(exang, ordered=TRUE))-1,
    slope   = as.numeric(factor(ifelse(slope[] > 1, 1, 0)))-1,
    ca      = factor(ca, ordered = TRUE),
    num     = as.numeric(factor(ifelse(num[] > 1, 1, 0)))-1,
  )

data$thal <- ordered(case_match(
  data$thal,
  3 ~ 0,
  7 ~ 1,
  6 ~ 2
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
colnames(data) <- c("AGE", "SEX", "CP", "BPr", "Chol", "FBS", "ECGr", "HRmax", "ANGe", "STd", "STs","CA", "Thal", "HD")


chol_lm <- lm(Chol ~ AGE + SEX, data)

round( coef(chol_lm), 3)

data$score <- dnorm(chol_lm$residuals, sd=sigma(chol_lm))

lm(Chol ~ HD + score, data)
lm(Chol ~ HD, data)

