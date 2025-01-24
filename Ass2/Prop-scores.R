library( stddiff )
library( forestplot )
library( MatchIt )
library( dplyr )
library( moments )
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




chol_lm <- lm("Chol ~ AGE + SEX", data)

round( coef(chol_lm), 3)

data$score <- dnorm(chol_lm$residuals, sd=sigma(chol_lm))


baseline <- glm("HD~Chol", data, family="binomial")

glm("HD ~ Chol + AGE + SEX", data, family = 'binomial')
glm("HD ~ Chol + score", data, family = 'binomial')





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



score2form <- paste(exposure, "~", paste(covs2[[1]], collapse = "+"))
formula_canon <- paste(outcome, "~", exposure, "+", "score2")

chollm2 <- lm(score2form, data = data)
data$score2 <- dnorm(chollm2$residuals, sd=sigma(chollm2))
canonical_fit <- glm(formula_canon, data = data, family = "binomial")

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

################

covs_final <- c("SEX", "BPr+SEX", "CP+SEX", "BPr+CP+SEX", "ECGr+SEX", "FBS+SEX", "CP+FBS+SEX")

for (i in 1:7){
  scorenewform <- paste(exposure, "~", paste(covs_final[i], collapse = "+"))
  formula_new <- paste(outcome, "~", exposure, "+", "scorenew")
  
  chollmnew <- lm(scorenewform, data = data)
  data$scorenew <- dnorm(chollm2$residuals, sd=sigma(chollmnew))
  new_fit <- glm(formula_new, data = data, family = "binomial")
  
  fits <- fits %>% add_row(
    formula = formula_new,
    AIC = AIC(new_fit),
    logLik = logLik(new_fit)[1],
    OR = exp(coef(new_fit)[2]),
    CI_lower = exp(confint(new_fit)[2,1]),
    CI_upper = exp(confint(new_fit)[2,2]),
    coef_chol = coef(new_fit)[2],
    pvalue = summary(new_fit)$coefficients[2,4]
  )
}
fitstest <- fits
fitstest %>% 
  mutate_if(is.numeric, round, 3)
fits3 <- head(fitstest, 3)

text_table <- cbind(
  fits3$formula,
  sprintf("%.2f", fits3$AIC),
  sprintf("%.2f", fits3$logLik),
  sprintf("%.2f", fits3$coef_chol),
  sprintf("%.4f", fits3$pvalue)
)

header <- c("Formula", "AIC", "log(L)", "\U1D5D (Chol)", "p (Chol)")
text_table <- rbind(header, text_table)
cov_adj_plot <- forestplot(
  text_table,
  title = "Propensity score adjustment",
  mean = c(NA, fits3$OR),
  lower = c(NA,fits3$CI_lower),
  upper = c(NA, fits3$CI_upper),
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
