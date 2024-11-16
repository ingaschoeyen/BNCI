# install.packages("tidyr", "dplyr", "ggplot2", "patchwork", "reshape2", "ggcorrplot")
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(reshape2)
library(ggcorrplot)


data_type <- 'data' # 'data' or 'data_clean'
data_path <- paste(data_type,'.csv', sep='')


data_raw <- read.csv(data_path)
data_raw <- data_raw[complete.cases(data_raw),]
colnames(data_raw) <- c("subject_id", "AGE", "SEX", "CP", "BPr", "Chol", "FBS", "ECGr", "HRm", "ANGe", "STd", "STs","CA", "Thal", "HD")

#------Plot raw values to check for outliers------------------------------------------------------------------


df_raw_long <- data_raw %>%
  pivot_longer(cols = -subject_id, names_to = "variable", values_to = "value")

# Define high-scale and low-scale variable sets
high_scale_vars <- c("BPr", "Chol", "HRm")  # Variables with larger scales
non_low <- c("AGE", "BPr", "Chol", "SEX", "HRm") # includes age and sex, since we dont really care about distribution
low_scale_vars <- setdiff(unique(df_long$variable), non_low)  # The rest

# Plot high-scale variables (left side)
plot_high_raw <- ggplot(df_raw_long %>% filter(variable %in% high_scale_vars), aes(x = variable, y = value)) +
  geom_jitter(width = 0.2,height=0, alpha = 0.6) +
  scale_y_continuous(name = "") +  # Left y-axis label
  labs(x = NULL, title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot low-scale variables (right side)
plot_low_raw <- ggplot(df_raw_long %>% filter(variable %in% low_scale_vars), aes(x = variable, y = value)) +
  geom_jitter(width = 0.3, height=0.1, alpha = 0.2) +
  scale_y_continuous(name = "", position = "right") +  
  labs(x = NULL, title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Combine the plots side by side
plot_both_raw <- plot_high_raw + plot_low_raw + plot_layout(widths = c(1, 2))

# save the plot
ggsave(filename = paste(data_type, "_raw_scatter.png"), plot = plot_both_raw, path = "./plots/", width = 8, height = 4, units = "in", dpi = 300)

#------Scale data------------------------------------------------------------------

data <- read.csv(data_path)
data <- data[complete.cases(data),]
data <- data |>
  mutate(
    sex     = as.numeric(factor(sex, ordered=TRUE))-1,
    cp      = as.numeric(factor(ifelse(cp[] > 1, 1, 0)))-1,
    fbs     = as.numeric(factor(fbs, ordered=TRUE))-1,
    restecg = as.numeric(factor(restecg, ordered=TRUE)),
    exang   = as.numeric(factor(exang, ordered=TRUE))-1,
    slope   = as.numeric(factor(ifelse(slope[] > 1, 1, 0)))-1,
    ca      = as.numeric(factor(ca, ordered = TRUE)),
    num     = as.numeric(factor(num, ordered = TRUE))-1,
  )

data$thal <- as.numeric(ordered(case_match(
  data$thal,
  3 ~ 0,
  7 ~ 1,
  6 ~ 2
  )))

data$cp <- as.numeric(ordered(case_match(
  data$cp,
  1 ~ 0,
  2 ~ 1,
  3 ~ 2,
  4 ~ 3
  )))

data$slope <- as.numeric(ordered(case_match(
  data$slope,
  1 ~ 0,
  2 ~ 1,
  3 ~ 2
  )))

data <- data |>
  mutate(
    age      = scale(age),
    trestbps = scale(trestbps),
    chol     = scale(chol),
    thalach  = scale(thalach),
    oldpeak  = scale(oldpeak),
)



colnames(data) <- c("subject_id", "AGE", "SEX", "CP", "BPr", "Chol", "FBS", "ECGr", "HRm", "ANGe", "STd", "STs","CA", "Thal", "HD")

#------Plot scaled values to check for skewed distributions------------------------------------------------------------------

df_long <- data %>%
  pivot_longer(cols = -subject_id, names_to = "variable", values_to = "value")

# Define high-scale and low-scale variable sets
high_scale_vars <- c("BPr", "Chol", "HRm", "STd")  # Variables with larger scales
non_low <- c("AGE", "BPr", "Chol", "SEX", "HRm", "STd") # includes age and sex, since we dont really care about distribution
low_scale_vars <- setdiff(unique(df_long$variable), non_low)  # The rest

# Plot high-scale variables (left side)
plot_high <- ggplot(df_long %>% filter(variable %in% high_scale_vars), aes(x = variable, y = value)) +
  geom_jitter(width = 0.2, height=0, alpha = 0.6) +
  scale_y_continuous(name = "") +  # Left y-axis label
  labs(x = NULL, title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot low-scale variables (right side)
plot_low <- ggplot(df_long %>% filter(variable %in% low_scale_vars), aes(x = variable, y = value)) +
  geom_jitter(width = 0.3, height=0.1, alpha = 0.2) +
  scale_y_continuous(name = "", position = "right") +  
  labs(x = NULL, title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Combine the plots side by side
plot_both <- plot_high + plot_low + plot_layout(widths = c(1, 2))

# save the plot
ggsave(filename = paste(data_type,"_scatter.png"), plot = plot_both, path = "./plots/", width = 8, height = 4, units = "in", dpi = 300)


# Compute and plot correlations between variables
cor_round <- round(cor(data[,-1]), 5)

# cor_round[upper.tri(cor_round, diag = F)]

melt_cor <- melt(cor_round)

cor_mat <- ggplot(data = melt_cor, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  theme(legend.margin = margin(t=0.5, unit='cm'), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("") +
  ylab("") +
  labs(fill = "Corr(X,Y)")

ggsave(filename = paste(data_type,"_corr_matrix.png"), plot = cor_mat, path = "./plots/", width = 4, height = 4, units = "in", dpi = 300)

p.mat <- cor_pmat(data[,-1])

alt_corr_plot <- ggcorrplot(cor_round, p.mat = p.mat)

ggsave(filename = paste(data_type,"_alt_corr_matrix.png"), plot = alt_corr_plot, path = "./plots/", width = 6, height = 6, units = "in", dpi = 300)