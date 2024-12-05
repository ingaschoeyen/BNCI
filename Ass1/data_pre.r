# install.packages("tidyr", "dplyr", "ggplot2", "patchwork", "reshape2", "ggcorrplot")
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(reshape2)
library(ggcorrplot)


data_type <- 'data_clean' # 'data' or 'data_clean'
data_path <- paste(data_type,'.csv', sep='')


data_raw <- read.csv(data_path)
data_raw <- data_raw[complete.cases(data_raw),]
colnames(data_raw) <- c("subject_id", "AGE", "SEX", "CP", "BPr", "Chol", "FBS", "ECGr", "HRm", "ANGe", "STd", "STs","CA", "Thal", "HD")

#------Plot raw values to check for outliers------------------------------------------------------------------


df_raw_long <- data_raw %>%
  pivot_longer(cols = -subject_id, names_to = "variable", values_to = "value")

# Define high-scale and low-scale variable sets
high_scale_vars <- c("BPr", "Chol", "HRm", "AGE")  # Variables with larger scales
low_scale_vars <- setdiff(unique(df_long$variable), high_scale_vars)  # The rest
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
#remove any outliers larger than 500
data <- data[!apply(data, 1, function(x) any(x > 500)),]

data <- data |>
  mutate(
    sex     = as.numeric(factor(sex, ordered=TRUE))-1,
    cp      = as.numeric(factor(cp, ordered=TRUE))-1,
    fbs     = as.numeric(factor(fbs, ordered=TRUE))-1,
    restecg = as.numeric(factor(restecg, ordered=TRUE))-1,
    exang   = as.numeric(factor(exang, ordered=TRUE))-1,
    slope = as.numeric(factor(slope, ordered=TRUE))-1,
    ca      = as.numeric(factor(ca, ordered = TRUE))-1,
    num     = as.numeric(factor(num, ordered = TRUE))-1,
  )

data$thal <- as.numeric(ordered(case_match(
  data$thal,
  3 ~ 0,
  6 ~ 1,
  7 ~ 2
  )))-1



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

# Define high-scale and low-scale variable sets
cont_vars <- c("BPr", "Chol", "HRm", "AGE", "STd")  # Variables with larger scales
categ_vars <- setdiff(unique(df_long$variable), cont_vars)  # The rest

df_long <- data %>%
  pivot_longer(col = -subject_id, names_to = "variable", values_to = "value")

# Plot high-scale variables (left side)
plot_high <- ggplot(df_long %>% filter(variable %in% cont_vars), aes(x = variable, y = value)) +
  geom_jitter(width = 0.2, height=0, alpha = 0.6) +
  scale_y_continuous(name = "") +  # Left y-axis label
  labs(x = NULL, title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot low-scale variables (right side)
plot_low <- ggplot(df_long %>% filter(variable %in% categ_vars), aes(x = variable, y = value)) +
  geom_jitter(width = 0.3, height=0.1, alpha = 0.2) +
  scale_y_continuous(name = "", position = "right") +  
  labs(x = NULL, title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Combine the plots side by side
plot_both <- plot_high + plot_low + plot_layout(widths = c(1, 2))

# save the plot
ggsave(filename = paste(data_type,"_scatter.png"), plot = plot_both, path = "./plots/", width = 8, height = 4, units = "in", dpi = 300)

data <- data[,-1]
M <- lavaan::lavCor(data)
p.mat <- cor_pmat(data)

p.mat.adjusted <- p.adjust(p.mat, method = "fdr")

p.mat.adj.matrix <- matrix(p.mat.adjusted, nrow = ncol(M), ncol = ncol(M))

corr_plot <- ggcorrplot(M, p.mat = p.mat.adj.matrix)

ggsave(filename = paste(data_type,"corr_matrix.png"), plot = corr_plot, path = "./plots/", width = 6, height = 6, units = "in", dpi = 300)