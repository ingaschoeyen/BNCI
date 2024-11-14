library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

data_path <- 'data.csv'

data <- read.csv(data_path)


# # drop NA
# data <- data[complete.cases(data),]
# # drop index
# # data <- data[,-1]

# data <- data |>
#   mutate(
#     sex     = as.numeric(factor(sex, ordered=TRUE))-1,
#     cp      = as.numeric(factor(ifelse(cp[] > 1, 1, 0)))-1,
#     fbs     = as.numeric(factor(fbs, ordered=TRUE))-1,
#     restecg = factor(restecg, ordered=TRUE),
#     exang   = as.numeric(factor(exang, ordered=TRUE))-1,
#     slope   = as.numeric(factor(ifelse(slope[] > 1, 1, 0)))-1,
#     ca      = factor(ca, ordered = TRUE),
#     num     = as.numeric(factor(num, ordered = TRUE))-1,
#   )

# data$thal <- ordered(case_match(
#   data$thal,
#   3 ~ 0,
#   7 ~ 1,
#   6 ~ 2
#   ))

# data <- data |>
#   mutate(
#     age      = scale(age),
#     trestbps = scale(trestbps),
#     chol     = scale(chol),
#     thalach  = scale(thalach),
#     oldpeak  = scale(oldpeak),
# )

# colnames(data) <- c("subject_id", "AGE", "SEX", "CP", "BPr", "Chol", "FBS", "ECGr", "HRm", "ANGe", "STd", "STs","CA", "Thal", "HD")

df_long <- data %>%
  pivot_longer(cols = -subject_id, names_to = "variable", values_to = "value")

# Create the plot with facets
fig_scatter <- ggplot(df_long, aes(x = variable, y = value)) +
  geom_jitter(width = 0.2, alpha = 0.6) +  # Adds some horizontal jitter to separate points
  labs(x = "Variable", y = "Value", title = "Scatter Plot of Variable Value Distributions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

print(fig_scatter)

# Define high-scale and low-scale variable sets
high_scale_vars <- c("trestbps", "chol", "thalach")  # Variables with larger scales
non_low <- c("age", "trestbps", "chol", "thalach", "sex")
low_scale_vars <- setdiff(unique(df_long$variable), non_low)  # The rest

# Plot high-scale variables (left side)
plot_high <- ggplot(df_long %>% filter(variable %in% high_scale_vars), aes(x = variable, y = value)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  scale_y_continuous(name = "High Scale Values") +  # Left y-axis label
  labs(x = NULL, title = "High-Scale Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot low-scale variables (right side)
plot_low <- ggplot(df_long %>% filter(variable %in% low_scale_vars), aes(x = variable, y = value)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  scale_y_continuous(name = "Low Scale Values", position = "right") +  # Right y-axis label
  labs(x = NULL, title = "Low-Scale Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Combine the plots side by side
plot_both <- gridExtra::grid.arrange(plot_high, plot_low, ncol = 2)

ggsave(filename = "data_scatter.png", plot = plot_both, path = "./plots/", width = 6, height = 4, units = "in", dpi = 300)