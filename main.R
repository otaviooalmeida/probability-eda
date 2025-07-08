# ====================================
# EDA – Probability and Statistics
# Authors: Otávio Almeida (169502) and Guilherme M. Soares (169513)
# Professor: Dr. Viviane Mattos
# ====================================

# Set working directory and load dataset
setwd("path/to/your/project")
data <- read.table("dataset.txt", header = TRUE, fill = TRUE, na.strings = c("NA", ""))

# Install and load packages
install.packages(c("dplyr", "ggplot2", "psych", "moments", "gridExtra"))
library(dplyr)
library(ggplot2)
library(psych)
library(moments)
library(gridExtra)

# Variable 1: Corruption Index
corruption <- data$Índice_de_corrupção

# Frequency table
corruption_table <- table(corruption)
freq_abs <- as.data.frame(corruption_table)
freq_abs$percentage <- round((freq_abs$Freq / sum(freq_abs$Freq)) * 100, 2)

# Descriptive statistics
corruption_stats <- data.frame(
  Variable = "Corruption Index",
  Mean = mean(corruption, na.rm = TRUE),
  Median = median(corruption, na.rm = TRUE),
  Mode = as.numeric(names(sort(table(corruption), decreasing = TRUE)[1])),
  Min = min(corruption, na.rm = TRUE),
  Max = max(corruption, na.rm = TRUE),
  Range = diff(range(corruption, na.rm = TRUE)),
  SD = sd(corruption, na.rm = TRUE),
  Variance = var(corruption, na.rm = TRUE),
  Q1 = quantile(corruption, 0.25, na.rm = TRUE),
  Q3 = quantile(corruption, 0.75, na.rm = TRUE),
  IQR = IQR(corruption, na.rm = TRUE),
  P10 = quantile(corruption, 0.10, na.rm = TRUE),
  P90 = quantile(corruption, 0.90, na.rm = TRUE),
  Skewness = skewness(corruption, na.rm = TRUE),
  Kurtosis = kurtosis(corruption, na.rm = TRUE)
)

# Plots
hist(corruption, main = "Histogram – Corruption Index", xlab = "Ranking Position", col = "steelblue", breaks = 15)
boxplot(corruption, main = "Boxplot – Corruption Index", ylab = "Ranking Position", col = "orange")
qqnorm(corruption, main = "Normal Q-Q Plot – Corruption Index")
qqline(corruption, col = "red")

# Variable 2: Annual Income per Capita
income <- data$Renda_anual_

# Descriptive statistics
income_stats <- data.frame(
  Variable = "Annual Income per Capita",
  Mean = mean(income, na.rm = TRUE),
  Median = median(income, na.rm = TRUE),
  Mode = as.numeric(names(sort(table(income), decreasing = TRUE)[1])),
  Min = min(income, na.rm = TRUE),
  Max = max(income, na.rm = TRUE),
  Range = diff(range(income, na.rm = TRUE)),
  SD = sd(income, na.rm = TRUE),
  Variance = var(income, na.rm = TRUE),
  Q1 = quantile(income, 0.25, na.rm = TRUE),
  Q3 = quantile(income, 0.75, na.rm = TRUE),
  IQR = IQR(income, na.rm = TRUE),
  P10 = quantile(income, 0.10, na.rm = TRUE),
  P90 = quantile(income, 0.90, na.rm = TRUE),
  Skewness = skewness(income, na.rm = TRUE),
  Kurtosis = kurtosis(income, na.rm = TRUE)
)

# Plots (raw)
hist(income, main = "Histogram – Annual Income per Capita", xlab = "Income (US$)", col = "forestgreen", breaks = 20)
boxplot(income, main = "Boxplot – Annual Income per Capita", ylab = "Income (US$)", col = "tomato")
qqnorm(income, main = "Normal Q-Q Plot – Annual Income per Capita")
qqline(income, col = "red")

# Log transformation
log_income <- log10(income[income > 0])
hist(log_income, main = "Histogram – Annual Income per Capita (log10)", xlab = "log10(Income in US$)", col = "purple", breaks = 15)
boxplot(log_income, main = "Boxplot – Annual Income per Capita (log10)", ylab = "log10(Income in US$)", col = "darkorange")

# Correlation plot
plot(corruption, income,
     main = "Scatterplot: Corruption Index vs. Income per Capita",
     xlab = "Corruption Index (ranking position)",
     ylab = "Income per Capita (US$)",
     pch = 19, col = "darkblue")

# Consolidated statistics table
summary_table <- rbind(
  cbind(Variable = "Corruption Index", corruption_stats),
  cbind(Variable = "Annual Income per Capita", income_stats)
)

# Transpose and format
final_table <- as.data.frame(t(summary_table[-1]))
colnames(final_table) <- summary_table$Variable
final_table$Statistic <- rownames(final_table)
final_table <- final_table[, c("Statistic", "Corruption Index", "Annual Income per Capita")]

# Export (uncomment if needed)
# write.csv(summary_table, file = "summary_table.csv", row.names = FALSE)
# write.csv(freq_abs, file = "corruption_frequency.csv", row.names = FALSE)

# png("summary_table.png", width = 800, height = 400, res = 150)
# grid.table(final_table)
# print(final_table)
# dev.off()
