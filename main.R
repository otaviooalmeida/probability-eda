# Código do trabalho de AED - Profa. Viviane Mattos
# Otávio Almeida, 169502 e Guilherme M. Soares, 169513

# Definindo diretório e lendo o dataset
setwd("/home/otavio/Documents/")
dados <- read.table("dataset.txt", header = TRUE, fill = TRUE, na.strings = c("NA", ""))

# Carregando pacotes necessários
library(dplyr)
library(ggplot2)
library(psych)
library(moments)
library(gridExtra)

# --- Análise da variável: Índice de Corrupção ---
corrupcao <- dados$Índice_de_corrupção

# Tabela de frequências absolutas e relativas
tabela_corrupcao <- table(corrupcao)
freq_abs <- as.data.frame(tabela_corrupcao)
freq_abs$percentual <- round((freq_abs$Freq / sum(freq_abs$Freq)) * 100, 2)

# Medidas descritivas
medidas_corrupcao <- data.frame(
  Variável = "Índice de Corrupção",
  Média = mean(corrupcao, na.rm = TRUE),
  Mediana = median(corrupcao, na.rm = TRUE),
  Moda = as.numeric(names(sort(table(corrupcao), decreasing = TRUE)[1])),
  Mínimo = min(corrupcao, na.rm = TRUE),
  Máximo = max(corrupcao, na.rm = TRUE),
  Amplitude = diff(range(corrupcao, na.rm = TRUE)),
  Desvio_Padrão = sd(corrupcao, na.rm = TRUE),
  Variância = var(corrupcao, na.rm = TRUE),
  Q1 = quantile(corrupcao, 0.25, na.rm = TRUE),
  Q3 = quantile(corrupcao, 0.75, na.rm = TRUE),
  IQR = IQR(corrupcao, na.rm = TRUE),
  P10 = quantile(corrupcao, 0.10, na.rm = TRUE),
  P90 = quantile(corrupcao, 0.90, na.rm = TRUE),
  Assimetria = skewness(corrupcao, na.rm = TRUE),
  Curtose = kurtosis(corrupcao, na.rm = TRUE)
)

# Gráficos para Índice de Corrupção
hist(corrupcao,
     main = "Histograma – Índice de Corrupção",
     xlab = "Posição no Ranking",
     col = "steelblue", breaks = 15)

boxplot(corrupcao,
        main = "Boxplot – Índice de Corrupção",
        ylab = "Posição no Ranking",
        col = "orange")

qqnorm(corrupcao, main = "Gráfico de Probabilidade Normal – Índice de Corrupção")
qqline(corrupcao, col = "red")

# --- Análise da variável: Renda Anual per Capita ---
renda <- dados$Renda_anual_

# Medidas descritivas
medidas_renda <- data.frame(
  Variável = "Renda Anual per Capita",
  Média = mean(renda, na.rm = TRUE),
  Mediana = median(renda, na.rm = TRUE),
  Moda = as.numeric(names(sort(table(renda), decreasing = TRUE)[1])),
  Mínimo = min(renda, na.rm = TRUE),
  Máximo = max(renda, na.rm = TRUE),
  Amplitude = diff(range(renda, na.rm = TRUE)),
  Desvio_Padrão = sd(renda, na.rm = TRUE),
  Variância = var(renda, na.rm = TRUE),
  Q1 = quantile(renda, 0.25, na.rm = TRUE),
  Q3 = quantile(renda, 0.75, na.rm = TRUE),
  IQR = IQR(renda, na.rm = TRUE),
  P10 = quantile(renda, 0.10, na.rm = TRUE),
  P90 = quantile(renda, 0.90, na.rm = TRUE),
  Assimetria = skewness(renda, na.rm = TRUE),
  Curtose = kurtosis(renda, na.rm = TRUE)
)

# Gráficos para Renda Anual per Capita
hist(renda,
     main = "Histograma – Renda Anual per Capita",
     xlab = "Renda (US$ por habitante)",
     col = "forestgreen", breaks = 20)

boxplot(renda,
        main = "Boxplot – Renda Anual per Capita",
        ylab = "Renda (US$ por habitante)",
        col = "tomato")

qqnorm(renda, main = "Gráfico de Probabilidade Normal – Renda Anual per Capita")
qqline(renda, col = "red")

# Gráfico de Dispersão entre as variáveis
plot(corrupcao, renda,
     main = "Dispersão: Índice de Corrupção x Renda per Capita",
     xlab = "Índice de Corrupção (posição no ranking)",
     ylab = "Renda per Capita (US$)",
     pch = 19, col = "darkblue")

# Consolidação das medidas descritivas
medidas_gerais <- rbind(
  cbind(Variável = "Índice de Corrupção", medidas_corrupcao),
  cbind(Variável = "Renda Anual per Capita", medidas_renda)
)

tabela_final <- as.data.frame(t(medidas_gerais[-1]))
colnames(tabela_final) <- medidas_gerais$Variável
tabela_final$Estatística <- rownames(tabela_final)
tabela_final <- tabela_final[, c("Estatística", "Índice de Corrupção", "Renda Anual per Capita")]

# Exportando tabelas e visualizações
write.csv(medidas_gerais, file = "medidas_gerais.csv", row.names = FALSE)
write.csv(freq_abs, file = "freq_abs.csv", row.names = FALSE)

png("medidas_gerais.png", width = 800, height = 400, res = 150)
grid.table(tabela_final)
dev.off()
