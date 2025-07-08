# AED - Probabilidade e Estatística
# Alunos: Otávio Almeida (169502) e Guilherme M. Soares (169513)
# Professora Dra. Viviane Mattos


# ==========================
# Diretório e leitura do dataset
# ==========================
setwd("/home/otavio/Documents/")
dados <- read.table("dataset.txt", header = TRUE, fill = TRUE, na.strings = c("NA", ""))

# ==========================
# Instalação e carregamento de pacotes
# ==========================
install.packages(c("dplyr", "ggplot2", "psych", "moments", "gridExtra"))
library(gridExtra)
library(dplyr)
library(ggplot2)
library(psych)
library(moments)

# ==========================
# Variável 1: Índice de Corrupção
# ==========================
corrupcao <- dados$Índice_de_corrupção

# Frequência
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

# Gráficos
hist(corrupcao, 
     main = "Histograma – Índice de Corrupção",
     xlab = "Posição no Ranking",
     col = "steelblue", breaks = 15)

boxplot(corrupcao, 
        main = "Boxplot – Índice de Corrupção",
        ylab = "Posição no Ranking",
        col = "orange", horizontal = FALSE)

qqnorm(corrupcao, main = "Gráfico de Probabilidade Normal – Índice de Corrupção")
qqline(corrupcao, col = "red")

# ==========================
# Variável 2: Renda Anual per Capita
# ==========================
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


# Gráficos (sem transformação)
hist(renda,
     main = "Histograma – Renda Anual per Capita",
     xlab = "Renda (US$ por habitante)",
     col = "forestgreen", breaks = 20)

boxplot(renda,
        main = "Boxplot – Renda Anual per Capita",
        ylab = "Renda (US$ por habitante)",
        col = "tomato", horizontal = FALSE)

# Boxplot da renda transformada com log10
boxplot(log_renda,
        main = "Boxplot – Renda Anual per Capita (log10)",
        ylab = "log10(Renda em US$)",
        col = "darkorange")


qqnorm(renda, main = "Gráfico de Probabilidade Normal – Renda Anual per Capita")
qqline(renda, col = "red")

# Transformação log
log_renda <- log10(renda[renda > 0])
hist(log_renda, 
     main = "Histograma – Renda Anual per Capita (log10)",
     xlab = "log10(Renda em US$)", 
     col = "purple", breaks = 15)

# ==========================
# Relação entre Corrupção e Renda
# ==========================
plot(corrupcao, renda,
     main = "Dispersão: Índice de Corrupção x Renda per Capita",
     xlab = "Índice de Corrupção (posição no ranking)",
     ylab = "Renda per Capita (US$)",
     pch = 19, col = "darkblue")

# ==========================
# Tabela consolidada das medidas
# ==========================
# Versão compacta (mesmo resultado)

medidas_gerais <- rbind(
  cbind(Variável = "Índice de Corrupção", medidas_corrupcao),
  cbind(Variável = "Renda Anual per Capita", medidas_renda)
)

# Transpor mantendo todas as colunas
tabela_final <- as.data.frame(t(medidas_gerais[-1]))  # Transpõe (exceto coluna 'Variável')
colnames(tabela_final) <- medidas_gerais$Variável    # Nomeia as colunas
tabela_final$Estatística <- rownames(tabela_final)   # Adiciona coluna de estatísticas

# Reordenar colunas
tabela_final <- tabela_final[, c("Estatística", "Índice de Corrupção", "Renda Anual per Capita")]

# Visualizar
View(tabela_final)

# ==========================
# Exportar tabelas geradas 
# ==========================
write.csv(medidas_gerais, file = "medidas_gerais.csv", row.names = FALSE)
write.csv(freq_abs, file = "freq_abs.csv", row.names = FALSE)
png("medidas_gerais.png", width = 800, height = 400, res = 150)  # res = DPI
grid.table(tabela_final)
print(tabela_final)
dev.off()