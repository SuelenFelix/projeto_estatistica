install.packages("ISLR")
install.packages("corrplot")

library(corrplot)
library("ISLR")
library(dplyr)
library(knitr)
library(ggplot2)

data(Credit)
kable(Credit)


str(Credit)

# Verifica possíveis valores ausentes
colSums(is.na(Credit))

install.packages("skimr", dependencies = TRUE)
library(skimr)

dados <-  Credit
skim(dados)

install.packages("GGally")
library(GGally)

### Multicolinearidade: r > 0.9 (ou 0.8)

graf1 <- ggpairs(dados, columns = 1:7, ggplot2::aes(colour=genero))

graf1

install.packages("car")
library(ggplot2)
library(dplyr)
library(car)


# Análise Univariada
# Estatísticas descritivas
summary(dados)

# Visualização das distribuições
ggplot(dados, aes(x = renda)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  ggtitle("Distribuição de Renda")

ggplot(dados, aes(x = limite)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  ggtitle("Distribuição de Limite")

ggplot(dados, aes(x = score)) +
  geom_histogram(bins = 30, fill = "lightcoral", color = "black") +
  ggtitle("Distribuição de Score")

ggplot(dados, aes(x = cartoes)) +
  geom_bar(fill = "lightpink", color = "black") +
  ggtitle("Distribuição de Cartões")

# Análise Bivariada
# Matriz de correlação
cor_matrix <- cor(dados[, c("renda", "limite", "score", "cartoes", "saldo")])
print(cor_matrix)

# Gráficos de dispersão e correlação
ggpairs(dados, columns = c("renda", "limite", "score", "cartoes", "saldo"),
        aes(colour = casado))

# Gráficos de dispersão específicos
ggplot(dados, aes(x = renda, y = saldo)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Dispersão entre Renda e Saldo")

ggplot(dados, aes(x = limite, y = saldo)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Dispersão entre Limite e Saldo")

ggplot(dados, aes(x = score, y = saldo)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Dispersão entre Score e Saldo")

ggplot(dados, aes(x = cartoes, y = saldo)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Dispersão entre Cartões e Saldo")

# Modelo de regressão linear
modelo <- lm(saldo ~ renda + limite + score + cartoes, data = dados)
# Exibir resultados de correlação de Pearson
cor_results_renda <- cor.test(dados$renda, dados$saldo)
print(cor_results_renda)

cor_results_limite <- cor.test(dados$limite, dados$saldo)
print(cor_results_limite)

cor_results_score <- cor.test(dados$score, dados$saldo)
print(cor_results_score)

cor_results_cartoes <- cor.test(dados$cartoes, dados$saldo)
print(cor_results_cartoes)

vif_results <- vif(modelo)
print(vif_results)

# Interpretação dos resultados de VIF
ifelse(any(vif_results > 10), "Atenção: Pode haver multicolinearidade!", "Não há problemas de multicolinearidade significativos.")


install.packages('lmtest', repos = 'http://cran.us.r-project.org')
# Ajustar o modelo inicial
modelo_inicial <- lm(saldo ~ renda + limite + score + cartoes + idade + educacao + genero + casado + etnia, data = dados)

# Resumo do modelo
summary(modelo_inicial)

# Análise dos resíduos
par(mfrow=c(2, 2)) # Configuração do layout do gráfico
plot(modelo_inicial, which = 1, col = "lightgreen") # Histograma dos resíduos
plot(modelo_inicial, which = 2, col = "lightgreen") # Gráfico QQ plot dos resíduos
plot(modelo_inicial, which = 3, col = "lightgreen") # Resíduos vs. Valores Ajustados
plot(modelo_inicial, which = 5, col = "lightgreen") # Resíduos vs. Variáveis Explicativas

# Gráfico de Dispersão dos Resíduos
residuos <- resid(modelo_inicial)
valores_ajustados <- fitted(modelo_inicial)
plot(valores_ajustados, residuos, main = "Gráfico de Dispersão dos Resíduos",
     xlab = "Valores Ajustados", ylab = "Resíduos", col = "lightgreen")
abline(h = 0, col = "red", lty = 2) # Adiciona linha horizontal em y=0

# Teste de normalidade dos resíduos
shapiro_test <- shapiro.test(residuals(modelo_inicial))
cat("Teste de Shapiro-Wilk para normalidade dos resíduos:\n")
print(shapiro_test)

# Teste de homocedasticidade
library(lmtest)
bptest_result <- bptest(modelo_inicial)
cat("Teste de Breusch-Pagan para homocedasticidade:\n")
print(bptest_result)

# Ajustando vários modelos
modelo_1 <- lm(saldo ~ renda + limite, data = dados)
modelo_2 <- lm(saldo ~ renda + limite + score, data = dados)
modelo_3 <- lm(saldo ~ renda + limite + score + cartoes + idade, data = dados)

# Resumos dos modelos
summary(modelo_1)
summary(modelo_2)
summary(modelo_3)

# Comparando AIC e BIC
aic_values <- c(AIC(modelo_1), AIC(modelo_2), AIC(modelo_3))
bic_values <- c(BIC(modelo_1), BIC(modelo_2), BIC(modelo_3))

cat("AIC dos modelos:\n", aic_values, "\n")
cat("BIC dos modelos:\n", bic_values, "\n")

install.packages("report")
plot(modelo_3)

library(report)
report(modelo_3)

summary(modelo_3)


# Criando um novo data frame com as médias das variáveis explicativas
novas.preditoras <- data.frame(
  renda = c(14.9, 104.6),
  limite = c(3606, 7075),
  score = c(283, 514),
  cartoes = c(1, 4),
  idade = c(18, 82)
)

# Previsão pontual
predict <- predict(modelo_3, novas.preditoras, interval = "confidence")
print(predict)