# Carregando pacotes necessários
library(MASS)       # Contém a base Boston
library(tidyverse)  # Para manipulação e visualização
library(mgcv)       # Para modelos GAM
library(glmnet)     # Para regularização LASSO
library(caret)      # Para modelagem preditiva

# 1. Carregando a base de dados
data(Boston)

# 2. Tipo e estrutura dos dados
str(Boston)

# 3. Número de observações e variáveis
cat("Número de observações:", nrow(Boston), "\n")
cat("Número de variáveis:", ncol(Boston), "\n")

# Visualizando as primeiras linhas
head(Boston)

# 5. Estatísticas do preço médio das casas (medv)
summary(Boston$medv)
cat("Desvio padrão de medv:", sd(Boston$medv), "\n")

# 6. Média do número de quartos (rm)
mean(Boston$rm)

# 7. Distribuição da taxa de criminalidade (crim)
summary(Boston$crim)
hist(Boston$crim, main = "Distribuição da Taxa de Criminalidade", 
     xlab = "Taxa de Criminalidade", col = "lightblue")

# 8. Histograma do preço médio das casas
ggplot(Boston, aes(x = medv)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribuição do Preço Médio das Casas",
       x = "Preço Médio (em $1,000)",
       y = "Frequência") +
  theme_minimal()

# 9. Scatterplot entre rm e medv
ggplot(Boston, aes(x = rm, y = medv)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relação entre Número de Quartos e Preço das Casas",
       x = "Número Médio de Quartos",
       y = "Preço Médio (em $1,000)") +
  theme_minimal()

# 10. Boxplot de medv por chas
ggplot(Boston, aes(x = factor(chas), y = medv, fill = factor(chas))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Não", "Sim")) +
  labs(title = "Preço Médio das Casas por Proximidade ao Rio Charles",
       x = "Limita com o Rio Charles?",
       y = "Preço Médio (em $1,000)") +
  theme_minimal() +
  theme(legend.position = "none")

# 11. Modelo de regressão linear com rm e lstat
lm_model <- lm(medv ~ rm + lstat, data = Boston)
summary(lm_model)

# 12. Modelo GAM com suavização em lstat
gam_model <- gam(medv ~ rm + s(lstat), data = Boston)
summary(gam_model)

# 13. Interpretação do efeito de lstat no GAM
plot(gam_model, pages = 1, residuals = TRUE, pch = 19, cex = 0.25,
     shade = TRUE, shade.col = "lightblue", seWithMean = TRUE)

# 14. Comparação entre ajuste linear e suavizado
par(mfrow = c(1, 2))
plot(Boston$lstat, Boston$medv, main = "Ajuste Linear",
     xlab = "lstat", ylab = "medv")
abline(lm(medv ~ lstat, data = Boston), col = "red")
plot(gam_model, select = 1, main = "Ajuste Suavizado")

# 15. Seleção de variáveis com LASSO
# Preparando os dados
x <- model.matrix(medv ~ ., Boston)[,-1]  # Removendo intercepto
y <- Boston$medv

# Ajustando o modelo LASSO
set.seed(123)
cv_lasso <- cv.glmnet(x, y, alpha = 1)
plot(cv_lasso)

# Coeficientes selecionados
coef(cv_lasso, s = "lambda.min")

# Variáveis mais importantes
var_imp <- as.matrix(coef(cv_lasso, s = "lambda.min"))
var_imp <- data.frame(Variable = rownames(var_imp), 
                      Coefficient = var_imp[,1])
var_imp <- var_imp[var_imp$Coefficient != 0 & var_imp$Variable != "(Intercept)",]
var_imp <- var_imp[order(abs(var_imp$Coefficient), decreasing = TRUE),]

ggplot(var_imp, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variáveis Mais Importantes pelo LASSO",
       x = "Variável",
       y = "Coeficiente") +
  theme_minimal()


#Análise Descritiva:
#  O preço médio das casas varia entre $5,000 e $50,000
#  A média de quartos por habitação é de aproximadamente 6.3
#  A taxa de criminalidade apresenta distribuição altamente assimétrica

#Visualizações:
#  Há uma relação positiva clara entre número de quartos e preço das casas
#  Imóveis próximos ao rio Charles tendem a ser mais valorizados
#  A distribuição de preços é aproximadamente normal com leve assimetria à direita

#Modelagem:
#  O modelo linear mostra que tanto rm (positivo) quanto lstat (negativo) são significativos
#  O GAM revela uma relação não-linear entre lstat e o preço das casas
#  O LASSO identifica rm, lstat, ptratio e indus como variáveis mais importantes

#Conclusões:
#  O número de quartos e o status socioeconômico são os principais drivers de preço
#  A relação entre status socioeconômico e preço é não-linear
#  A proximidade com o rio Charles agrega valor aos imóveis
