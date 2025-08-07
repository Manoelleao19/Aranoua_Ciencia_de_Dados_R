# Instalação de pacotes (se necessário)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, caret, pROC, ggplot2, plotly, GGally)

# Carregar a base de dados
data(GermanCredit, package = "caret")

## Parte 1: Análise Exploratória de Dados (AED) ----

# 1. Dimensões da base
cat("Número de observações:", nrow(GermanCredit), "\n")
cat("Número de variáveis:", ncol(GermanCredit), "\n")
#Número de observações: 1000 
#Número de variáveis: 62

# 2. Tipos de variáveis
str(GermanCredit)

# 3. Distribuição da variável resposta
table(GermanCredit$Class)
prop.table(table(GermanCredit$Class))

# 4. Gráfico de barras da variável resposta
ggplot(GermanCredit, aes(x = Class, fill = Class)) +
  geom_bar() +
  labs(title = "Distribuição da Variável Resposta (Class)",
       x = "Classificação de Crédito",
       y = "Contagem") +
  theme_minimal()

# 5. Boxplots para três variáveis numéricas
# usar Duration, Amount e Age
ggplot(GermanCredit, aes(x = Class, y = Duration, fill = Class)) +
  geom_boxplot() +
  labs(title = "Duração do Empréstimo por Classificação")

ggplot(GermanCredit, aes(x = Class, y = Amount, fill = Class)) +
  geom_boxplot() +
  labs(title = "Valor do Empréstimo por Classificação")

ggplot(GermanCredit, aes(x = Class, y = Age, fill = Class)) +
  geom_boxplot() +
  labs(title = "Idade do Cliente por Classificação")



## Parte 2: Preparação dos dados ----

# 1. Divisão treino-teste
set.seed(123)
train_index <- createDataPartition(GermanCredit$Class, p = 0.7, list = FALSE)
train_data <- GermanCredit[train_index, ]
test_data <- GermanCredit[-train_index, ]

# 2. Verificação de balanceamento
cat("\nProporção na base completa:\n")
prop.table(table(GermanCredit$Class))

cat("\nProporção na base de treino:\n")
prop.table(table(train_data$Class))

cat("\nProporção na base de teste:\n")
prop.table(table(test_data$Class))

## Parte 3: Ajuste do modelo ----

# Configurar validação cruzada
ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = TRUE
)

# Ajustar modelo de Regressão Logística
model <- train(
  Class ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

# Visualizar resumo do modelo
print(model)
summary(model$finalModel)

## Parte 4: Avaliação do modelo ----

# 1. Previsões nos dados de teste
predictions <- predict(model, newdata = test_data)
probabilities <- predict(model, newdata = test_data, type = "prob")

# 2. Matriz de confusão e métricas
conf_matrix <- confusionMatrix(predictions, test_data$Class, positive = "Good")
print(conf_matrix)

# Extraindo métricas específicas
metrics <- data.frame(
  Acurácia = conf_matrix$overall["Accuracy"],
  Sensibilidade = conf_matrix$byClass["Sensitivity"],
  Especificidade = conf_matrix$byClass["Specificity"],
  F1 = conf_matrix$byClass["F1"]
)
print(metrics)

# 3. Curva ROC
roc_curve <- roc(test_data$Class, probabilities$Good)
plot(roc_curve, print.auc = TRUE, main = "Curva ROC")

# 4. Interpretação dos resultados
cat("\nInterpretação:\n")
cat("O modelo apresentou uma acurácia de", round(metrics$Acurácia, 3), "\n")
cat("Sensibilidade (capacidade de detectar bons pagadores):", round(metrics$Sensibilidade, 3), "\n")
cat("Especificidade (capacidade de detectar maus pagadores):", round(metrics$Especificidade, 3), "\n")
cat("Área sob a curva ROC:", round(roc_curve$auc, 3), "\n\n")

if (roc_curve$auc > 0.8) {
  cat("O modelo teve excelente desempenho na discriminação entre as classes.")
} else if (roc_curve$auc > 0.7) {
  cat("O modelo teve bom desempenho, mas pode ser melhorado.")
} else {
  cat("O desempenho do modelo foi abaixo do esperado. Considere:")
  cat("\n- Engenharia de features adicionais")
  cat("\n- Testar outros algoritmos (Random Forest, XGBoost)")
  cat("\n- Lidar com possíveis problemas de desbalanceamento")
}

# Visualização adicional: importância das variáveis
var_imp <- varImp(model)
plot(var_imp, main = "Importância das Variáveis")



# Matriz de correlação para variáveis numéricas
numeric_data <- GermanCredit %>% select_if(is.numeric)
ggcorr(numeric_data, label = TRUE)

# Gráfico interativo de dispersão
plot_ly(GermanCredit, x = ~Duration, y = ~Amount, color = ~Class, 
        type = "scatter", mode = "markers")