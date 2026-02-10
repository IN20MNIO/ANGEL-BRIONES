
# Lectura y documentacion inicial -----------------------------------------

library(corrplot)
unlist(lapply(c('haven', 'magrittr', 'data.table', 'stringr', 'clipr', 'vtable','readxl', 'ggplot2','dplyr'), require, character.only = TRUE))

#cargamos los archivos

df1 <- read_excel("/home/angsmb16/R/Modulo_Mineria/countries_info.xlsx")
df2 <- read.csv("/home/angsmb16/R/Modulo_Mineria/happiness2020.csv")

#Conbinamos ambos archivos

# Convertimos ambas columnas a minúsculas
df2$country <- tolower(df2$country)
df1$country_name <- tolower(df1$country_name)

# Ahora el merge funcionará perfecto
df <- merge(
  df2,
  df1,
  by.x = "country",
  by.y = "country_name",
  all.x = TRUE
)
#¿Cuántas observaciones y variables hay?
df %>% nrow()
#135
df %>% length()
#11

#¿Cuántas variables son categóricas y cuántas numéricas?

str(df)
# 9 numericas y 2 categoricas, aunque cuente "literacy" como chr
# realmente es numerica.

#¿Hay alguna variable que pueda ser un ID?

unique(df$country) %>% length() #Pais : 135
unique(df$area) %>% length() #Area : 135
unique(df$population) %>% length() #poblacion : 135

# observando podemos ver queson candiatos para ser id, sin embargo me
# gustaria crear un id alfanumerico corto 

df$ID <- paste0(
  substr(df$country, 1, 3), substr(df$area, 1, 5)    
)
df$ID <- toupper(df$ID)

unique(df$ID) %>% length() #ID : 135


# Diagnostico exploratorio --------------------------------------------------------------

#Describe al menos 5 variables numéricas con media, min y max.

df$healthy_life_expectancy %>% mean(na.rm = TRUE) # 64.76249
df$healthy_life_expectancy %>% min(na.rm = TRUE) # 48.00362
df$healthy_life_expectancy %>% max(na.rm = TRUE) # 76.80458

df$area %>% mean(na.rm = TRUE) # 900782.9
df$area %>% min(na.rm = TRUE) #  316
df$area %>% max(na.rm = TRUE) # 17075200

df$population %>% mean(na.rm = TRUE) # 45522038
df$population %>% min(na.rm = TRUE) #   299388
df$population %>% max(na.rm = TRUE) # 1313973713

df$happiness_score %>% mean(na.rm = TRUE) # 5.525062
df$happiness_score %>% min(na.rm = TRUE) # 2.5669
df$happiness_score %>% max(na.rm = TRUE) # 7.8087

df$perception_of_corruption %>% mean(na.rm = TRUE) # 0.7274473
df$perception_of_corruption %>% min(na.rm = TRUE) # 0.1097842
df$perception_of_corruption %>% max(na.rm = TRUE) # 0.9355851

#para las variables categoricas realmente solo hay una que podemos utilizar 
#y es word_region, aunque que country es una variable categorica la frecuencia
#es 1 para todos.

freq_word_region <- df$world_region %>% table() %>% as.data.frame()
# Sub-Saharan Africa: 32


# Conclusiones exploratorias ----------------------------------------------

#Frecuencia de paises 
ggplot(freq_word_region, aes(x = ., y = Freq)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Frecuencia por Región",
       x = "Región",
       y = "Frecuencia")

#Area por region del mundo
area_por_region <- df %>%
  group_by(world_region) %>%
  summarise(Total_Area = sum(area, na.rm = TRUE)) %>%
  arrange(desc(Total_Area))

ggplot(area_por_region, aes(x = world_region, y = Total_Area)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Area total por region",
       x = "Región",
       y = "Área")

#Poblacion por region del mundo
poblacion_por_region <- df %>%
  group_by(world_region) %>%
  summarise(total_population = sum(population, na.rm = TRUE)) %>%
  arrange(desc(total_population))
print(poblacion_por_region)

ggplot(poblacion_por_region, aes(x = world_region, y = total_population)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Area total por region",
       x = "Region",
       y = "Poblacion")

#Densidad de poblacion

densidad_region <- df %>%
  group_by(world_region) %>%
  summarise(
    Total_Area = sum(area, na.rm = TRUE),
    Total_Pop = sum(population, na.rm = TRUE),
    Densidad = Total_Pop / Total_Area
  ) %>% arrange(desc(Densidad))

ggplot(densidad_region, aes(x = reorder(world_region, -Densidad), y = Densidad)) +
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(Densidad, 1)), vjust = -0.5, size = 3.5) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + theme_minimal() +
  labs(
    title = "Densidad de Población por Región Mundial",
    subtitle = "Habitantes por unidad de área",
    x = "Región",
    y = "Densidad (Hab/Área)"
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Matriz de correlacion

df_numerico <- df[sapply(df, is.numeric)]
matriz_cor <- cor(df_numerico, use = "complete.obs", method = "pearson")
print(matriz_cor)

corrplot(matriz_cor, 
         method = "color", 
         type = "full", 
         addCoef.col = "black",
         number.cex = 0.7,
         tl.cex = 0.8,
         tl.srt = 45, 
         diag = FALSE)

# Parte 2: Preprocesamiento -----------------------------------------------

# NA ----------------------------------------------------------------------
#Visualiza y analiza si los datos faltan al azar.
#Identifica los NA con is.na() o librerías como naniar.
#En caso necesario, imputa al menos dos variables con técnicas diferentes (media, mediana, moda, o categórica). Justifica tu decisión.
#En caso de que no haya NAs, añade evidencia.

library(dplyr)
library(data.table)
library(imputeTS)
library(zoo)
library(naniar)
#verificamos si tiene na
df %>% naniar::vis_miss(., warn_large_data = F)

#contamos los na
df %>% is.na() %>% sum()

# Imputar valores faltantes con la media,calculamos la media de literacy por region ya que seria buen idea imputar las medias
#de cada region asi evitamos globalizar todo

df_limpio <- df %>%
  mutate(literacy = as.numeric(gsub(",", ".", literacy))) %>%
  group_by(world_region) %>%
  mutate(
    literacy = ifelse(is.na(literacy), 
                      mean(literacy, na.rm = TRUE), 
                      literacy)
  ) %>% ungroup()

df_limpio %>% is.na() %>% sum()


# Outliers -----------------------------------------------------------------
#Usa boxplots, histogramas o scatterplots para detectar outliers en al menos 5 variables.
#Identifica filas con outliers y toma decisiones: imputar, transformar o eliminar (al menos 5 variables).
#En caso de que no haya outliers, añade evidencia

library(tidyr)

# Función para contar outliers 
contar_outliers <- function(x) {
  if(is.numeric(x)) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lim_inf <- q1 - 1.5 * iqr
    lim_sup <- q3 + 1.5 * iqr
    return(sum(x < lim_inf | x > lim_sup, na.rm = TRUE)
           )
  } else {
    return(NA)
  }
}

#las columnas a las que vamos a quitar outliers "social_support", "freedom_of_choices", 
#"generosity", "perception_of_corruption", "literacy"

#Funcion mostrar outliers
mostrar_outliers <- function(x) {
  if(is.numeric(x)) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lim_inf <- q1 - 1.5 * iqr
    lim_sup <- q3 + 1.5 * iqr
    
    outliers <- df_limpio %>%
      filter(x < lim_inf | x > lim_sup)
    return(outliers)
  } else {
    return(NA)
  }
}


#En total hay 5 colunmnas que representan los outlieres de 
#"social_support" = 2, "freedom_of_choices" = 2, 
#"generosity" = 1 , "literacy " =2 , perception_of_corruption = 13 
#intentaremos remplazar con la media

replace_outliers <- function(column, method = "mean") {
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  IQR <- Q3 - Q1
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  # Calcular reemplazo según el método
  if (method == "mean") {
    replacement <- mean(column, na.rm = TRUE)
  }  else {
    stop("Método no reconocido. Usa 'mean' o 'median'.")
  }
  
  # Reemplazar valores atípicos
  column[column < lower_limit | column > upper_limit] <- replacement
  return(column)
}

df_limpio$generosity <- replace_outliers(df_limpio$generosity, method = "mean")
df_limpio$freedom_of_choices <- replace_outliers(df_limpio$freedom_of_choices, method = "mean")
df_limpio$social_support <- replace_outliers(df_limpio$social_support, method = "mean")
df_limpio$perception_of_corruption <- replace_outliers(df_limpio$perception_of_corruption, method = "mean")
df_limpio$literacy <- replace_outliers(df_limpio$literacy, method = "mean")

sapply(df_limpio, contar_outliers)
#En total hay 5 colunmnas que representan los outlieres de 
#"social_support" = 1, "freedom_of_choices" = 0, 
#"generosity" = 1 , "literacy " =1 , perception_of_corruption = 4 
nrow(df_limpio)
#135

df_clean <- df_limpio %>%
  filter(
    perception_of_corruption >= quantile(perception_of_corruption, 0.25) - 1.5 * IQR(perception_of_corruption),
    perception_of_corruption <= quantile(perception_of_corruption, 0.75) + 1.5 * IQR(perception_of_corruption),
    social_support >= quantile(social_support, 0.25) - 1.5 * IQR(social_support),
    social_support <= quantile(social_support, 0.75) + 1.5 * IQR(social_support)
    )

df_clean$generosity <- replace_outliers(df_clean$generosity, method = "mean")

sapply(df_clean, contar_outliers)
#Aunque no eh podido eliminar todos los outliers puesto que si elimino uno o pongo
#la media ya que al moverlos salen nuevos outliers puedo trabajar con ello 
#dejando un outlier para literacy
nrow(df_clean)
#130

# Duplicados en ID --------------------------------------------------------
#Visualiza posibles duplicaciones.
#Detecta y elimina duplicados exactos con duplicated().
#Encuentra duplicados parciales usando combinaciones de columnas relevantes.
#En caso de que no haya duplicados, añade evidencia.

ids_duplicados <- df_clean$ID[duplicated(df_clean$ID)]
print(ids_duplicados)


# Parte 3.1 clasificacion -------------------------------------------------

#Escoge una variable objetivo categórica relevante.
#Codifica adecuadamente las variables predictoras.
#Divide el dataset en 70% entrenamiento y 30% prueba.
#Ajusta los siguientes modelos:
  #Regresión logística
  #Naive Bayes
  #SVM
#Evalúa con:
  
#Matriz de confusión
#Precisión y recall
#Curva ROC y AUC
#Importancia de variables

#La variable categorica que voy a crear es con la happiness_score
df_clean$happiness_level <- cut(
  df_clean$happiness_score,
  breaks = quantile(df_clean$happiness_score, probs = c(0,0.5, 1)),
  labels = c("Baja", "Alta"),
  include.lowest = TRUE
)

df_clean$happiness_level <- factor(df_clean$happiness_level)

# Selección de variables numéricas relevantes

predictors <- c(
  "social_support",
  "healthy_life_expectancy",
  "freedom_of_choices",
  "generosity",
  "perception_of_corruption"
)


# Selección de variables para el modelo

model_df <- df_clean[, c("happiness_level",
                   "social_support",
                   "healthy_life_expectancy",
                   "freedom_of_choices",
                   "generosity",
                   "perception_of_corruption")]

model_df <- na.omit(model_df)

# División train / test

set.seed(123)
ind <- sample(seq_len(nrow(model_df)), size = 0.7 * nrow(model_df))

train_data <- model_df[ind, ]
test_data  <- model_df[-ind, ]

library(e1071)  
library(caret)
library(pROC) 

# ======================================================================
# Regresión Logística
# ======================================================================

modelo_logit <- glm(
  happiness_level ~ .,
  data = train_data,
  family = binomial
)

modelo_logit <- glm(
  happiness_level ~  social_support + healthy_life_expectancy + freedom_of_choices + generosity + perception_of_corruption,
  data = train_data,
  family = binomial
)

# Predicciones
test_data$logit_prob <- predict(
  modelo_logit,
  newdata = test_data,
  type = "response"
)

test_data$logit_pred <- factor(
  ifelse(test_data$logit_prob >= 0.5, "Alta", "Baja"),
  levels = levels(test_data$happiness_level)
)

# ROC y AUC
logit_roc <- roc(test_data$happiness_level, test_data$logit_prob)
logit_auc <- auc(logit_roc)


plot(logit_roc, 
     col = "#377EB8",           
     lwd = 3,                   
     main = "Curva ROC - Regresión Logística", 
     xlab = "Especificidad (1 - Tasa Falsos Positivos)", 
     ylab = "Sensibilidad (Tasa Verdaderos Positivos)")

abline(a = 1, b = -1, lty = 2, col = "gray")

plot(logit_roc, 
     print.auc = TRUE,           
     auc.polygon = TRUE,         
     auc.polygon.col = "#D1E5F0",
     grid = TRUE,                
     main = "Evaluación del Modelo: Curva ROC y AUC")

# ======================================================================
# Naive Bayes
# ======================================================================

modelo_nb <- naiveBayes(happiness_level ~  social_support + healthy_life_expectancy + freedom_of_choices + generosity + perception_of_corruption, data = train_data)
test_data$nb_pred <- predict(modelo_nb, newdata = test_data)

nb_prob <- predict(modelo_nb, newdata = test_data, type = "raw")[, "Alta"]
nb_roc <- roc(test_data$happiness_level, nb_prob)
nb_auc <- auc(nb_roc)

# ======================================================================
# SVM
# ======================================================================

modelo_svm <- svm(happiness_level ~  social_support + healthy_life_expectancy + freedom_of_choices + generosity + perception_of_corruption, data = train_data)
test_data$svm_pred <- predict(modelo_svm, newdata = test_data)

svm_num_pred <- as.numeric(test_data$svm_pred) - 1
svm_num_true <- as.numeric(test_data$happiness_level) - 1

svm_roc <- roc(svm_num_true, svm_num_pred)
svm_auc <- auc(svm_roc)


# ======================================================================
# Evaluación de modelos
# ======================================================================

confusionMatrix(test_data$logit_pred, test_data$happiness_level)
confusionMatrix(test_data$nb_pred, test_data$happiness_level)
confusionMatrix(test_data$svm_pred, test_data$happiness_level)

cat("AUC Regresión Logística:", logit_auc, "\n")
cat("AUC Naive Bayes:", nb_auc, "\n")
cat("AUC SVM:", svm_auc, "\n")

# ======================================================================
# Visualización de predicciones
# ======================================================================

visualizar_predicciones <- function(data, pred_column, model_name) {
  ggplot(data, aes(
    x = social_support,             
    y = healthy_life_expectancy,    
    color = .data[[pred_column]]    
  )) +
    geom_point(alpha = 0.7, size = 3) + 
    scale_color_manual(values = c("Baja" = "#E41A1C", "Alta" = "#377EB8")) +
    labs(
      title = paste("Predicciones de Felicidad –", model_name),
      subtitle = "Clasificación basada en Apoyo Social y Expectativa de Vida",
      x = "Apoyo Social (Social Support)",
      y = "Expectativa de Vida Saludable",
      color = "Nivel Predicho"
    ) +
    theme_minimal()
}

visualizar_predicciones(test_data, "logit_pred", "Regresión Logística")
visualizar_predicciones(test_data, "nb_pred", "Naive Bayes")
visualizar_predicciones(test_data, "svm_pred", "SVM")

# Importancia relativa
modelo_logit$coefficients %>% barplot()

resultado_logit <- confusionMatrix(test_data$logit_pred, test_data$happiness_level, positive = "Alta", mode = "everything")

#Precisión y Recall
print(resultado_logit$byClass["Precision"])
print(resultado_logit$byClass["Recall"])


# Parte 3.2 - Práctica ----------------------------------------------------
#7. Clasificación:
  
#Escoge una variable objetivo categórica relevante.
#Codifica adecuadamente las variables predictoras.
#Divide el dataset en 70% entrenamiento y 30% prueba.
#Ajusta los siguientes modelos:
#Árbol de decisión
#Regresión multinomial
#CHAID
#Evalúa con:
#
#Matriz de confusiónPrecisión y recallCurva ROC y AUCImportancia de variable


# ======================================================================
# Creación de variables 
# ======================================================================

#La variable categorica que voy a crear es con la happiness_score sera la misma que use
#anteriormente pero ahora con 3

df_clean$happiness_level <- cut(
  df_clean$happiness_score,
  breaks = quantile(df_clean$happiness_score, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("Triste", "Moderado", "Muy feliz"),
  include.lowest = TRUE
)
library(data.table)
setDT(df_clean)
# Convertir country a factor
df_clean[, country := factor(country)]

# Crear categoría de healthy_life_expectancy
df_clean$life_expectancy <- cut(
  df_clean$healthy_life_expectancy,
  breaks = quantile(df_clean$healthy_life_expectancy, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("Bajo", "Medio", "Longevo"),
  include.lowest = TRUE
)

# Crear categoría de population
df_clean$population_range <- cut(
  df_clean$population,
  breaks = quantile(df_clean$population, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("Poco poblado", "Semi poblado", "Muy poblado"),
  include.lowest = TRUE
)

#df_clean$happiness_level <- NULL

#names(df_clean)[names(df_clean) == "Happiness_Level"] <- "happiness_level"

#-------------------------------------------------------------------------------
#Definimos una sola seed
set.seed(123)

# Selección de variables para el modelo y divicion

model_df1 <- df_clean[, c("country",
                         "social_support",
                         "healthy_life_expectancy",
                         "freedom_of_choices",
                         "generosity",
                         "perception_of_corruption",
                         "literacy",
                         "life_expectancy",
                         "world_region",
                         "population_range",
                         "happiness_level")]


model_df1 <- na.omit(model_df1)

# División train / test

ind1 <- sample(seq_len(nrow(model_df1)), size = 0.7 * nrow(model_df1))

train_data1 <- model_df1[ind1 ]
test_data1  <- model_df1[-ind1, ]

# Arbol de Decision -------------------------------------------------------

# Instalar y cargar el paquete necesario
# install.packages("rpart")
library(rpart)

# El conjunto que tengo que usar es 
#df_clean

# Ajustar el árbol de decisión
arbol_decision <- rpart(happiness_level ~ social_support + healthy_life_expectancy + freedom_of_choices + generosity + perception_of_corruption + literacy, data = train_data1, method = "class")
test_data1$arbol <- predict(arbol_decision, newdata = test_data1, type = "class")

# Instalar y cargar el paquete necesario
# install.packages("partykit")
library(partykit)
library(ggplot2)

# Convertir el árbol de decisión a un objeto de clase 'party'
arbol_party <- as.party(arbol_decision)

# Mostrar el gráfico del árbol de decisión
plot(arbol_party)

# CHAID -------------------------------------------------------------------

# Cargar el paquete partykit
# install.packages("partykit")
#install.packages("catdata")
library(catdata)
library(partykit)

# Cargar el conjunto de datos
#df_clean

# Verificar que df_clean es un df
is.data.frame(df_clean) #TRUE 

df_clean$happiness_level %>% table()
# Ajustar el modelo CHAID
modelo_chaid <- ctree(happiness_level ~ country + life_expectancy + world_region + population_range, data = train_data1)
test_data1$chaid <- predict(modelo_chaid, newdata = test_data1)

plot(modelo_chaid)


# ------------------------------------------------------------
# Multinomial Logistic Regression
# ------------------------------------------------------------
# Instalar y cargar los paquetes necesarios
# install.packages("nnet")
library(nnet)

modelo_multinom <- multinom(
  happiness_level ~ social_support + perception_of_corruption + healthy_life_expectancy + literacy,
  data = train_data1,
  na.action = na.omit
)

test_data1$multinomial <- predict(modelo_multinom, newdata = test_data1)
summary(modelo_multinom)

# ======================================================================
# Evaluación de modelos
# ======================================================================
library(e1071)  
library(caret)
library(pROC) 

confusionMatrix(test_data1$arbol, test_data1$happiness_level)
confusionMatrix(test_data1$chaid, test_data1$happiness_level)
confusionMatrix(test_data1$multinomial, test_data1$happiness_level)

cat("AUC Regresión Logística:", logit_auc, "\n")
cat("AUC Naive Bayes:", nb_auc, "\n")
cat("AUC SVM:", svm_auc, "\n")

#---------------------------------------------------------------------------

#Precisión y Recall

res_arbol <- confusionMatrix(test_data1$arbol, test_data1$happiness_level)

print("Métricas Árbol:")
print(res_arbol$byClass[, c("Precision", "Recall")])

res_chaid <- confusionMatrix(test_data1$chaid, test_data1$happiness_level)

print("Métricas CHAID:")
print(res_chaid$byClass[, c("Precision", "Recall")])


res_multi <- confusionMatrix(test_data1$multinomial, test_data1$happiness_level)

print("Métricas Modelo Multinomial:")
print(res_multi$byClass[, c("Precision", "Recall")])

#Importancia de variables

importancia <- summary(modelo_multinom)$coefficients[1, -1] 
barplot(importancia, main="Importancia Relativa - Multinomial", col="lightblue", las=2)

# ROC y AUC

library(pROC)

prob_multi <- predict(modelo_multinom, newdata = test_data1, type = "probs")

roc_multi <- roc(test_data1$happiness_level == "Muy feliz", prob_multi[, "Muy feliz"])


plot(roc_multi, 
     col = "green", 
     lwd = 3, 
     main = "Curva ROC - Modelo Multinomial (Clase: Muy Feliz)",
     xlab = "Especificidad (1 - Falsos Positivos)", 
     ylab = "Sensibilidad (Verdaderos Positivos)",
     print.auc = TRUE,      
     auc.polygon = TRUE,    
     grid = TRUE)           


print(paste("El AUC del modelo Multinomial es:", round(auc(roc_multi), 4)))
