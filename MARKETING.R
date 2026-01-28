rm(list = ls())
install.packages("readxl")  # Install if not already installed
library(readxl)

#QUESTION 1 
dataset <- read_excel("dataset_deodorant.xlsx")

dataset$Q1 <- ifelse(dataset$QUARTER == 1, 1, 0)
dataset$Q4 <- ifelse(dataset$QUARTER == 4, 1, 0)
dataset$log_NIVEASales <- log(dataset$NIVEASales)
dataset$log_NIVEAPrice <- log(dataset$NIVEAPrice)
View(dataset)

#SEARCHING FOR MISSING VALUES
missing_values <- sum(is.na(dataset))
missing_values
colSums(is.na(dataset))
str(dataset)

#BOXPLOTING FOR SALES
sales_data <- dataset[, 4:11]
boxplot(sales_data, main = "Sales BoxPlot", 
        las = 2, col = "lightblue")

#BOXPLOTING FOR PRICE
price_data <- dataset[, 12:19]
boxplot(price_data, main = "Price BoxPlot", 
        las = 2, col = "lightblue")

#BOXPLOTING FOR REGULAR PRICE
regular_price_data <- dataset[, 20:27]
boxplot(regular_price_data, main = "Price BoxPlot", 
        las = 2, col = "lightblue")

#BOXPLOTING FOR DISP
disp_data <- dataset[, 28:35]
boxplot(disp_data, main = "Price BoxPlot", 
        las = 2, col = "lightblue")

#BOXPLOTING FOR FEAT
feat_data <- dataset[, 36:43]
boxplot(feat_data, main = "Price BoxPlot", 
        las = 2, col = "lightblue")

#BOXPLOTING FOR DF
df_data <- dataset[, 44:50]
boxplot(df_data, main = "Price BoxPlot", 
        las = 2, col = "lightblue")



#FINDING INCONSISTENCIES FOR PRICES VALUES LESS THAN ZERO
# Identificamos los valores que son 0
price_data <- dataset[, 12:19]
regular_price_data <- dataset[, 20:27]

price_data[price_data < 1] <- NA
regular_price_data[regular_price_data < 1] <- NA

dataset[, 12:19] <- price_data
dataset[, 20:27] <- regular_price_data

#Boxplotting again
boxplot(price_data, main = "Price BoxPlot", 
        las = 2, col = "lightblue")

boxplot(regular_price_data, main = "Price BoxPlot", 
        las = 2, col = "lightblue")




#CODIGO AJUSTADO
# Carga las librerías necesarias
library(tidyverse)

# Asegúrate de que la columna "QUARTER" sea de tipo numérico si es necesario
dataset$QUARTER <- as.factor(dataset$QUARTER)

# Muestra la estructura de los datos para verificar que todo esté correcto
str(dataset)

# Transformación de datos: pivotea el dataframe para convertir los productos en una sola columna
dataset_long <- dataset %>%
  pivot_longer(cols = c("DOVESales", "FASales", "NIVEASales", "REXONASales", "SANEXSales", 
                        "VOGUESales", "@8X4Sales", "AXESales"), 
               names_to = "product", 
               values_to = "sales") %>%
  mutate(Chain = as.factor(Chain), 
         product = gsub("Sales", "", product)) # Limpia los nombres de productos

# Grafica las ventas por cadena y por trimestre (QUARTER)
# Gráfico de barras con un bin para cada marca dentro de cada trimestre
ggplot(dataset_long, aes(x = QUARTER, y = sales, fill = product)) +  
  geom_bar(stat = "identity", position = "dodge") +  # Bars separated by product
  facet_wrap(~Chain, scales = "fixed") +  # All facets with the same Y-axis scale
  labs(title = "Sales by Brand Throughout the Year (by Quarter)",
       x = "Quarter",
       y = "Sales",
       fill = "Product") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

#CODIGO PARA VER LA ESTACIONALIDAD DE TODAS LAS MARCAS
ggplot(dataset_long, aes(x = QUARTER, y = sales)) +  
  geom_bar(stat = "identity", position = "stack", fill = "steelblue") +  # Single bin per quarter
  facet_wrap(~Chain, scales = "fixed") +  # Same Y-axis scale across facets
  labs(title = "Total Sales Throughout the Year (by Quarter)",
       x = "Quarter",
       y = "Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))




#CREATING A POOLING MODEL
model1 <- lm(NIVEASales ~ NIVEAPrice + NIVEADISP + NIVEAFEAT + NIVEADF, data = dataset)
summary(model1)

model2 <- lm(NIVEASales ~ NIVEAPrice + NIVEADISP + NIVEAFEAT,  data = dataset)
summary(model2) #QUITAR EL INTERACTION TERM XQ NO ES VARIABLE 

model3 <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + NIVEADISP + NIVEAFEAT + NIVEADF, data = dataset)
summary(model3) #ESTE ES EL MODELO BUENO 

library('gap')
cor(dataset[, c("NIVEAPrice", "NIVEARPrice", "NIVEADISP", "NIVEAFEAT", "NIVEADF")])
library(car)
vif(lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + NIVEADISP + NIVEAFEAT + NIVEADF, data = dataset))


model3 <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + NIVEADISP + NIVEAFEAT + NIVEADF, data = dataset)
summary(model3) #DE MOMENTO NOS INCLINAMOS POR ESTE MODELO



------------------------------------------------------------------------------------------
#CREATING A NONPOOLING MODEL AND A POOLING MODEL
df_list <- split(dataset, dataset$Chain)
head(df_list)

model4 <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + NIVEADISP + NIVEAFEAT + NIVEADF, data = df_list$`ALBERT HEIJN`)
summary(model4)

model5 <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + NIVEADISP + NIVEAFEAT + NIVEADF, data = df_list$`C-1000`)
summary(model5)

model6 <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + NIVEADISP + NIVEAFEAT + NIVEADF, data = df_list$EDAH)
summary(model6)

model7 <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + NIVEADISP + NIVEAFEAT + NIVEADF, data = df_list$JUMBO)
summary(model7)

model8 <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + NIVEADISP + NIVEAFEAT + NIVEADF, data = df_list$`SUPER DE BOER`)
summary(model8)

RSS4<-sum(residuals(model4)^2)
RSS5<-sum(residuals(model5)^2)
RSS6<-sum(residuals(model6)^2)
RSS7<-sum(residuals(model7)^2)
RSS8<-sum(residuals(model8)^2)

RSS_up <- RSS4 + RSS5 + RSS6 + RSS7 + RSS8
df_up <- 5*(123-6)

#HACEMOS LO MISMO PARA NUESTRO POOLED MODEL
RSS3<-sum(residuals(model3)^2)
RSS_p <- RSS3
df_p <- 5*123-6 # N * T - k

# Chow test
F <- ((RSS_p - RSS_up)/(df_p - df_up))/(RSS_up/df_up)


pf(F, (df_p-df_up), df_up, lower.tail=F)


print(F)


#WE CONCLUDE THAT IS BETTER TO NOT POOL THE DATA SO WE WILL CREATE A REGRESSION MODEL FOR EACH OF THE MODELS
model_albert_heijn <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + NIVEADISP, data = df_list$`ALBERT HEIJN`)
summary(model_albert_heijn)


model_C1000 <- lm(NIVEASales ~  NIVEADISP + NIVEADF, data = df_list$`C-1000`)
summary(model_C1000)

model_EDAH <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice, data = df_list$EDAH)
summary(model_EDAH)

model_JUMBO <- lm(NIVEASales ~ NIVEAPrice, data = df_list$JUMBO)
summary(model_JUMBO)

model_SUPER_DE_BOER <- lm(NIVEASales ~ NIVEAPrice, data = df_list$`SUPER DE BOER`)
summary(model_SUPER_DE_BOER)


#--------------------------------------------------------------------------------------
#we will try to consider the seasonality by using the quarter column 
#according to our analysis the best quarters are 1 and 4 so we consider both as dummy 
#variables

#POOLED MODEL SEASONLAITY
model_seasonality <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + NIVEADISP + NIVEAFEAT + NIVEADF + Q1 + Q4, data = dataset)
summary(model_seasonality)

#NOW WE DOO THE SAME BY UNPOOLING
model_albert_season <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + NIVEADISP + Q1 + Q4, data = df_list$`ALBERT HEIJN`)
summary(model_albert_season)

model_C_Season <- lm(NIVEASales ~ NIVEADISP + NIVEADF + Q1 + Q4, data = df_list$`C-1000`)
summary(model_C_Season)

model_Edah_Season <- lm(NIVEASales ~ NIVEAPrice + NIVEARPrice + Q1 + Q4, data = df_list$EDAH)
summary(model_Edah_Season)

model_Jumbo_Season <- lm(NIVEASales ~ NIVEAPrice + Q4 + NIVEAPrice*Q4, data = df_list$JUMBO)
summary(model_Jumbo_Season) #Impact only in Q4

model_super_season <- lm(NIVEASales ~ NIVEAPrice + Q1 + Q4 + NIVEAPrice*Q1 + NIVEAPrice*Q4, data = df_list$`SUPER DE BOER`)
summary(model_super_season)




#--------------------------------------------------------------------------------------
#Now lets apply logarithms to the sales and to the price
#We create a pooled model

model_seasonality_log <- lm(log_NIVEASales ~ log_NIVEAPrice + NIVEADISP + NIVEAFEAT + NIVEADF + Q1 + Q4, data = dataset)
summary(model_seasonality_log)

#Bad results so far, lets try unpooling
model_albert_season_log <- lm(log_NIVEASales ~ log_NIVEAPrice + NIVEADISP + Q1 + Q4, data = df_list$`ALBERT HEIJN`)
summary(model_albert_season_log)

model_C_Season_log <- lm(log_NIVEASales ~ NIVEADISP + NIVEADF + Q1 + Q4, data = df_list$`C-1000`)
summary(model_C_Season_log)

model_Edah_Season_log <- lm(log_NIVEASales ~ log_NIVEAPrice + NIVEARPrice + Q1 + Q4, data = df_list$EDAH)
summary(model_Edah_Season_log)

model_Jumbo_Season_log <- lm(log_NIVEASales ~ log_NIVEAPrice + Q4 + NIVEAPrice*Q4, data = df_list$JUMBO)
summary(model_Jumbo_Season_log) 

model_super_season_log <- lm(log_NIVEASales ~ log_NIVEAPrice, data = df_list$`SUPER DE BOER`)
summary(model_super_season_log)

vif(model_C_Season_log)







#LETS DO WHAT RUSLANKA DID

model_rus <- lm(log(NIVEASales) ~ log(NIVEAPrice) + log(NIVEARPrice) + NIVEADISP + NIVEAFEAT + NIVEADF + Q1 + Q4, data = dataset)
summary(model_rus)

library(car)
vif(model_rus)

#UNPOOLING
model_albert_season_2 <- lm(log(NIVEASales) ~ log(NIVEAPrice) + log(NIVEARPrice) + NIVEADISP + NIVEAFEAT + NIVEADF + Q1 + Q4, data = df_list$`ALBERT HEIJN`)
summary(model_albert_season_2)

model_C_Season_2 <- lm(log(NIVEASales) ~ log(NIVEAPrice) + log(NIVEARPrice) + NIVEADISP + NIVEAFEAT + NIVEADF + Q1 + Q4, data = df_list$`C-1000`)
summary(model_C_Season_2)

model_Edah_Season_2 <- lm(log(NIVEASales) ~ log(NIVEAPrice) + log(NIVEARPrice) + NIVEADISP + NIVEAFEAT + NIVEADF + Q1 + Q4, data = df_list$EDAH)
summary(model_Edah_Season_2)

model_Jumbo_Season_2 <- lm(log(NIVEASales) ~ log(NIVEAPrice) + log(NIVEARPrice) + NIVEADISP + NIVEAFEAT + NIVEADF + Q1 + Q4, data = df_list$JUMBO)
summary(model_Jumbo_Season_2) #Impact only in Q4

model_super_season_2 <- lm(log(NIVEASales) ~ log(NIVEAPrice) + log(NIVEARPrice) + NIVEADISP + NIVEAFEAT + NIVEADF + Q1 + Q4, data = df_list$`SUPER DE BOER`)
summary(model_super_season_2)

