require(readxl)
library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(hrbrthemes)
library(PerformanceAnalytics)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(anytime)
library(tidyr)
library(gridExtra)
library(GGally)
library(grid)

setwd("C:/Users/javi3/OneDrive/Escritorio/UVG/Business Intelligence and Analytics/Statistical Analysis/Proyecto_final")

datos <- read.csv('Bank_Loan_Granting.csv')

# Se debe limpiar la columna CCavg cambiando el '/' por un punto decimal

datos$CCAvg <- gsub('/', '.', datos$CCAvg)
datos$CCAvg <- as.numeric(datos$CCAvg)*1000

datos$Income <- (datos$Income/12)*1000
datos$Mortgage <- datos$Mortgage*1000
datos$Education <- as.factor(datos$Education)

datos$Personal.Loan <- as.character(datos$Personal.Loan)

datos <- datos %>%
  mutate(Mortgage_y = ifelse(Mortgage > 0, 'Y', 'N'))

datos <- datos %>%
  group_by(Personal.Loan)

grid.arrange(
  datos %>%
    group_by(Personal.Loan) %>% 
    tally() %>% 
    ggplot(aes(y = factor(Personal.Loan), x = n)) + 
    geom_bar(stat = "identity") + 
    geom_text(aes(label = n), hjust = -0.5) + 
    lims(x = c(0,7000)) + 
    ggtitle("Tamaño de los grupos")
  ,
  
  datos %>% 
    ggplot(aes(y = factor(Personal.Loan))) + 
    geom_boxplot(aes(x = Income)) + 
    ggtitle("Ingresos mensuales del cliente") +
    labs(y = "Aprobado"),
  
  datos %>% 
    ggplot(aes(y = factor(Personal.Loan))) + 
    geom_boxplot(aes(x = CCAvg)) + 
    ggtitle("Gasto promedio mensual de la tarjeta") +
    labs(y = "Aprobado"),
  
  datos %>% 
    ggplot(aes(y = factor(Personal.Loan))) + 
    geom_boxplot(aes(x = Age)) + 
    ggtitle("Distribución de edades") +
    labs(y = "Aprobado")
  
  , ncol = 2)


 grid.arrange(
  datos %>% 
    group_by(Personal.Loan) %>%
    ggplot(aes(y = factor(Personal.Loan))) + 
    geom_bar(aes(fill = as.character(Education)), position ="fill") + 
    ggtitle("Nivel de educación") +
    labs(y = "Grupo", x = "%")
  ,
  datos %>% 
    ggplot(aes(y = factor(Personal.Loan))) + 
    geom_bar(aes(fill = as.character(CreditCard)), position ="fill") + 
    ggtitle("Cuenta con tarjeta de crédito") +
    labs(y = "Grupo", x = "%")
  ,
  datos %>% 
    ggplot(aes(y = factor(Personal.Loan))) + 
    geom_bar(aes(fill = as.character(Online)), position ="fill") + 
    ggtitle("Uso del servicio en línea") +
    labs(y = "Grupo", x = "%")
  ,
  datos %>% 
    ggplot(aes(y = factor(Personal.Loan))) + 
    geom_bar(aes(fill = as.character(Securities.Account)), position = "fill") + 
    ggtitle("Conteo cuentas de seguridad") +
    labs(y = "Grupo", x = "%"),
  
  datos %>% 
    ggplot(aes(y = factor(Personal.Loan))) + 
    geom_bar(aes(fill = as.character(CD.Account)), position = "fill") + 
    ggtitle("Conteo cuentas de plazo fijo") +
    labs(y = "Grupo", x = "%"),
  
  datos %>% 
    ggplot(aes(y = factor(Personal.Loan))) + 
    geom_bar(aes(fill = as.character(Mortgage_y)), position = "fill") + 
    ggtitle("Conteo de personas con garantía para el banco") +
    labs(y = "Grupo", x = "%")
  
  , ncol = 2)

datos <- datos %>%
  ungroup()

numeric_values <- datos %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-c(ID, ZIP.Code, Securities.Account, Online, CreditCard, CD.Account))

cor_m <- cor(numeric_values)

ggpairs(cor_m)

# Verificar si hay una correlación más significativa en las personas con 
# crédito aprobado que las personas que no entre el income y CCavg

corr_y <- datos %>%
  filter(Personal.Loan == "1") %>%
  dplyr::select(Income, CCAvg)

corr_n <- datos %>%
  filter(Personal.Loan == "0") %>%
  dplyr::select(Income, CCAvg)

cor_Loan <- cor(corr_y)

ggplot(corr_y, aes(x=Income, y=CCAvg)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()

cor_No_loan <- cor(corr_n)

ggplot(corr_n, aes(x=Income, y=CCAvg)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()

# Prueba Chi cuadrado

tabla1 <- table(datos$Personal.Loan,datos$Mortgage_y)
tabla1

mosaicplot(tabla1)

chisq.test(tabla1)

# Diferencia de medias

t.test(datos$CCAvg ~ datos$Personal.Loan)

# Anova

anova1 <- aov(Income ~ Education, data=datos)

## veamos el resumen del an?lisis

summary(anova1)

tuk <- TukeyHSD(anova1)

tuk

## aqu? se puede observar que la ?nica pareja de marcas que no presentan
## una diferencia significativa son CEAT - Apollo, todas las dem?s son
## diferentes entre s?.

## visualicemos los intervalos de confianza de las diferencias

par(oma=c(0,5,0,0))  ## cambiar los m?rgenes de la gr?fica

plot(tuk,las=1,col="red")  ## graficar los intervalos

par(oma=c(0,0,0,0))  # regresar los m?rgenes a sus valores normales

