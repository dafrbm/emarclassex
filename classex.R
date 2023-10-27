rm(list = ls())

library(tidyverse)
library(dplyr)
library(readxl)

###Importación de los datos###

rawdata <- read_excel("C:/Users/Lenovo/Downloads/data_run221467.xlsx", 
                             sheet = "decision (subjects table)")

###Arreglo de los datos###

df1 <- filter(rawdata, variable == 'Dove')

df1 <- subset(df1, select = c("run ID", "player ID", "round", "decision", "treatment"))

df1 <- rename(df1, 'Dove' = 'decision')

df1 <- df1 %>% mutate(Paloma = Dove)

df1 <- df1 %>% mutate(Paloma = case_when(Paloma == 1 ~ 'Paloma', Paloma == 2 ~ 'Halcón'),
                     Paloma = factor(Paloma, levels = c('Paloma', 'Halcón')))

df1 <- df1 %>% mutate(Dove = case_when(Dove == 1 ~ 1, Dove == 2 ~ 0))

###Crear otras columnas###

###Filtrar la columna X###

df2 <- filter(rawdata, variable == 'X')

df2 <- rename(df2, 'X' = 'decision')

df2 <- subset(df2, select = c("run ID", "player ID", "round", "X"))

###Filtrar la columna Y###

df3 <- filter(rawdata, variable == 'Y')

df3 <- rename(df3, 'Y' = 'decision')

df3 <- subset(df3, select = c("run ID", "player ID", "round", "Y"))

###Filtrar la columna del Código###

df4 <- rawdata

###df4 %>% 
  #group_by('player ID') %>% 
  #mutate(iduis = CodUIS)

df4 <- filter(rawdata, variable == 'CodUIS')

df4 <- rename(df4, 'iduis' = 'decision')

df4 <- subset(df4, select = c("run ID", "player ID", "round", "iduis"))

###Pegar la Base### 

dataframe <- left_join(df1,df2,by = c("run ID", "player ID", "round"))

dataframe <- left_join(dataframe,df3,by = c("run ID", "player ID", "round"))

dataframe <- left_join(dataframe,df4,by = c("run ID", "player ID", "round"))

###Guardar la base###

write.csv(dataframe, file = "datos.csv")

###Librerías para analizar y graficar datos###

library(GGally)
library(ggplot2)
library(stats)

###Análisis de datos###

dataframe <- dataframe %>% mutate(dataframe, conflict = Y-X)

###Descriptivas###

summary(dataframe$Paloma)
summary(dataframe$X)
summary(dataframe$Y)
summary(dataframe$conflict)

###Gráficas###

ggplot(dataframe, aes(x= Paloma, fill=Paloma))+
  geom_bar()+
  ggtitle("Decisiones")+
  theme_bw()+
  theme(legend.position="none")+
  labs(y="Frecuencia", x="")+
  theme(axis.text = element_text( size = 12),
        axis.title = element_text( size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=45,hjust = 1))

modelo1 <- lm(Dove ~ conflict, data = dataframe)

summary(modelo1)

ggplot(data = dataframe, aes(x = conflict, y = Dove)) +
  geom_point() +
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE)


