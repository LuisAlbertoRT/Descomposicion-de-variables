#########################################
###     DESCOMPOSICION DE VARIABLES   ###
###            PRONOSTICO             ###
#########################################


#Borra la memoria

rm(list=ls(all=TRUE))

#Codigo para instalar paquetes
#Llama las librerias

library(quantmod)
library(xts)
library(tseries)
library(forecast)
library(timeSeries)
library(tframePlus)
library(ggplot2)
library(dplyr) #
library(plotly)
library(hrbrthemes)
library(ggthemes)
library(tidyverse)
library(dygraphs)# para los gráficos con zoom 
library(gridExtra)
library(backports)


#install.packages("assertthat")
#import_roboto_condensed()

#Carga el archivo CSV
Data<-read.csv(file.choose())

# Me permite visualizar los datos
View(Data)

# str muestra de forma compacta la estructura de un objeto R
str(Data$Date) #el signo de $ identifica la columna sobre la que quiero hacer un calculo

# Se obtiene como resultado un factor.
# Los factores se utilizan para representar datos categ?ricos.
# Los factores pueden ser ordenados o desordenados y son una clase importante para el an?lisis estad?stico y para plotear

## Convierte el facto Date en formato de fecha
Data$Date <- as.Date(Data$Date, "%Y-%m-%d")

# Indica el rango de la fecha
range(Data$Date)

#Ordena los datos a partir de las fechas
Data <- Data[order(Data$Date), ]



## Grafica la serie de tiempo
plot(Data$Date, Data$Close, type = "l",
     main="Precio de cierre ETF Ultra S&P 500 Enero 2018-Noviembre 2020", #mean es para poner titulos a las graficas
     xlab="Fecha", ylab="Cierre", col= "palegreen4") #plot grafica las series

# Puedes seleccionar diferentes colores en esta p?gina http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf


#Gr?fico con mejor dise?o
ultra <- ggplot(Data, aes(x=Data$Date, y=Data$Close)) +
  ggtitle("Precio de ETF Ultra S&P 500, Cierre : Enero 2018-Noviembre 2020") +
  geom_line(color="deepskyblue4") +
  xlab("Fecha")+
  ylab("Precio de cierre")


ggplotly(ultra)

# GrÃ¡fico interactivo
ultra2 <- Data %>%
  ggplot( aes(x=Data$Date, y=Data$Close)) +
  ggtitle("Precio de Cierre ETF Ultra S&P 500: Enero 2018-Noviembre 2020") +
  geom_area(fill="blueviolet", alpha=0.3) +
  geom_line(color="darkblue") +
  ylab("Precio de Cierre  ($MXN)") +
  xlab("Fecha")


# El operador pipeline %>% es ?til para concatenar m?ltiples operaciones
# El operador %>%  permite escribir una secuencia de operaciones de izquierda a derecha

#Conforme el par?metro alpha  disminuye, se hace m?s "clara" el ?rea

# Gr?fico Interactivo 1
ggplotly(ultra2)

# Gr?fico Interactivo 2

Precio<- xts(x= Data$Close, order.by = Data$Date)
colnames(Precio)<- c("Cierre")

p <- dygraph(Precio) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.5, drawGrid = F, colors="darkblue") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = F)  %>%
  dyRoller(rollPeriod = 1)

p

#PARA SABER LOS PRIMEROS O ÚLTIMOS DATOS

head(Data$Date, 5) #Ve los primeros datos
tail(Data$Date, 5) #Ve los ?ltimos datos
tail(Data$Close, 5) 

## Numero de dias que cotiza el instrumento por a?o
years <- format(Data$Date, "%Y")
View(years)
tab <- table(years) #Saca la frecuencia de los datos
tab

## Numero de dias que cotiza en promedio por a?o
# Esto es para seleccionar la ventana de tiempo "optima"

mean(tab[1:(length(tab) - 1)]) #mean me permite sacar promedios



###DESCOMPOSICION

ultra_des <- ts(Data$Close, start = c(2018,01,02), frequency = 137) #ts convierte en series tiempo la serie
ultra_des %>% stl(s.window='periodic')%>% autoplot #stl hace la descomposici?n - tendencia 



#Descomposici?n estacional por a?o

ggseasonplot(ultra_des,year.labels=TRUE,year.labels.left=T) +
  ylab("Precio de cierre") +
  xlab("Componente estacional") +
  ggtitle("Grafico estacional de Ultra S&P500 por año")



###PRONOSTICO


pronostico<-forecast(ultra_des)
View(pronostico)

datos_pronosticados<-data.frame(pronostico) #lleva los datos pronosticados a una tabla
View(datos_pronosticados)



#Gr?fico del pron?stico

autoplot(pronostico) + geom_forecast(h=500)+ #h es el n?mero de d?as a pronosticar
  ylab("Precio de cierre") +
  xlab("Tiempo") +
  ggtitle("Pronostico de Ultra S&P 500 a 500 dias")


#Elegir subconjuntos para el pron?stico

subcojunto <- window(ultra_des, start =2021)
autoplot(subcojunto) + geom_forecast(h=150)+
  ggtitle("Pronostico de S&P 500 a 150 dias, w= 137")

#Guarda los resultados

write.csv(pronostico, "C:/Users/Admin/Desktop/Econometria financiera/Modelo 137 dias SP 500 DESDE 2018/Pronostico 137 SP.csv")




#### CICLO PARA N NUMERO DE RESULTADOS###


Resultados_des <- "C:/Users/Admin/Desktop/Econometria financiera/"
contador1 = 1

for (contador1 in 115:250){
  Des_n<-ts(Data$Close, start = c(2017,01,03), frequency = contador1)
  print(Des_n)
  descomposiciones = Des_n %>% stl(s.window='periodic') %>% autoplot+
    ggtitle(paste("Descomposici?n a", contador1, "d?as"))
  print(descomposiciones)
  
  ggsave(descomposiciones, file=paste(Resultados_des ,
                                      'Descomposicion a',
                                      contador1, ".pdf"))
  
  
  
  
  #Gr?fico del pron?stico
  
  subconjunto <- window(Des_n, start = 2017)
  pronosticos =  print(autoplot(subconjunto) + geom_forecast(h=100)+ #h es el n?mero de d?as a pronosticar
                         ylab("Precio de cierre") +
                         xlab("Tiempo") +
                         ggtitle(paste("Pron?stico de Pe?oles a 100 d?as, w=", contador1, "d?as")))
  
  
  
  ggsave(pronosticos, file=paste(Resultados_des ,
                                 'Pron?stico a',
                                 contador1, ".pdf"))
  
  
  
  pronostico_n<-forecast(Des_n)
  pronostico_n<-data.frame(pronostico_n)
  #View(pronostico)
  
  #datos_pronosticados<-data.frame(pronostico) #lleva los datos pronosticados a una tabla
  #View(datos_pronosticados)
  
  
  
  
  
}


# write.csv(pronostico_n, file= paste("C:/Users/USER/Dropbox/CLASES/ECONOMETRIA FINANCIERA/SEMESTRE 2019 AGOSTO DICIEMBRE/PLANTILLAS/Pronosticos_n.csv"))




