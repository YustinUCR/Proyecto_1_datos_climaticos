library(ggplot2)
library(dplyr)
library(hrbrthemes)

#Carga de datos

dat <- read.csv("liberia_datos_climaticos.csv", sep = ",", na.strings="" , dec=",")

#para visualizar la estructura
str(dat)

head(newdata)

dim(newdata)

#El siguiente paso es obtener si los datos poseen celdas vacías y eliminarlas 
#Argumento para ver si posee celdas vacias
dat[!complete.cases(dat),]


#Eliminar las celdas vacías con una data nueva
newdata <- na.omit(dat)

#Conjunto de datos sin celdas vacías
newdata[!complete.cases(newdata),]


# Cambio de nombre de las columnas mediante rename()
newdata <-
  newdata %>%
  rename(Fecha = Date,
         Temperatura = Temperatura..Celsius.,
         HumedadRelativa = HumedadRelativa....,
         Velocidadviento = VelocidadViento..m.s.,
         Lluvia = Lluvia..mm.,
         Irradiación = Irradiacion..W.m2.,
         EvapoTranspiracion = EvapoTranspiracion..mm.)


#Conversión de la columna fecha a la clase date

newdata$Fecha <- as.Date(newdata$Fecha, "%d/%m/%Y")



#Histograma de temperatura
ggplot(newdata, aes(x = Temperatura)) +
  geom_histogram(binwidth = 0.1,
                 color = "black",
                 fill = "red") +
  ggtitle("Histograma de Temperatura") +
  xlab("Temperatura(°C)") +
  ylab("Frecuencia") +
  theme_light()

#Histograma de Humedad relativa


ggplot(newdata, aes(x = HumedadRelativa)) +
  geom_histogram(binwidth = 0.5,
                 color = "black",
                 fill = "#1E90FF") +
  ggtitle("Histograma de Humedad Relativa") +
  xlab("Humedad relativa(%)") +
  ylab("Frecuencia(%)") +
  theme_light()

#Histograma de Velocidad del viento

ggplot(newdata, aes(x = Velocidadviento)) +
  geom_histogram(binwidth = 0.4,
                 color = "black",
                 fill = "#808080") +
  ggtitle("Histograma de velocidad del viento") +
  xlab("Velocidad del viento (m/s)") +
  ylab("Frecuencia(%)") +
  theme_light()


#Histograma de LLuvia


ggplot(newdata, aes(x = Lluvia)) +
  geom_histogram(binwidth = 10,
                 color = "black",
                 fill = "#0000FF") +
  ggtitle("Histograma de Lluvia") +
  xlab("Precipitación(mm)") +
  ylab("Frecuencia(%)") +
  theme_grey()

#Histograma de irradiación

ggplot(newdata, aes(x = Irradiación)) +
  geom_histogram(binwidth = 8,
                 color = "black",
                 fill = "#FF4500") +
  ggtitle("Histograma de Irradición") +
  xlab("Irradiación(wm2)") +
  ylab("Frecuencia(%)") +
  theme_bw()

#Histograma de Evotranspiración

ggplot(newdata, aes(x = EvapoTranspiracion)) +
  geom_histogram(binwidth = 0.2,
                 color = "black",
                 fill = "#B0E0E6") +
  ggtitle("Histograma de Evapotranspiracion") +
  xlab("Evapotranspiracion(mm)") +
  ylab("Frecuencia(%)") +
  theme_replace()


#Promedio de las variables



#Para el tercer ejercicio



ggplot(newdata, aes(x=Fecha, y=Temperatura )) + 
  geom_line(colour="blue")  + 
  geom_point( size=2, shape=21, fill="white", colour="red") + 
  theme_minimal()

##Recordar añadir el promedio por mes




#esto para el grafico de puntos del ejercicio 4


# Gráfico de dispersión entre la temperatura y humedad relativa
ggplot(newdata, aes(x = Temperatura)) +
  geom_point(
    aes(y = HumedadRelativa)
  ) +
  ggtitle("Relación entre Humedad Relativa y Temperatura") +
  xlab("Temperatura(C)") +
  ylab("Humedad Relativa(%)") +
  theme_ipsum()

#Grafico de dispersión entre velocidad del viento y lluvia

ggplot(newdata, aes(x = Velocidadviento)) +
  geom_point(
    aes(y = Lluvia)
  ) +
  ggtitle("Relación entre Velocidad del Viento y Lluvia") +
  xlab("Velocidad del viento(ms)") +
  ylab("Lluvia(mm)") +
  theme_modern_rc()

#Gráfico de dispersión entre la irradiación y evapotranspiracion

ggplot(newdata, aes(x = Irradiación)) +
  geom_point(
    aes(y = EvapoTranspiracion)
  ) +
  ggtitle("Relación entre Irradiación y EvapoTranspiración") +
  xlab("Irradiación(Wm2)") +
  ylab("Evapotranspiración(mm)") +
  theme_update()

#Gráfico de dispersion entre Evapotranspiración y lluvia

ggplot(newdata, aes(x = EvapoTranspiracion)) +
  geom_point(
    aes(y = Lluvia)
  ) +
  ggtitle("Relación entre Evapotranspiración y lluvia") +
  xlab("EvapoTranspiracion(mm)") +
  ylab("Lluvia(mm)") +
  theme_tinyhand()

#Gráfico de dispersion entre Irradiacion y Temperatura

ggplot(newdata, aes(x = Irradiación)) +
  geom_point(
    aes(y = Temperatura)
  ) +
  ggtitle("Relación entre Irradiación y Temperatura") +
  xlab("Irradiación(Wm2)") +
  ylab("Temperatura°C") +
  theme_ipsum_pub()

#Gráfico de dispersion entre Lluvia y humedad

ggplot(newdata, aes(x = HumedadRelativa)) +
  geom_point(
    aes(y = Lluvia)
  ) +
  ggtitle("Relación entre HumedadRelativa y Lluvia") +
  xlab("Humedad Relativa(%)") +
  ylab("Lluvia(mm)") +
  theme_ipsum_pub()