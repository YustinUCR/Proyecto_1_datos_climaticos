library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(gridExtra)
library(grid)
library(extrafont)
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

a <- ggplot(newdata, aes(x = Temperatura)) +
  geom_histogram(binwidth = 0.1,
                 color = "black",
                 fill = "red") +
  ggtitle("Histograma de Temperatura") +
  xlab("Temperatura(°C)") +
  ylab("Frecuencia(%)") +
  theme_modern_rc()

#Histograma de Humedad relativa


b <- ggplot(newdata, aes(x = HumedadRelativa)) +
  geom_histogram(binwidth = 0.5,
                 color = "black",
                 fill = "#1E90FF") +
  ggtitle("Histograma de Humedad Relativa") +
  xlab("Humedad relativa(%)") +
  ylab("Frecuencia(%)") +
  theme_modern_rc()

#Histograma de Velocidad del viento

c <- ggplot(newdata, aes(x = Velocidadviento)) +
  geom_histogram(binwidth = 0.4,
                 color = "black",
                 fill = "#808080") +
  ggtitle("Histograma de velocidad del viento") +
  xlab("Velocidad del viento (m/s)") +
  ylab("Frecuencia(%)") +
  theme_modern_rc()


#Histograma de LLuvia


d <- ggplot(newdata, aes(x = Lluvia)) +
  geom_histogram(binwidth = 10,
                 color = "black",
                 fill = "#0000FF") +
  ggtitle("Histograma de Lluvia") +
  xlab("Precipitación(mm)") +
  ylab("Frecuencia(%)") +
  theme_modern_rc()

#Histograma de irradiación

e <- ggplot(newdata, aes(x = Irradiación)) +
  geom_histogram(binwidth = 8,
                 color = "black",
                 fill = "#FF4500") +
  ggtitle("Histograma de Irradición") +
  xlab("Irradiación(wm2)") +
  ylab("Frecuencia(%)") +
  theme_modern_rc()

#Histograma de Evotranspiración

f <- ggplot(newdata, aes(x = EvapoTranspiracion)) +
  geom_histogram(binwidth = 0.4,
                 color = "black",
                 fill = "#B0E0E6") +
  ggtitle("Histograma de Evapotranspiracion") +
  xlab("Evapotranspiracion(mm)") +
  ylab("Frecuencia(%)") +
  theme_modern_rc()

#Todos los histogramas
grid.arrange(a, b, c, d, e, f, nrow=2,
             top= textGrob("Histogramas",gp=gpar(fontsize=25,font=15)))

#Promedio de las variables

prom_mensual_temp <- data.frame(mes = 1:12,
                                Temperatura = tapply
                                (newdata$Temperatura,
                                  format(newdata$Fecha, format = "%m"),
                                  FUN = mean))

prom_mensual_humedad <- data.frame(mes = 1:12,
                                HumedadRelativa = tapply
                                (newdata$HumedadRelativa,
                                  format(newdata$Fecha, format = "%m"),
                                  FUN = mean))

prom_mensual_viento <- data.frame(mes = 1:12,
                                  VelocidadViento = tapply
                                  (newdata$Velocidadviento,
                                    format(newdata$Fecha, format = "%m"),
                                    FUN = mean))

prom_mensual_irradi <- data.frame(mes = 1:12,
                                  Irradiación = tapply
                                  (newdata$Irradiación,
                                    format(newdata$Fecha, format = "%m"),
                                    FUN = mean))

suma_mensual_lluvia <- data.frame(mes = 1:12,
                                  Lluvia = tapply
                                  (newdata$Lluvia,
                                    format(newdata$Fecha, format = "%m"),
                                    FUN = sum))

suma_mensual_evapo <- data.frame(mes = 1:12,
                                  EvapoTranspiración = tapply
                                  (newdata$EvapoTranspiracion,
                                    format(newdata$Fecha, format = "%m"),
                                    FUN = sum))
#Para el tercer ejercicio


#Gráfico de lineas de temperatura mensual
g <- ggplot(prom_mensual_temp, aes(x=mes, y=Temperatura )) + 
  geom_line(colour="red", lwd = 1, lty = 1)+ 
  ggtitle("Promedio mensual de Temperatura") +
  xlab("Mes") +
  ylab("Temperatura(°C)")+
  theme_modern_rc()

#Grafico de lineas de Humedad Relativa Mensual
h <- ggplot(rom_mensual_humedad, aes(x=mes, y=HumedadRelativa )) + 
  geom_line(colour="#076DFD", lwd = 1.5, lty = 2) + 
  ggtitle("Promedio mensual de Humedad Relativa") +
  xlab("Mes") +
  ylab("Humedad Relativa(%)")+
  theme_modern_rc()

#Gráfico de lineas de Irradiación Mensual 

i <- ggplot(prom_mensual_irradi, aes(x=mes, y=Irradiación )) + 
  geom_line(colour="#FF4500", lwd = 1.5, lty = 3) + 
  ggtitle("Promedio mensual de Irradiación") +
  xlab("Mes") +
  ylab("Irradiación(Mw2)")+
  theme_modern_rc()

#Gráfico de lineas velocidad del viento

j <- ggplot(prom_mensual_viento, aes(x=mes, y=VelocidadViento )) + 
  geom_line(colour="#808080", lwd = 1.5, lty = 4) + 
  ggtitle("Promedio mensual de Velocidad Del viento") +
  xlab("Mes") +
  ylab("Velocidad del viento(ms)")+
  theme_modern_rc()

#Grafico de lineas de Evapotranspiracion

k <- ggplot(suma_mensual_evapo, aes(x=mes, y=EvapoTranspiración )) + 
  geom_line(colour="#B0E0E6", lwd = 1.5, lty = 5) + 
  ggtitle("Suma mensual de la Evapotranspiración") +
  xlab("Mes") +
  ylab("Evapotranspiración") +
  theme_modern_rc()

#Gráfico de lineas de Lluvia

l <- ggplot(suma_mensual_lluvia, aes(x=mes, y=Lluvia )) + 
  geom_line(colour="#0000FF", lwd = 1.5, lty = 6) + 
  ggtitle("Suma mensual de Lluvia") +
  xlab("Mes") +
  ylab("Lluvia(mm)") +
  theme_modern_rc()


#Todos los gráficos de lineas

grid.arrange(g, h, i, j, k, l, nrow=2,
             top= textGrob("Gráficos de lineas",gp=gpar(fontsize=25,font=15)))


###Ejercicio 4


# Gráfico de dispersión entre la temperatura y humedad relativa
m <- ggplot(newdata, aes(x = Temperatura)) +
  geom_point(
    aes(y = HumedadRelativa)
  ) +
  ggtitle("Relación entre Humedad Relativa y Temperatura") +
  xlab("Temperatura(C)") +
  ylab("Humedad Relativa(%)") +
  theme_ipsum()

#Grafico de dispersión entre velocidad del viento y lluvia

n <- ggplot(newdata, aes(x = Velocidadviento)) +
  geom_point(
    aes(y = Lluvia)
  ) +
  ggtitle("Relación entre Velocidad del Viento y Lluvia") +
  xlab("Velocidad del viento(ms)") +
  ylab("Lluvia(mm)") +
  theme_ipsum()

#Gráfico de dispersión entre la irradiación y evapotranspiracion

o <- ggplot(newdata, aes(x = Irradiación)) +
  geom_point(
    aes(y = EvapoTranspiracion)
  ) +
  ggtitle("Relación entre Irradiación y EvapoTranspiración") +
  xlab("Irradiación(Wm2)") +
  ylab("Evapotranspiración(mm)") +
  theme_ipsum()

#Gráfico de dispersion entre Evapotranspiración y lluvia

p <- ggplot(newdata, aes(x = EvapoTranspiracion)) +
  geom_point(
    aes(y = Lluvia)
  ) +
  ggtitle("Relación entre Evapotranspiración y lluvia") +
  xlab("EvapoTranspiracion(mm)") +
  ylab("Lluvia(mm)") +
  theme_ipsum()

#Gráfico de dispersion entre Irradiacion y Temperatura

q <- ggplot(newdata, aes(x = Irradiación)) +
  geom_point(
    aes(y = Temperatura)
  ) +
  ggtitle("Relación entre Irradiación y Temperatura") +
  xlab("Irradiación(Wm2)") +
  ylab("Temperatura(°C)") +
  theme_ipsum()

#Gráfico de dispersion entre Lluvia y humedad

r <- ggplot(newdata, aes(x = HumedadRelativa)) +
  geom_point(
    aes(y = Lluvia)
  ) +
  ggtitle("Relación entre Humedad Relativa y Lluvia") +
  xlab("Humedad Relativa(%)") +
  ylab("Lluvia(mm)") +
  theme_ipsum()

#Todos los gráficos de dispersión

grid.arrange(m, n, o, p, q, r, nrow=2,
             top= textGrob("Gráficos de dispersión",gp=gpar(fontsize=25,font=15)))
