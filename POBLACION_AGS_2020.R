##contexto
#aguascalientes es uno de los estados mas chicos en cuestion territortial de mexico
#de lo cual cuenta con 11 municipios
#del cual se dara conocer su cantidad de poblacion y dividido por genero hombre y mujer 
#la fuente de datos es del INEGI y proceden del año 2020
 ##MAPA AGUASCALIENTES



## Paso 1: Cargar paquetes

install.packages("sf")

install.packages("tidyverse")
install.packages("skimr")
install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(skimr)
library(dplyr)
library(sf)

## Paso 2: Importar datos
poblacion_AGS_2020 <-read.csv("C:/poblacion_ags_2020/conjunto_de_datos_iter_01CSV20.csv")
aguascalientes <- st_read("ruta/al/archivo/aguascalientes.shp")
## Paso 3: Conocer tus datos
head(poblacion_AGS_2020)
colnames(poblacion_AGS_2020)
glimpse(poblacion_AGS_2020)
str(poblacion_AGS_2020)
skim_without_charts(poblacion_AGS_2020) #te dará un resumen estadístico de las variables en ese conjunto de datos, lo que te permitirá obtener una comprensión rápida de la distribución y las características de los datos, pero sin gráficos adjuntos.


## Paso 4: Limpiar los datos

##filtrar las columans requeridas y guardar en una variable  
total_poblacion <- poblacion_AGS_2020 %>% 
  select(MUN,NOM_MUN, NOM_LOC, POBTOT, POBFEM, POBMAS)
print(total_poblacion)

##se filtra el total de la entidad AGS 

total_poblacion <- poblacion_AGS_2020 %>%
  filter(MUN == 0 & NOM_LOC == "Total de la Entidad") %>%
  select(NOM_MUN, NOM_LOC, POBTOT, POBFEM, POBMAS)

print(total_poblacion)


# Filtrar las filas donde NOM_LOC sea igual a "Total del Municipio" y MUN esté en el rango de 0 a 11
total_poblacion <- poblacion_AGS_2020 %>%
  filter(NOM_LOC == "Total del Municipio" & MUN >= 0 & MUN <= 11) %>%
  select(NOM_MUN, NOM_LOC, POBTOT, POBFEM, POBMAS)

print(total_poblacion)

##verificando el contenido
names(total_poblacion)
str(total_poblacion)


##crear grafica con lo filtrado

gg_total_poblacion <- ggplot(total_poblacion) +
  geom_bar(mapping = aes(x = NOM_MUN, y = POBTOT, fill=NOM_MUN), stat = "identity") +  # Color azul para todas las barras
  facet_wrap(~ POBTOT, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) +
  ggtitle("Total de población en municipios de Aguascalientes 2020") +
  xlab("Municipios") +
  ylab("Población total") + 
  labs(caption="Datos recopilados del INEGI 2020")

print(gg_total_poblacion)


#grafico de pastel 

gg_pastel <- ggplot(total_poblacion, aes(x = "", y = POBTOT, fill = NOM_MUN)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("Distribución de población por municipio 2020") +
  theme_void() +
  labs(caption="datos recopilados del INEGI 2020") +
  theme(legend.position = "right")
   

print(gg_pastel)

##MAPA
str(italy_map)
