##contexto
#aguascalientes es uno de los estados mas chicos en cuestion territortial de mexico
#de lo cual cuenta con 11 municipios
 
#la fuente de datos es del INEGI y proceden del año 2020
#esta linea es para convertir un archivo mark down en pdf
rmarkdown::render("RMARKDOWN_POBLACION_AGS_2020.Rmd", output_format = "pdf_document")

list.files()
## Paso 1: Cargar paquetes


install.packages("rmarkdown")
install.packages("sf")
install.packages("tidyverse")
install.packages("skimr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages('tinytex')
tinytex::install_tinytex()

library(tinytex)
library(ggplot2)
library(tidyverse)
library(skimr)
library(dplyr)
library(sf)
library(magrittr)
# Verificar si el objeto está cargado
if ("total_poblacion1" %in% ls()) {
  print("El objeto total_poblacion1 está cargado en el entorno.")
} else {
  print("El objeto total_poblacion1 NO está cargado en el entorno.")
}



## Paso 2: Importar datos
poblacion_AGS_2020 <-read.csv("C:/poblacion_ags_2020/conjunto_de_datos_iter_01CSV20.csv")


## Paso 3: Conocer tus datos
head(poblacion_AGS_2020)
colnames(poblacion_AGS_2020)
glimpse(poblacion_AGS_2020)
str(poblacion_AGS_2020)
skim_without_charts(poblacion_AGS_2020) #te dará un resumen estadístico de las variables en ese conjunto de datos, lo que te permitirá obtener una comprensión rápida de la distribución y las características de los datos, pero sin gráficos adjuntos.


## Paso 4: Limpiar los datos

##filtrar las columans requeridas y guardar en una variable  
total_poblacion2 <- poblacion_AGS_2020 %>% 
  select(MUN,NOM_MUN, NOM_LOC, POBTOT, POBFEM, POBMAS)
print(total_poblacion2) 

# Filtrar las filas donde MUN sea igual a 0


##se filtra el total de la entidad AGS 

total_poblacion1 <- poblacion_AGS_2020 %>%
  filter(MUN == 0 & NOM_LOC == "Total de la Entidad") %>%
  select(NOM_MUN, POBTOT, POBFEM, POBMAS)

print(total_poblacion1)


# Filtrar los datos para obtener solo el total de la entidad
total_entidad <- poblacion_AGS_2020 %>%
  filter(MUN == 0 & NOM_LOC == "Total de la Entidad")%>%
  select(NOM_MUN, NOM_LOC, POBTOT, POBFEM, POBMAS)
print(total_entidad)


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
  geom_bar(mapping = aes(x = NOM_MUN, y = POBTOT, fill=POBTOT) ,stat = "identity") +  # Color azul para todas las barras
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


# Crear un gráfico de pastel para la poblacion de genero 

# Reestructurar los datos
total_entidad_long <- total_entidad %>%
  pivot_longer(cols = c(POBMAS, POBFEM),
               names_to = "Genero",
               values_to = "Poblacion")

# Crear el gráfico de pastel
gg_pastel2 <- ggplot(total_entidad_long, aes(x = "", y = Poblacion, fill = Genero)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("Distribución de población por género") +
  theme_void() +
  labs(caption = "Datos recopilados del INEGI 2020") +
  theme(legend.position = "right") +
  geom_text(aes(label = Poblacion), position = position_stack(vjust = 0.5))

print(gg_pastel2)













