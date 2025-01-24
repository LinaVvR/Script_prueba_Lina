install.packages("readxl")
install.packages("openxlsx")
library(tidyverse)
library(openxlsx)
library(readxl)

archivo1 <- read.xlsx("C:/Users/Asus/Desktop/Prueba R/Prueba_Tecnica/input/A.xlsx", sheet = 1)
A <- read_excel("Prueba_Tecnica/input/A.xlsx")
datos1 <- A %>% 
  mutate(Fuente = "A")
View(datos1)

archivo2 <- read.xlsx("C:/Users/Asus/Desktop/Prueba R/Prueba_Tecnica/input/B.xlsx", sheet = 1)
B <- read_excel("Prueba_Tecnica/input/B.xlsx")
datos2 <- B %>% 
  mutate(Fuente = "B")
View(datos2)

Tabla_consolidada <- bind_rows(datos1, datos2)
View(Tabla_consolidada)

summary(Tabla_consolidada)
#Se crea var "identificador" con los criterios elegidos
Tabla_consolidada <- Tabla_consolidada %>%
  group_by(NOMBRE1, DEPARTAMENTO, NUMERO_DOCUMENTO) %>%
  mutate(Identificador = cur_group_id()) %>%
  ungroup()
View(Tabla_consolidada)
#Número de homicidios
max_identificador <- Tabla_consolidada %>%
  summarise(Max_Identificador = max(Identificador)) %>%
  pull(Max_Identificador)
print(max_identificador)
max_identificador

#Número de duplicaods
duplicados <- Tabla_consolidada %>%
  count(Identificador) %>% 
  filter(n > 1)
print(duplicados)

#Nueva tabla (tabla final) limpiada sin victimas repetidas con el criterio fecha más reciente
tabla_final <- Tabla_consolidada %>%
  group_by(Identificador) %>%  
  slice_max(FECHA_HECHOS, n = 1, with_ties = FALSE) %>%
  ungroup()
View(tabla_final)

write.xlsx(tabla_final, "C:/Users/Asus/Desktop/Prueba R/Prueba_Tecnica/input/tabla_final.xlsx", overwrite = TRUE)

#Frecuencias
tabla_final <- tabla_final %>%
  mutate(AÑO = format(as.Date(FECHA_HECHOS, format = "%Y-%m-%d"), "%Y"))
View(tabla_final)
####Edad
frecuencia_edad <- tabla_final %>%
  count(EDAD, name = "Frecuencia") %>%
  arrange(desc(Frecuencia))
print(frecuencia_edad)
####Sexo
frecuencia_sexo <- tabla_final %>%
  count(SEXO, name = "Frecuencia") %>%
  arrange(desc(Frecuencia))
print(frecuencia_sexo)
####Dpto
frecuencia_departamento <- tabla_final %>%
  count(DEPARTAMENTO, name = "Frecuencia") %>%
  arrange(desc(Frecuencia))
print(frecuencia_departamento)
####Dpio
frecuencia_municipio <- tabla_final %>%
  count(MUNICIPIO, name = "Frecuencia") %>%
  arrange(desc(Frecuencia))
print(frecuencia_municipio)
####Año
frecuencia_año <- tabla_final %>%
  count(AÑO, name = "Frecuencia") %>%
  arrange(desc(Frecuencia))
print(frecuencia_año)

### Resultados PDF
library(dplyr)
library(openxlsx)
wb <- createWorkbook()

crear_tabla_frecuencia <- function(tabla, variable, titulo, nombre_hoja) {
  tabla_frecuencia <- tabla %>%
    count(!!sym(variable), name = "Frecuencia") %>%
    arrange(desc(Frecuencia))
  addWorksheet(wb, nombre_hoja)
  writeData(wb, nombre_hoja, titulo, startRow = 1, startCol = 1) 
  writeData(wb, nombre_hoja, tabla_frecuencia, startRow = 3, startCol = 1)
  return(tabla_frecuencia)
}
frecuencia_edad <- crear_tabla_frecuencia(
  tabla_final, "EDAD",
  titulo = "Frecuencia de Edades",
  nombre_hoja = "Edad"
)
frecuencia_sexo <- crear_tabla_frecuencia(
  tabla_final, "SEXO",
  titulo = "Frecuencia por Sexo",
  nombre_hoja = "Sexo"
)

frecuencia_departamento <- crear_tabla_frecuencia(
  tabla_final, "DEPARTAMENTO",
  titulo = "Frecuencia por Departamento",
  nombre_hoja = "Departamento"
)

frecuencia_municipio <- crear_tabla_frecuencia(
  tabla_final, "MUNICIPIO",
  titulo = "Frecuencia por Municipio",
  nombre_hoja = "Municipio"
)

frecuencia_año <- crear_tabla_frecuencia(
  tabla_final, "AÑO",
  titulo = "Frecuencia por Año",
  nombre_hoja = "Año"
)

saveWorkbook(wb, "C:/Users/Asus/Desktop/Prueba R/Prueba_Tecnica/input/Tablas_de_Frecuencia.xlsx", overwrite = TRUE)

##Guardar output
sink("C:/Users/Asus/Desktop/Prueba R/Prueba_Tecnica/output/salida_script.txt")
source("C:/Users/Asus/Desktop\Prueba R/Prueba_Tecnica/src/Script_prueba_Lina.R", echo = TRUE)
