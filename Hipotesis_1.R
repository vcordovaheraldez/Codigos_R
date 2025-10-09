
# Hipótesis 1
  # Indicador 1: Caudal tratado de las PTAR con un régimen privado (2022) 

# Bases de datos utilizadas:  
  # Inventario_Mexico_con_Municipio
  # IDH_2015

# Librerías utilizadas:  
library(dplyr)
library(readr)
library(ggplot2)
library(extrafont)

#####################################################################################################################

# Nombres de columnas
colnames(Inventario_Mexico_con_Municipio)

# Renombrar columnas
Inventario_Mexico_con_Municipio <- Inventario_Mexico_con_Municipio %>%
  rename(Tamaño = "9. Tamaño\n[Campo Calculado]")

Inventario_Mexico_con_Municipio <- Inventario_Mexico_con_Municipio %>%
  rename(Capacidad = "14.Capacidad de tratamiento en litros por segundo (L/s)")

Inventario_Mexico_con_Municipio <- Inventario_Mexico_con_Municipio %>%
  rename(Caudal = "15. Caudal de operación promedio (L/s)")

#####################################################################################################################

# Transformar caudal a numérico
Inventario_Mexico_con_Municipio$Caudal <- as.numeric(Inventario_Mexico_con_Municipio$Caudal)

# Caudal total
suma_caudal <- sum(Inventario_Mexico_con_Municipio$Caudal, na.rm = TRUE)
print(suma_caudal)

# Filtrar por tamaño de PTAR
ptar_maximas <- Inventario_Mexico_con_Municipio %>%
  filter(Tamaño %in% c("03 Grande", "04 Megaplanta"))

# Resumen por Estado
resumen_estado <- ptar_maximas %>%
  group_by(Estado) %>%
  summarise(
    Numero_de_Plantas = n()
  ) %>%
  arrange(desc(Numero_de_Plantas))

print(resumen_estado)

# Descargar 
write.csv(resumen_estado, file = "resumen_estado.csv", row.names = FALSE)

# Suma total
capacidad_total <- sum(ptar_maximas$Capacidad, na.rm = TRUE)
print(capacidad_total)

caudal_total <- sum(ptar_maximas$Caudal, na.rm = TRUE)
print(caudal_total)

# Promedio
capacidad_promedio <- mean(ptar_maximas$Capacidad, na.rm = TRUE)
print(capacidad_promedio)

caudal_promedio <- mean(ptar_maximas$Caudal, na.rm = TRUE)
print(caudal_promedio)

#####################################################################################################################

# Filtrar PTAR clasificadas como Megaplanta
ptar_mega <- ptar_maximas %>%
  filter(Tamaño == "04 Megaplanta")

head(ptar_mega)

# Suma del caudal de las PTAR Mega 
caudal_mega <- ptar_mega %>%
  summarise(Suma_Caudal = sum(Caudal, na.rm = TRUE))

print(caudal_mega)

# Cantidad 
cat("Número de PTAR Megaplantas:", nrow(ptar_mega), "\n") 

# Filtrar PTAR clasificadas como Grandes
ptar_grande <- ptar_maximas %>%
  filter(Tamaño == "03 Grande")

head(ptar_grande)

# Suma del caudal de las PTAR grandes 
caudal_grande <- ptar_grande %>%
  summarise(Suma_Caudal = sum(Caudal, na.rm = TRUE))

print(caudal_grande)

# Cantidad 
cat("Número de PTAR Grandes:", nrow(ptar_grande), "\n")

#####################################################################################################################

# Limpiar valores (quitar espacios y convertir a minúsculas)
ptar_maximas$tipo_regimen <- tolower(trimws(ptar_maximas$tipo_regimen))

# Promedio del caudal total por tipo de régimen
promedio_regimen <- ptar_maximas %>%
  group_by(tipo_regimen) %>%
  summarise(
    Numero_de_Plantas = n(),
    Suma_Caudal = sum(Caudal, na.rm = TRUE),
    Promedio_Caudal = sum(Caudal, na.rm = TRUE) / n()
  ) %>%
  arrange(desc(Suma_Caudal))

promedio_regimen

#####################################################################################################################

# Base sin la PTAR Atotonilco de Tula
Reuso_filtrado_sin_Atotonilco <- Inventario_Mexico_con_Municipio %>%
  filter(`2. Nombre de la planta de tratamiento` != "Atotonilco de Tula")

# Filtrar por tamaño de PTAR
ptar_maximas1 <- Reuso_filtrado_sin_Atotonilco %>%
  filter(Tamaño %in% c("03 Grande", "04 Megaplanta"))

# Limpiar valores (quitar espacios y convertir a minúsculas)
ptar_maximas1$tipo_regimen <- tolower(trimws(ptar_maximas1$tipo_regimen))

# Promedio del caudal total por tipo de régimen
promedio_regimen <- ptar_maximas1 %>%
  group_by(tipo_regimen) %>%
  summarise(
    Numero_de_Plantas = n(),
    Suma_Caudal = sum(Caudal, na.rm = TRUE),
    Promedio_Caudal = sum(Caudal, na.rm = TRUE) / n()
  ) %>%
  arrange(desc(Suma_Caudal))

promedio_regimen

#####################################################################################################################

# Unión de bases 

# IDH
base_unida <- left_join(ptar_maximas,IDH_2015,by = c("Estado", "Municipio"))

# Población 
pobmun <- read.csv("https://raw.githubusercontent.com/asalazaradams/data/main/PobMuni20.csv")

# Rangos y etiquetas
pobmun$categoria_poblacion <- cut(
  pobmun$POBTOT,
  breaks = c(-Inf, 5000, 50000, 300000, Inf),  
  labels = c("Rurales", "Semiurbanos", "Urbanos", "Metropolitanos"),  
  right = TRUE)

print(pobmun)

# Reenombar variables
pobmun = dplyr::rename(pobmun, plnt_folio = clave_mun)

# Unión de ambas bases 
base_completa <- left_join(base_unida, pobmun, by = "plnt_folio")

#####################################################################################################################

# Caudal total por régimen, población y desarrollo económico
resumen <- base_completa %>%
  group_by(tipo_regimen, categoria_poblacion, gdh) %>%
  summarise(
    Numero_de_Plantas = n(),
    Suma_Caudal = sum(Caudal, na.rm = TRUE),
    .groups = "drop"
  )

print(resumen)

# Descargar 
write.csv(resumen, file = "resumen.csv", row.names = FALSE)

# Omitir NA 
resumen_filtrado <- resumen %>%
  filter(!is.na(tipo_regimen), 
         !is.na(categoria_poblacion), 
         !is.na(gdh))

# Colores del gráfico
colores_azules <- c("#458B00", "#CD6600", "#00688B")

windowsFonts(Times=windowsFont("Times New Roman"))

# Gráfico 
ggplot(resumen_filtrado, aes(x = categoria_poblacion, 
                             y = Suma_Caudal, 
                             fill = gdh)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Suma_Caudal, 1)), 
            position = position_dodge(width = 0.8),
            vjust = -0.3, 
            size = 3.5, 
            family = "Times") +
  facet_wrap(~ tipo_regimen) +
  scale_fill_manual(values = colores_azules) +  # Colores fijos
  labs(title = "Caudal total tratado por régimen, categoría poblacional y desarrollo económico",
       x = "Categoría poblacional",
       y = "Caudal total tratado (l/s)",
       fill = "Nivel de desarrollo económico") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

#####################################################################################################################
#####################################################################################################################

# Hipótesis 1 
  # Indicador 2: Caudal territorial tratado por año de las PTAR con un régimen privado 

# Bases de datos utilizadas:
  # Base_2017m
  # Base_2019m
  # Base_2021m

# Librerías utilizadas:
library(dplyr)
library(readr)
library(ggplot2)
library(extrafont)

#####################################################################################################################

# Codificación de la variable tra_regi_a para las bases de datos del Censo del INEGI. 
Base_2017m <- Base_2017m %>%
  mutate(
    tipo_regimen = case_when(
      tra_regi_a %in% c("Estatal", "Intermunicipal", "Municipal", "Otro publico", "Social") ~ "Público",
      tra_regi_a == "Privado" ~ "Privado",
      TRUE ~ NA_character_ # Para cualquier otro valor que no esté en las categorías anteriores, asigna NA
    )
  )

Base_2019m <- Base_2019m %>%
  mutate(
    tipo_regimen = case_when(
      tra_regi_a %in% c("Estatal", "Intermunicipal", "Municipal", "Otro publico", "Social") ~ "Público",
      tra_regi_a == "Privado" ~ "Privado",
      TRUE ~ NA_character_ # Para cualquier otro valor que no esté en las categorías anteriores, asigna NA
    )
  )

Base_2021m <- Base_2021m %>%
  mutate(
    tipo_regimen = case_when(
      tra_regi_a %in% c("Estatal", "Intermunicipal", "Municipal", "Otro publico", "Social") ~ "Público",
      tra_regi_a == "Privado" ~ "Privado",
      TRUE ~ NA_character_ # Para cualquier otro valor que no esté en las categorías anteriores, asigna NA
    )
  )

# Filtrar las PTAR con caudal tratado igual o menor a 35,000 l/s (límite máximo según la PTAR Atotonilco) 
ptar_filtradas2017 <- Base_2017m %>%
  filter(!is.na(cau_trat) & cau_trat > 0 & cau_trat <= 35000)

ptar_filtradas2019 <- Base_2019m %>%
  filter(!is.na(cau_trat) & cau_trat > 0 & cau_trat <= 35000)

ptar_filtradas2021 <- Base_2021m %>%
  filter(!is.na(cau_trat) & cau_trat > 0 & cau_trat <= 35000)

# Descargar 
write.csv(ptar_filtradas2021, file = "ptar_filtradas2021.csv", row.names = FALSE)
write.csv(ptar_filtradas2019, file = "ptar_filtradas2019.csv", row.names = FALSE)
write.csv(ptar_filtradas2017, file = "ptar_filtradas2017.csv", row.names = FALSE)

#####################################################################################################################

# Número total de municipios en México
total_municipios <- 2478 # INEGI (2025)

# Función para calcular cobertura territorial
calcular_cobertura <- function(data) {
  data %>%
    filter(tra_oper == 1) %>%               # Filtrar plantas en operación
    distinct(id_estado, id_municipio) %>%   # Municipios únicos
    nrow() / total_municipios * 100         # Porcentaje
}

# Cobertura para cada año
cobertura_2017 <- calcular_cobertura(ptar_filtradas2017)
cobertura_2019 <- calcular_cobertura(ptar_filtradas2019)
cobertura_2021 <- calcular_cobertura(ptar_filtradas2021)

# Resultados
cat("Cobertura 2017:", round(cobertura_2017, 2), "%\n")
cat("Cobertura 2019:", round(cobertura_2019, 2), "%\n")
cat("Cobertura 2021:", round(cobertura_2021, 2), "%\n")

#####################################################################################################################

# Número total de municipios en México
total_municipios <- 2478

# Cobertura territorial de gestión privada
calcular_cobertura_privada <- function(data) {
  data %>%
    filter(tra_oper == 1, tolower(tipo_regimen) == "privado") %>%  # Filtrar PTAR en operación y privadas
    distinct(id_estado, id_municipio) %>%                          # Municipios únicos
    nrow() / total_municipios * 100                                # Porcentaje
}

# Cobertura para cada año
cobertura_2017 <- calcular_cobertura_privada(ptar_filtradas2017)
cobertura_2019 <- calcular_cobertura_privada(ptar_filtradas2019)
cobertura_2021 <- calcular_cobertura_privada(ptar_filtradas2021)

# Resultados
cat("Cobertura privada 2017:", round(cobertura_2017, 2), "%\n")
cat("Cobertura privada 2019:", round(cobertura_2019, 2), "%\n")
cat("Cobertura privada 2021:", round(cobertura_2021, 2), "%\n")

# Número total de municipios en México
total_municipios <- 2478

# Cobertura territorial de gestión pública
calcular_cobertura_público <- function(data) {
  data %>%
    filter(tra_oper == 1, tolower(tipo_regimen) == "público") %>%  # Filtrar PTAR en operación y públicas
    distinct(id_estado, id_municipio) %>%                          # Municipios únicos
    nrow() / total_municipios * 100                                # Porcentaje
}

# Cobertura para cada año
cobertura_2017 <- calcular_cobertura_público(ptar_filtradas2017)
cobertura_2019 <- calcular_cobertura_público(ptar_filtradas2019)
cobertura_2021 <- calcular_cobertura_público(ptar_filtradas2021)

# Resultados
cat("Cobertura público 2017:", round(cobertura_2017, 2), "%\n")
cat("Cobertura público 2019:", round(cobertura_2019, 2), "%\n")
cat("Cobertura público 2021:", round(cobertura_2021, 2), "%\n")

#####################################################################################################################

# Número total de municipios en México
total_municipios <- 2478

# Función para calcular cobertura por categoría 
calcular_cobertura_categoria <- function(data, categoria_col) {
  data %>%
    filter(tra_oper == 1) %>%
    distinct(id_estado, id_municipio, .keep_all = TRUE) %>%
    group_by(.data[[categoria_col]]) %>%
    summarise(
      Municipios = n(),
      Porcentaje = n() / total_municipios * 100
    ) %>%
    rename(Categoria = .data[[categoria_col]])
}

# Cobertura por categoría poblacional 
cobertura_2017 <- calcular_cobertura_categoria(ptar_filtradas2017, "categoria_poblacion") %>% mutate(Año = 2017)
cobertura_2019 <- calcular_cobertura_categoria(ptar_filtradas2019, "categoria_poblacion") %>% mutate(Año = 2019)
cobertura_2021 <- calcular_cobertura_categoria(ptar_filtradas2021, "categoria_poblacion") %>% mutate(Año = 2021)

# Unir resultados
cobertura_total <- bind_rows(cobertura_2017, cobertura_2019, cobertura_2021)

print(cobertura_total)

# Descargar 
write.csv(cobertura_total, file = "cobertura_poblacional.csv", row.names = FALSE)

# Cobertura por nivel de desarrollo económico
desarrollo_2017 <- calcular_cobertura_categoria(ptar_filtradas2017, "gdh") %>% mutate(Año = 2017)
desarrollo_2019 <- calcular_cobertura_categoria(ptar_filtradas2019, "gdh") %>% mutate(Año = 2019)
desarrollo_2021 <- calcular_cobertura_categoria(ptar_filtradas2021, "gdh") %>% mutate(Año = 2021)

# Unir resultados
cobertura_desarrollo <- bind_rows(desarrollo_2017, desarrollo_2019, desarrollo_2021)

print(cobertura_desarrollo)

# Descargar 
write.csv(cobertura_desarrollo, file = "cobertura_desarrollo.csv", row.names = FALSE)
