
### Hipótesis 1: Caudal tratado de las PTAR con un régimen privado (2022) ###

# Bases de datos utilizadas:  
  # Inventario_Mexico_con_Municipio
  # IDH_2015

# Librerías utilizadas:  
library(readr)
library(dplyr)
library(ggplot2)
library(extrafont)

# Nombres de columnas
colnames(Inventario_Mexico_con_Municipio)

# Renombrar columnas
Inventario_Mexico_con_Municipio <- Inventario_Mexico_con_Municipio %>%
  rename(Tamaño = "9. Tamaño\n[Campo Calculado]")

Inventario_Mexico_con_Municipio <- Inventario_Mexico_con_Municipio %>%
  rename(Capacidad = "14.Capacidad de tratamiento en litros por segundo (L/s)")

Inventario_Mexico_con_Municipio <- Inventario_Mexico_con_Municipio %>%
  rename(Caudal = "15. Caudal de operación promedio (L/s)")

# Transformar caudal a numérico
Inventario_Mexico_con_Municipio$Caudal <- as.numeric(Inventario_Mexico_con_Municipio$Caudal)

# Suma caudal tratado
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
colores_azules <- c("#2171B5", "#6b8ea1", "#08306B")

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

### Hipótesis 1: Porcentaje del caudal territorial tratado por año de las PTAR con un régimen privado ###

# Bases de datos utilizadas:
  # Base_2017m
  # Base_2019m
  # Base_2021m

# Librerías utilizadas:
library(dplyr)
library(readr)
library(ggplot2)
library(extrafont)

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

# Función para calcular cobertura por tipo de gestión
calcular_cobertura <- function(data, tipo) {
  data %>%
    filter(tra_oper == 1, tolower(tipo_regimen) == tipo) %>%
    distinct(id_estado, id_municipio) %>%
    nrow() / total_municipios * 100
}

# Dataframe con los resultados
cobertura_df <- data.frame(
  Año = rep(c(2017, 2019, 2021), each = 2),
  Régimen = rep(c("Pública", "Privada"), times = 3),
  Cobertura = c(
    calcular_cobertura(ptar_filtradas2017, "público"),
    calcular_cobertura(ptar_filtradas2017, "privado"),
    calcular_cobertura(ptar_filtradas2019, "público"),
    calcular_cobertura(ptar_filtradas2019, "privado"),
    calcular_cobertura(ptar_filtradas2021, "público"),
    calcular_cobertura(ptar_filtradas2021, "privado")
  )
)

# Gráfico
ggplot(cobertura_df, aes(x = factor(Año), y = Cobertura, fill = Régimen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), show.legend = TRUE) +
  geom_text(aes(label = sprintf("%.2f%%", Cobertura)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5, family = "Times New Roman") +
  geom_line(aes(group = Régimen, color = Régimen),
            position = position_dodge(width = 0.9), size = 1, show.legend = FALSE) +
  geom_point(aes(group = Régimen, color = Régimen),
             position = position_dodge(width = 0.9), size = 1.5, show.legend = FALSE) +
  labs(
    title = "Cobertura Territorial de PTAR en Operación por Tipo de Gestión",
    x = "Año",
    y = "Cobertura (%)"
  ) +
  scale_fill_manual(values = c("Pública" = "#6b8ea1", "Privada" = "#2171B5")) +  # Colores de barras
  scale_color_manual(values = c("Pública" = "red", "Privada" = "red")) +   # Colores solo para línea
  theme_minimal(base_family = "Times New Roman") +
  theme(legend.title = element_blank())

# Número total de municipios en México
total_municipios <- 2478

# Función para calcular cobertura por categoría
calcular_cobertura_categoria <- function(data, categoria_col) {
  data %>%
    filter(tra_oper == 1) %>%
    distinct(id_estado, id_municipio, .keep_all = TRUE) %>%
    group_by(.data[[categoria_col]]) %>%
    summarise(Cobertura = n() / total_municipios * 100) %>%
    rename(Categoria = .data[[categoria_col]])
}

# Cobertura por clasificación poblacional
poblacion_2017 <- calcular_cobertura_categoria(ptar_filtradas2017, "categoria_poblacion") %>% mutate(Año = 2017)
poblacion_2019 <- calcular_cobertura_categoria(ptar_filtradas2019, "categoria_poblacion") %>% mutate(Año = 2019)
poblacion_2021 <- calcular_cobertura_categoria(ptar_filtradas2021, "categoria_poblacion") %>% mutate(Año = 2021)

# Unir resultados
cobertura_poblacional <- bind_rows(poblacion_2017, poblacion_2019, poblacion_2021)

# Gráfico 
ggplot(cobertura_poblacional, aes(x = factor(Año), y = Cobertura, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f%%", Cobertura)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5, family = "Times New Roman") +
  scale_fill_manual(values = c("#08306B", "#08519C", "#2171B5", "#6b8ea1")) +  
  labs(
    title = "Cobertura Territorial de PTAR en operación por clasificación poblacional",
    x = "Año",
    y = "Cobertura (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),          
    axis.title = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman", face = "bold", size = 14)
  )


# Cobertura por nivel de desarrollo económico
desarrollo_2017 <- calcular_cobertura_categoria(ptar_filtradas2017, "gdh") %>% mutate(Año = 2017)
desarrollo_2019 <- calcular_cobertura_categoria(ptar_filtradas2019, "gdh") %>% mutate(Año = 2019)
desarrollo_2021 <- calcular_cobertura_categoria(ptar_filtradas2021, "gdh") %>% mutate(Año = 2021)

cobertura_desarrollo <- bind_rows(desarrollo_2017, desarrollo_2019, desarrollo_2021)

# Gráfico
ggplot(cobertura_desarrollo, aes(x = factor(Año), y = Cobertura, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f%%", Cobertura)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5, family = "Times New Roman") +
  scale_fill_manual(values = c("#08306B", "#08519C", "#2171B5", "#6baed6")) +  # 4 tonos de azul
  labs(
    title = "Cobertura territorial de PTAR en operación por nivel de desarrollo económico",
    x = "Año",
    y = "Cobertura (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),          
    axis.title = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman", face = "bold", size = 14)
  )
