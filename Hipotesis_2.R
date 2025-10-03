
### Hipótesis 2: Porcentaje de cobertura del reúso de aguas residuales tratadas (2022) ###

# Bases de datos utilizadas:  
# Inventario_Mexico_con_Municipio
# IDH_2015
# Reuso_clasificado_manual

# Librerías utilizadas: 
library(dplyr)
library(ggplot2)
library(readr)
library(extrafont)

#####################################################################################################################

# Nombres de columnas
colnames(Inventario_Mexico_con_Municipio)

# Renombrar columnas
Inventario_Mexico_con_Municipio <- Inventario_Mexico_con_Municipio %>%
  rename(Caudal= "15. Caudal de operación promedio (L/s)")

Inventario_Mexico_con_Municipio <- Inventario_Mexico_con_Municipio %>%
  rename(Reuso = "20. Destino del agua residual")

Inventario_Mexico_con_Municipio <- Inventario_Mexico_con_Municipio %>%
  rename(Categoria = "20a. Categoría de destino")

Inventario_Mexico_con_Municipio <- Inventario_Mexico_con_Municipio %>%
  rename(Notas = "27 Notas")

#####################################################################################################################

# Unión de bases

# IDH
base_unida <- left_join(Inventario_Mexico_con_Municipio,IDH_2015,by = c("Estado", "Municipio"))

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

# Descargar 
write.csv(base_completa, file = "base_completa.csv", row.names = FALSE)

#####################################################################################################################

# Variable Caudal a numérico 
Reuso_clasificado_manual$Caudal <- as.numeric(gsub(",", "", Reuso_clasificado_manual$Caudal))

# Resumen por tipo de reuso
caudal_total <- Reuso_clasificado_manual %>%
  group_by(tipo_reuso) %>%
  summarise(
    numero_plantas = n(),
    caudal_total = sum(Caudal, na.rm = TRUE),
    caudal_promedio = caudal_total / numero_plantas
  )

print(caudal_total)

# Descargar 
write.csv(caudal_total, file = "caudal_total.csv", row.names = FALSE)

#####################################################################################################################

# Base sin la PTAR Atotonilco de Tula
Reuso_filtrado_sin_Atotonilco <- Reuso_clasificado_manual %>%
  filter(`2. Nombre de la planta de tratamiento` != "Atotonilco de Tula")

# Resumen por tipo de reuso
caudal <- Reuso_filtrado_sin_Atotonilco %>%
  group_by(tipo_reuso) %>%
  summarise(
    numero_plantas = n(),
    caudal_total = sum(Caudal, na.rm = TRUE),
    caudal_promedio = caudal_total / numero_plantas
  )

print(caudal)

#####################################################################################################################

# Filtrar reúso agrícola  
datos_agricola <- Reuso_clasificado_manual %>%
  filter(tipo_reuso == "Agricola")

# Reuso agrícola por Estados 
Estado_agri <- datos_agricola %>%
  group_by(Estado) %>%
  summarise(Caudal_total = sum(Caudal, na.rm = TRUE)) %>%
  arrange(desc(Caudal_total))

head(Estado_agri, 10)

# Filtrar reúso en áreas verdes  
datos_AreasVerdes <- Reuso_clasificado_manual %>%
  filter(tipo_reuso == "Areas verdes")

# Reuso áreas verdes por Estados 
Estado_AreasVerdes <- datos_AreasVerdes %>%
  group_by(Estado) %>%
  summarise(Caudal_total = sum(Caudal, na.rm = TRUE)) %>%
  arrange(desc(Caudal_total))

head(Estado_AreasVerdes, 10)

# Filtrar reúso industrial
datos_industrial <- Reuso_clasificado_manual %>%
  filter(tipo_reuso == "Industrial")

# Reuso industrial por Estados 
Estado_industrial <- datos_industrial %>%
  group_by(Estado) %>%
  summarise(Caudal_total = sum(Caudal, na.rm = TRUE)) %>%
  arrange(desc(Caudal_total))

head(Estado_industrial, 10)

#####################################################################################################################

# Filtrar reuso 
df_reuso <- Reuso_clasificado_manual %>%
  filter(!is.na(tipo_reuso)) %>%
  filter(!tipo_reuso %in% c("Reuso no especificado"))

# Resumir datos: número de plantas por Estado y tipo de reúso
df_resumen <- df_reuso %>%
  group_by(Estado, tipo_reuso) %>%
  summarise(n_plantas = n(), .groups = "drop")

# Gráfico 
ggplot(df_resumen, aes(x = Estado, y = n_plantas, fill = tipo_reuso)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n_plantas), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            family = "Times New Roman") +
  scale_fill_manual(values = c(
    "Agricola" = "orange",
    "Industrial" = "steelblue",
    "Areas verdes" = "darkgreen",
    "Generacion de energia" = "purple",
    "Mineria" = "brown"
  )) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    x = "Estado",
    y = "Número de Plantas",
    fill = "Tipo de Reúso"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman", face = "bold")
  )

#####################################################################################################################

# Caudal total por régimen, población y desarrollo económico
resumen_reuso <- Reuso_clasificado_manual %>%
  group_by(tipo_reuso,tipo_regimen, categoria_poblacion, gdh) %>%
  summarise(
    Numero_de_Plantas = n(),
    Suma_Caudal = sum(Caudal, na.rm = TRUE),
    .groups = "drop"
  )

print(resumen_reuso)

# Descargar 
write.csv(resumen_reuso, file = "resumen_reuso.csv", row.names = FALSE)
