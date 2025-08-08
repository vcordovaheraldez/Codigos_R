
### Hipótesis 1 ###
# Indicador 1: Sumatoria del caudal tratado por año de las PTAR con un régimen privado 

# Paquetes utilizados:
library(dplyr)
library(ggplot2)
library(scales)

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
  # y omitir los valores 0 y NA para la variable caudal tratado.

ptar_filtradas2017 <- Base_2017m %>%
  filter(!is.na(cau_trat) & cau_trat > 0 & cau_trat <= 35000)

ptar_filtradas2019 <- Base_2019m %>%
  filter(!is.na(cau_trat) & cau_trat > 0 & cau_trat <= 35000)

ptar_filtradas2021 <- Base_2021m %>%
  filter(!is.na(cau_trat) & cau_trat > 0 & cau_trat <= 35000)

# Filtrar NA en las variables régimen jurídico, categoría poblacional y el desarrollo económico para cada año 
Base_filtrada2017 <- ptar_filtradas2017 %>%
  filter(
    !is.na(tipo_regimen),
    !is.na(categoria_poblacion),
    !is.na(gdh)
  )

Base_filtrada2019 <- ptar_filtradas2019 %>%
  filter(
    !is.na(tipo_regimen),
    !is.na(categoria_poblacion),
    !is.na(gdh)
  )

Base_filtrada2021 <- ptar_filtradas2021 %>%
  filter(
    !is.na(tipo_regimen),
    !is.na(categoria_poblacion),
    !is.na(gdh)
  )

# Suma del caudal tratado por régimen jurídico (público - privado)
suma_caudal <- data.frame(
  Año = c(2017, 2019, 2021),
  
  Caudal_Privado = c(
    sum(Base_filtrada2017$cau_trat[Base_filtrada2017$tipo_regimen == "Privado"], na.rm = TRUE),
    sum(Base_filtrada2019$cau_trat[Base_filtrada2019$tipo_regimen == "Privado"], na.rm = TRUE),
    sum(Base_filtrada2021$cau_trat[Base_filtrada2021$tipo_regimen == "Privado"], na.rm = TRUE)
  ),
  
  Caudal_Publico = c(
    sum(Base_filtrada2017$cau_trat[Base_filtrada2017$tipo_regimen == "Público"], na.rm = TRUE),
    sum(Base_filtrada2019$cau_trat[Base_filtrada2019$tipo_regimen == "Público"], na.rm = TRUE),
    sum(Base_filtrada2021$cau_trat[Base_filtrada2021$tipo_regimen == "Público"], na.rm = TRUE)
  )
)

# Tabla
suma_caudal

# Agregar columna de año
Base_filtrada2017$Año <- 2017
Base_filtrada2019$Año <- 2019
Base_filtrada2021$Año <- 2021

# Unir las tres bases
Base_completa <- bind_rows(Base_filtrada2017, Base_filtrada2019, Base_filtrada2021)

# Agrupar y sumar caudal tratado
caudal_agrupado <- Base_completa %>%
  group_by(Año, tipo_regimen, categoria_poblacion, gdh) %>%
  summarise(Caudal_tratado = sum(cau_trat), .groups = "drop")

# Tabla
View(caudal_agrupado)

# Descargar tabla
write.csv(caudal_agrupado, file = "CaudalTratado_Poblacion_Regimen_gdh.csv", row.names = FALSE)

# Reordenar niveles de gdh
caudal_agrupado$gdh <- factor(caudal_agrupado$gdh,
                              levels = c("MUY ALTO", "ALTO", "MEDIO", "BAJO"))

# Porcentaje del caudal tratado de las PTAR con un regimen privado
caudal_porcentaje <- caudal_agrupado %>%
  group_by(Año, categoria_poblacion, gdh) %>%
  mutate(
    total_caudal = sum(Caudal_tratado),
    porcentaje_privado = ifelse(tipo_regimen == "Privado",
                                (Caudal_tratado / total_caudal) * 100,
                                NA)
  ) %>%
  ungroup()

# Tipografía 
windowsFonts(Times = windowsFont("Times New Roman"))

# Colores de la paleta para el gráfico 
ggplot(caudal_porcentaje, aes(x = categoria_poblacion, y = Caudal_tratado, fill = tipo_regimen)) +
  geom_bar(stat = "identity", position = "dodge", color = "gray30") +
  
# Gráfico
  geom_label(
    data = caudal_porcentaje %>% filter(tipo_regimen == "Privado"),
    aes(label = paste0(round(porcentaje, 1), "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5,
    family = "Times",
    fill = "white",       # fondo claro
    color = "black"       # texto oscuro
  ) +
  facet_grid(gdh ~ Año) +
  labs(
    title = "Caudal tratado por régimen, categoría poblacional y nivel de desarrollo económico",
    x = "Categoría poblacional",
    y = "Caudal tratado (l/s)",
    fill = "Régimen"
  ) +
  scale_fill_manual(
    values = c("Público" = "#1f3552", "Privado" = "#3c6e91")
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +  # espacio extra arriba
  theme_minimal(base_size = 12, base_family = "Times") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "#f0f0f0", color = "black"),
    strip.text = element_text(face = "bold", family = "Times"),
    plot.title = element_text(family = "Times", face = "bold"),
    axis.title = element_text(family = "Times"),
    legend.text = element_text(family = "Times"),
    legend.title = element_text(family = "Times")
  )

#################################################################################################################

### Hipótesis 1 ###
# Indicador 2: Sumatoria del porcentaje del caudal territorial tratado por año de las PTAR con un régimen privado 

# Paquetes utilizados:
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Total de municipios en México
total_municipios_mexico <- 2478 # Dato del INEGI (2025)

# Porcentaje de municipios con al menos una PTAR en operación
calc_porcentaje_ptar <- function(base) {
  ptar_operando <- base[base$tra_oper == 1, ]
  municipios_con_ptar <- length(unique(ptar_operando$Municipio))
  porcentaje <- (municipios_con_ptar / total_municipios_mexico) * 100
  return(porcentaje)
}

# Porcentajes para cada año
porcentaje_2017 <- calc_porcentaje_ptar(ptar_filtradas2017)
porcentaje_2019 <- calc_porcentaje_ptar(ptar_filtradas2019)
porcentaje_2021 <- calc_porcentaje_ptar(ptar_filtradas2021)


# Tabla resumen
tabla_porcentajes <- data.frame(
  Año = c(2017, 2019, 2021),
  Porcentaje_Municipios_con_Plantas = c(
    porcentaje_2017,
    porcentaje_2019,
    porcentaje_2021
  )
)

# Tabla
tabla_porcentajes

# Porcentaje por año
porcentaje_privadas <- function(base) {
  privadas_operando <- base[base$tra_oper == 1 & base$tipo_regimen == "Privado", ]
  municipios_unicos <- length(unique(privadas_operando$Municipio))
  porcentaje <- (municipios_unicos / 2478) * 100
  return(porcentaje)
}

porcentaje_2017 <- porcentaje_privadas(Base_2017m)
porcentaje_2019 <- porcentaje_privadas(Base_2019m)
porcentaje_2021 <- porcentaje_privadas(Base_2021m)

# Tabla resumen
tabla_privadas <- data.frame(
  Año = c(2017, 2019, 2021),
  Porcentaje_Municipios_con_PTAR_Privada = c(
    porcentaje_2017,
    porcentaje_2019,
    porcentaje_2021
  )
)

# Tabla
tabla_privadas

# Total de municipios en México según INEGI (2025)
total_municipios_mexico <- 2478

# Función general para calcular porcentajes
calc_porcentaje_general <- function(base, variable, anio) {
  base %>%
    filter(tra_oper == 1) %>%
    group_by(across(all_of(variable))) %>%
    summarise(municipios_unicos = n_distinct(Municipio), .groups = "drop") %>%
    mutate(
      porcentaje = (municipios_unicos / total_municipios_mexico) * 100,
      Año = anio,
      Categoria = !!sym(variable),
      Tipo_Categoria = variable
    ) %>%
    select(Año, Tipo_Categoria, Categoria, municipios_unicos, porcentaje)
}

# Aplicar para las tres variables y tres años
tabla_general <- bind_rows(
  # Por tipo_regimen
  calc_porcentaje_general(ptar_filtradas2017, "tipo_regimen", 2017),
  calc_porcentaje_general(ptar_filtradas2019, "tipo_regimen", 2019),
  calc_porcentaje_general(ptar_filtradas2021, "tipo_regimen", 2021),
  
  # Por categoria_poblacion
  calc_porcentaje_general(ptar_filtradas2017, "categoria_poblacion", 2017) %>% filter(Categoria != ""),
  calc_porcentaje_general(ptar_filtradas2019, "categoria_poblacion", 2019) %>% filter(Categoria != ""),
  calc_porcentaje_general(ptar_filtradas2021, "categoria_poblacion", 2021) %>% filter(Categoria != ""),
  
  # Por gdh
  calc_porcentaje_general(ptar_filtradas2017, "gdh", 2017),
  calc_porcentaje_general(ptar_filtradas2019, "gdh", 2019),
  calc_porcentaje_general(ptar_filtradas2021, "gdh", 2021)
)

# Ver tabla
View(tabla_general)

# Descargar tabla
write.csv(tabla_general, file = "CaudalTerritorial_Poblacion_Regimen_gdh.csv", row.names = FALSE)

# Filtrar las filas con NA en Categoria
tabla_filtrada <- tabla_general %>%
  filter(!is.na(Categoria))

# Paleta para 10 categorías
paleta_azul_oscura <- colorRampPalette(brewer.pal(9, "Blues")[4:9])(10)

# Gráfico
ggplot(tabla_filtrada, aes(x = factor(Año), y = porcentaje, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = round(porcentaje, 1)),  # Etiquetas redondeadas a 1 decimal
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.5,
    family = "Times New Roman"
  ) +
  facet_wrap(~ Tipo_Categoria, scales = "free") +
  scale_fill_manual(values = paleta_azul_oscura) +
  labs(
    title = "Porcentaje de municipios con al menos una PTAR en operación por categoría.",
    x = "Año",
    y = "Porcentaje (%)",
    fill = "Categoría"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    strip.text = element_text(face = "bold"),
    text = element_text(size = 12)
  )

#################################################################################################################

### Hipótesis 1 ###
# Caudal tratado - Inventario ALC 2022

# Unión de ambas bases (Inventario - IDH)
Inventario_Mexicom <- left_join(Inventario_Mexico, IDH_2015, by = "Estado")

# Renombra columna
Inventario_Mexicom <- Inventario_Mexicom %>%
  rename(cau_trat = `15. Caudal de operación promedio (L/s)`)

# Base de clasificación poblacional
pobmun <- read.csv("https://raw.githubusercontent.com/asalazaradams/data/main/PobMuni20.csv")

# Rangos y etiquetas
pobmun$categoria_poblacion <- cut(
  pobmun$POBTOT,
  breaks = c(-Inf, 5000, 50000, 300000, Inf),  
  labels = c("Rurales", "Semiurbanos", "Urbanos", "Metropolitanos"),  
  right = TRUE)

# Resultado
print(pobmun)

# Reenombar variables
pobmun = dplyr::rename(pobmun, plnt_folio = clave_mun)

# Union Unión de ambas bases (Inventario - Clasificación poblacional)
Inventario_Mexicom <- left_join(Inventario_Mexicom, pobmun, by = "plnt_folio")

# Descargar 
write.csv(Inventario_Mexicom, file = "Inventario_Mexicom.csv", row.names = FALSE)

# Variable cau_trat a formato numérico
Inventario_Mexicom$cau_trat <- as.numeric(Inventario_Mexicom$cau_trat)

# Omitir NA
Inventario_Mexicom <- Inventario_Mexicom %>%
  filter(
    !is.na(tipo_regimen),
    !is.na(categoria_poblacion),
    !is.na(gdh)
  )

# Filtrar las PTAR con caudal tratado igual o menor a 35,000 l/s (límite máximo según la PTAR Atotonilco) 
# y omitir los valores 0 y NA para la variable caudal tratado.

Inventario_Mexicom <- Inventario_Mexicom %>%
  filter(!is.na(cau_trat) & cau_trat > 0 & cau_trat <= 35000)

# Suma del caudal tratado en 2022
suma_caudal <- sum(Inventario_Mexicom$cau_trat, na.rm = TRUE)

print(suma_caudal)
sprintf("%.2f", suma_caudal)  # Resultado con 2 decimales

# Agrupar y sumar el caudal por tipo de regimen 
caudal_por_regimen <- Inventario_Mexicom %>%
  group_by(tipo_regimen) %>%
  summarise(
    suma_caudal = sum(cau_trat, na.rm = TRUE)
  )

#Resultado
print(caudal_por_regimen)

# caudal desagregado por tipo de régimen, clasificación poblacional y desarrollo económico
caudal_desagregado <- Inventario_Mexicom %>%
  filter(tipo_regimen %in% c("Privado", "Publico")) %>%  
  group_by(tipo_regimen, categoria_poblacion, gdh) %>%
  summarise(
    suma_caudal = sum(cau_trat, na.rm = TRUE),
    .groups = "drop"  
  )

# Resultado
print(caudal_desagregado)

# Descargar 
write.csv(caudal_desagregado, file = "caudal_desagregado.csv", row.names = FALSE)

# Porcentaje del caudal 
caudal_porcentaje <- caudal_desagregado %>%
  group_by(gdh, categoria_poblacion) %>%
  mutate(
    porcentaje = suma_caudal / sum(suma_caudal) * 100
  ) %>%
  ungroup()

# Tipografía
windowsFonts(Times = windowsFont("Times New Roman"))  # Solo en Windows

# Gráfico
ggplot(caudal_porcentaje, aes(x = categoria_poblacion, y = suma_caudal, fill = tipo_regimen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3,
    family = "Times"
  ) +
  facet_wrap(~ gdh) +
  labs(
    title = "Caudal tratado por tipo de régimen, categoría de población y GDH",
    x = "Categoría población",
    y = "Caudal tratado (l/s)",
    fill = "Tipo de régimen"
  ) +
  scale_fill_manual(
    values = c("Publico" = "#2171B5", "Privado" = "#1f3552")
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    text = element_text(family = "Times"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
