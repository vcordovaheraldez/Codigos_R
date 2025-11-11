
# Hipótesis 3
  # Indicadores: volumen tratado en m³ (2019), nivel de tratamiento, y los parámetros asociados con la pendiente de 
  # los modelos de regresión

# Base de datos utilizada:   
  # Base_2019m

# Librerías utilizadas:
library(dplyr)
library(ggplot2)
library(broom)
library(car)
library(lmtest)

#####################################################################################################################

# Omitir valores NA y 0 en las variables utilizadas 
Base_2019m <- Base_2019m %>%
  mutate(across(c(tra_peso, vol_trat), ~ ifelse(.x == 0, NA, .x)))

Base_2019m <- Base_2019m %>%
  filter(tra_regi_a != "No Aplica")

Base_2019m <- Base_2019m %>%
  filter(tr_po_pl_a != "No Aplica")

# Transformar variables a factor 
Base_2019m$tra_regi_a <- as.factor(Base_2019m$tra_regi_a)

Base_2019m$categoria_poblacion <- as.factor(Base_2019m$categoria_poblacion)

Base_2019m$gdh <- as.factor(Base_2019m$gdh)

#####################################################################################################################

# Modelo 1

# Modelo 
modelo1=lm(log(tra_peso)~log(vol_trat), data=Base_2019m)
summary(modelo1)

# Correlación de Pearson
cor(log(Base_2019m$tra_peso), log(Base_2019m$vol_trat), use = "complete.obs")

# Correlación de Pearson manual
x <- log(Base_2019m$vol_trat)
y <- log(Base_2019m$tra_peso)

# Omitir NA 
datos <- na.omit(data.frame(tra_peso = Base_2019m$tra_peso,
                            vol_trat = Base_2019m$vol_trat))

cov_xy <- cov(log(datos$tra_peso), log(datos$vol_trat))
var_x <- var(log(datos$tra_peso))
var_y <- var(log(datos$vol_trat))

r <- cov_xy / sqrt(var_x * var_y)
r

# Dispersión de valores  
plot(Base_2019m$tra_peso, Base_2019m$vol_trat)

# Escala logarítmica por los valores dispersos 
plot(log(Base_2019m$tra_peso), Base_2019m$vol_trat, 
     main="Escala Logarítmica en tra_peso", col="#00688B", pch=19)

plot(Base_2019m$tra_peso, log(Base_2019m$vol_trat), 
     main="Escala Logarítmica en vol_trat", col="#00688B", pch=19)

plot(log(Base_2019m$tra_peso), log(Base_2019m$vol_trat), 
     main="Ambos ejes en Log", col="#00688B", pch=19)

#####################################################################################################################

# Modelo 2

# Crear variable nivel_tratamiento
Base_2019m <- Base_2019m %>%
  mutate(
    nivel_tratamiento = case_when(
      tr_po_pl_a == "Lagunas de estabilización" ~ "Secundario",
      tr_po_pl_a == "Lodos activados" ~ "Secundario",
      tr_po_pl_a == "RAFA o UASB" ~ "Secundario",
      tr_po_pl_a == "Fosa séptica + Humedal" ~ "Secundario",
      tr_po_pl_a == "Fosa séptica" ~ "Primario",
      tr_po_pl_a == "Anaerobio" ~ "Secundario",
      tr_po_pl_a == "Humedales construidos" ~ "Secundario",
      tr_po_pl_a == "RAFA + Filtro biológico" ~ "Secundario",
      tr_po_pl_a == "Tanque Imhoff" ~ "Primario",
      tr_po_pl_a == "Reactor enzimático" ~ "Secundario",
      tr_po_pl_a == "Filtros biológicos/rociadores/percoladores" ~ "Secundario",
      tr_po_pl_a == "Lagunas aireadas" ~ "Secundario",
      tr_po_pl_a == "RAFA + UASB + Humedal" ~ "Secundario",
      tr_po_pl_a == "Fosa séptica + Filtro biológico" ~ "Secundario",
      tr_po_pl_a == "Biológico" ~ "Secundario",
      tr_po_pl_a == "Discos biológicos/biodiscos" ~ "Secundario",
      tr_po_pl_a == "Dual" ~ "Secundario/Terciario",
      tr_po_pl_a == "Tanque Imhoff + Filtro biológico" ~ "Primario mejorado",
      tr_po_pl_a == "Sedimentación + Humedal" ~ "Primario mejorado",
      tr_po_pl_a == "Primario/Sedimentación" ~ "Primario",
      tr_po_pl_a == "Zanjas de oxidación" ~ "Secundario",
      tr_po_pl_a == "Primario avanzado" ~ "Primario",
      tr_po_pl_a == "Aerobio" ~ "Secundario",
      tr_po_pl_a == "Terciario" ~ "Terciario",
      tr_po_pl_a == "Tanque Imhoff + Humedal" ~ "Primario mejorado",
      tr_po_pl_a == "Otro" ~ "Otro",
      TRUE ~ NA_character_ # Asignar NA para cualquier valor que no coincida
    )
  )

# Omitir NA 
Base_2019m <- Base_2019m %>%
  filter(nivel_tratamiento != "No Aplica")

# Transformar variable a factor 
Base_2019m$nivel_tratamiento <- as.factor(Base_2019m$nivel_tratamiento)

# variable de referencia 
Base_2019m$nivel_tratamiento <- relevel(Base_2019m$nivel_tratamiento, ref = "Terciario")

# Modelo 
modelo2=lm(log(tra_peso)~log(vol_trat)+nivel_tratamiento, data=Base_2019m)
summary(modelo2)

levels(Base_2019m$nivel_tratamiento)

# Gráfico de coeficientes con intervalos de confianza
modelo_2 <- tidy(modelo2, conf.int = TRUE) %>%
  filter(term != "(Intercept)") # Excluir el intercepto para mejor visualización

ggplot(modelo_2, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Gráfico de Coeficientes con Intervalos de Confianza",
       y = "Variable Predictora",
       x = "Estimación del Coeficiente") +
  theme_minimal()

#####################################################################################################################

# Modelo 3

# Recodificar variable
Base_2019m <- Base_2019m %>%
  mutate(
    tipo_regimen = case_when(
      tra_regi_a %in% c("Estatal", "Intermunicipal", "Municipal", "Otro publico", "Social") ~ "Público",
      tra_regi_a == "Privado" ~ "Privado",
      TRUE ~ NA_character_ # Para cualquier otro valor que no esté en las categorías anteriores, asigna NA
    )
  )

# Resultados
table(Base_2019m$tra_regi_a, Base_2019m$tipo_regimen)

# Transfomar a factor
Base_2019m$tipo_regimen <- as.factor(Base_2019m$tipo_regimen)

# Variable de referencia 
Base_2019m$tipo_regimen <- relevel(Base_2019m$tipo_regimen, ref = "Público")

# Modelo 
modelo3=lm(log(tra_peso)~log(vol_trat)+nivel_tratamiento+tipo_regimen, data=Base_2019m)
summary(modelo3)

# Gráfico de coeficientes con intervalos de confianza 
modelo_3 <- tidy(modelo3, conf.int = TRUE) %>%
  filter(term != "(Intercept)") # Excluir el intercepto para mejor visualización

ggplot(modelo_3, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Gráfico de Coeficientes con Intervalos de Confianza",
       y = "Variable Predictora",
       x = "Estimación del Coeficiente") +
  theme_minimal()

#####################################################################################################################

# Modelo 4

# Variable de referencia 
Base_2019m$categoria_poblacion <- relevel(Base_2019m$categoria_poblacion, ref = "Metropolitanos") 

# Modelo 
modelo4=lm(log(tra_peso)~log(vol_trat)+nivel_tratamiento+tipo_regimen+categoria_poblacion, data=Base_2019m)
summary(modelo4)

# Gráfico de coeficientes con intervalos de confianza 
modelo_4 <- tidy(modelo4, conf.int = TRUE) %>%
  filter(term != "(Intercept)") # Excluir el intercepto para mejor visualización

ggplot(modelo_4, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Gráfico de Coeficientes con Intervalos de Confianza",
       y = "Variable Predictora",
       x = "Estimación del Coeficiente") +
  theme_minimal()

#####################################################################################################################

# Modelo 5

# Variable de referencia 
Base_2019m$gdh <- relevel(Base_2019m$gdh, ref = "MUY ALTO")

# Modelo 
modelo5=lm(log(tra_peso)~log(vol_trat)+nivel_tratamiento+tipo_regimen+categoria_poblacion+gdh, data=Base_2019m)
summary(modelo5)

# Diagrama boxplot
ggplot(Base_2019m, aes(x = gdh, y = tra_peso, fill = gdh)) +
  geom_boxplot() +
  scale_y_log10() + # Opcional si los datos son muy dispersos
  theme_minimal() +
  labs(
    title = "Costo energético según categoría del IDH",
    x = "Categoría del IDH",
    y = "Costo energético (escala log)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de coeficientes con intervalos de confianza 
modelo_5 <- tidy(modelo5, conf.int = TRUE) %>%
  filter(term != "(Intercept)") # Excluir el intercepto para mejor visualización

ggplot(modelo_5, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Gráfico de Coeficientes con Intervalos de Confianza",
       y = "Variable Predictora",
       x = "Estimación del Coeficiente") +
  theme_minimal()

# Multicolinealidad - VIF (Variance Inflation Factor)
vif(modelo5)

# Homocedasticidad (Varianza constante)
bptest(modelo5)

# Normalidad de residuos - Prueba de Shapiro-Wilk 
shapiro.test(resid(modelo5))

# Descargar
write.csv(Base_2019m, file = "Base_completa.csv", row.names = FALSE)

#####################################################################################################################

# Modelos por nivel de tratamiento 

# Agrupar niveles de tratamiento  
Base_2019m <- Base_2019m %>%
  mutate(
    nivel_tratamiento2 = case_when(
      nivel_tratamiento %in% c("Primario", "Primario mejorado") ~ "Primario",
      nivel_tratamiento %in% c("Secundario") ~ "Secundario",
      nivel_tratamiento %in% c("Secundario/Terciario", "Terciario") ~ "Terciario",
      TRUE ~ NA_character_  
    )
  )

# Modelos 
modelo_primario=lm(log(tra_peso)~log(vol_trat)+tipo_regimen+categoria_poblacion+gdh, data=filter(Base_2019m, nivel_tratamiento2 == "Primario"))
summary(modelo_primario)

modelo_secundario=lm(log(tra_peso)~log(vol_trat)+tipo_regimen+categoria_poblacion+gdh, data=filter(Base_2019m, nivel_tratamiento2 == "Secundario"))
summary(modelo_secundario)

modelo_terciario=lm(log(tra_peso)~log(vol_trat)+tipo_regimen+categoria_poblacion+gdh, data=filter(Base_2019m, nivel_tratamiento2 == "Terciario"))
summary(modelo_terciario)

# Comparar coeficientes
comparacion_coef <- bind_rows(
  tidy(modelo_primario) %>% mutate(Modelo = "Primario"),
  tidy(modelo_secundario) %>% mutate(Modelo = "Secundario"),
  tidy(modelo_terciario) %>% mutate(Modelo = "Terciario")
)

comparacion_coef %>%
  select(Modelo, term, estimate, std.error, p.value)

# Descargar
write.csv(comparacion_coef, file = "comparacion_coef.csv", row.names = FALSE)
