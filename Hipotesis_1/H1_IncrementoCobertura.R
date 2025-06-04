
  # Hipótesis 1. El nivel de cobertura del tratamiento de aguas residuales en México ha mostrado un incremento significativo.

# Paquetes utilizados
library(ggplot2)
library(gridExtra)
library(readr)
library(tidyr)

datos <- data.frame(
  Año = c(2017, 2019, 2021, 2023),
  Plantas = c(2526, 2642, 2872, 2928),
  Cobertura = c(63.0, 65.7, 67.5, 67.0)
)

# Transformar datos en tabla como un data frame
tabla_conagua <- data.frame(
  Año = c(2017, 2019, 2021, 2023),
  Plantas_en_operacion = c(2526, 2642, 2872, 2928),
  Capacidad_instalada = c(181150.24, 194715.32, 198603.55, 196442.66),
  Caudal_tratado = c(135580.28, 141479.04, 145341.00, 143568.17),
  Cobertura_nacional_porcentaje = c(63.0, 65.7, 67.5, 67.0)
)

# Ver tabla
print(tabla_conagua)

# Guardar tabla en CSV
write_csv(tabla_conagua, "Tabla_H1.csv")

# Gráfico 1: Plantas en operación
g1 <- ggplot(datos, aes(x = Año, y = Plantas)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Gráfico 1: Plantas en operación", x = "Año", y = "Número de plantas") +
  theme_minimal()

# Gráfico 2: Cobertura nacional
g2 <- ggplot(datos, aes(x = Año, y = Cobertura)) +
  geom_line(color = "steelblue", linewidth = 1.5, linetype = "dashed") +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Gráfico 2: Cobertura nacional de tratamiento (%)", x = "Año", y = "Cobertura (%)") +
  theme_minimal()

# Ver ambos gráficos juntos
grid.arrange(g1, g2, ncol = 1)

# Grafico sobre la evolucion del saneaminto en Mexico (1996-2023).
# Datos
saneamiento <- data.frame(
  Año = 1996:2023,
  Plantas_Operacion = c(595, 639, 727, 777, 793, 938, 1077, 1182, 1300, 1433, 1593, 1710, 1833, 2029, 2186, 2289, 2342, 2287, 2337, 2477, 2536, 2526, 2540, 2642, 2786, 2872, 2774, 2928),
  Capacidad_Instalada = c(51696.3, 57401.7, 58560.2, 61559.0, 68970.0, 73852.6, 79735.0, 84331.5, 88718.3, 95774.3, 99764.2, 106266.7, 113024.0, 120860.9, 126847.5, 137082.1, 140142.1, 152171.9, 151883.4, 177973.6, 180569.7, 181150.2, 181152.2, 194715.3, 196749.5, 198603.6, 195536.8, 196442.7),
  Caudal_Tratado = c(33745.4, 39388.8, 40854.7, 42396.8, 45927.3, 50810.0, 56148.5, 60242.6, 64541.9, 71784.8, 74388.3, 79294.3, 83640.6, 88127.1, 93600.2, 97640.2, 99750.2, 105934.9, 111253.5, 120902.2, 123586.8, 135580.3, 137698.6, 141479.0, 144710.0, 145341.0, 143756.7, 143568.2)
)

# Convertir a formato largo (para ggplot)
saneamiento_long <- pivot_longer(saneamiento, 
                                 cols = -Año,
                                 names_to = "Variable",
                                 values_to = "Valor")

# Grafico 
ggplot(saneamiento_long, aes(x = Año, y = Valor, color = Variable)) +
  geom_line(size = 1.2) +
  labs(title = "Evolución del Saneamiento en México (1996–2023)",
       y = "Valor (Plantas / l/s)",
       color = "Indicador") +
  theme_minimal()

