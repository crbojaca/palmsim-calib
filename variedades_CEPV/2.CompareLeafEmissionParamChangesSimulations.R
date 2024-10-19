##### Comparación de las simulaciones de los cambios en los parámetros de emisión de las hojas #####
library(data.table)

# Cargar los datos de las simulaciones
sim_run_original <- fread("variedades_CEPV/PALMSIM_output_modelo_original.csv")
sim_run_modified <- fread("variedades_CEPV/PALMSIM_output_leafemissionpar_modified.csv")
observados <- fread("variedades_CEPV/Variedades_CEPV Historial variedades_mes y año_at.csv")

# Comparar los rendimientos
sim_run_original[, rendimiento_t_ha := `generative_bunch_count_daily (1/ha/day)` * `generative_bunch_weight (kg)` / 1000]
sim_run_original[, Escenario := "Modelo original"]
sim_run_modified[, rendimiento_t_ha := `generative_bunch_count_daily (1/ha/day)` * `generative_bunch_weight (kg)` / 1000]
sim_run_modified[, Escenario := "Emisión de hojas modificada"]

observados[, Fecha := dmy(Fecha)]
observados_agregado <- observados[, .(rendimiento_t_ha = mean(Ton_ha)), by = .(Año = Fecha)]
# observados_agregado <- observados_agregado[, .(rendimiento_t_ha = sum(rendimiento_t_ha)), by = .(Año = year(Fecha))]
observados_agregado[, Escenario := "Observado"]

# Unir los datos
sim_run <- rbind(sim_run_original, sim_run_modified)

# Agrupo los datos por fecha y escenario
sim_run_agregado <- sim_run[, .(rendimiento_t_ha = sum(rendimiento_t_ha)), by = .(Año = floor_date(date, 'month'), Escenario)]

sim_run_agregado <- rbind(sim_run_agregado, observados_agregado)

# Graficar
ggplot(sim_run_agregado, aes(x = Año, y = rendimiento_t_ha, color = Escenario)) +
  geom_line() +
  labs(title = "Comparación de los rendimientos de la simulación de PALMSIM",
       x = "Fecha",
       y = "Rendimiento (t/ha)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Modelo original" = "blue", 
                                "Emisión de hojas modificada" = "red",
                                "Observado" = "black"))

