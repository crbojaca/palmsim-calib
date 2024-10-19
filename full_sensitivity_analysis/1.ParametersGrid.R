library(data.table)

##### Generación de la combinación de parámetros para realizar el análisis de sensibilidad
param <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))

##### Variación a aplicar a cada parámetro
variation <- c(0.8, 0.9, 1.1, 1.2)

##### Crear la combinación de parámetros
param_variation <- param %>%
  select(component, parameter, value_0 = value) %>%
  mutate(
    value_1 = value_0 * variation[1],
    value_2 = value_0 * variation[2],
    value_3 = value_0 * variation[3],
    value_4 = value_0 * variation[4]
  )

##### Corrijo las proporciones de fraction_leaflets y fraction_rachis para que sumen 1
param_variation <- param_variation %>%
  mutate(
    value_1 = ifelse(parameter == 'fraction_rachis', 1 - value_1[parameter == 'fraction_leaflets'], value_1),
    value_2 = ifelse(parameter == 'fraction_rachis', 1 - value_2[parameter == 'fraction_leaflets'], value_2),
    value_3 = ifelse(parameter == 'fraction_rachis', 1 - value_3[parameter == 'fraction_leaflets'], value_3),
    value_4 = ifelse(parameter == 'fraction_rachis', 1 - value_4[parameter == 'fraction_leaflets'], value_4)
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Genero grilla de cinco mil combinaciones de parámetros
# Número de iteraciones
num_iterations <- 5000

# Inicializar una lista para almacenar los resultados
result_list <- vector("list", num_iterations)

for (i in 1:num_iterations) {
  # Supongamos que param_variation es un data frame existente
  result_list[[i]] <- param_variation %>%
    rowwise() %>%
    mutate(iteration = paste0('set_', i),
      value_selected = sample(c(value_0, value_1, value_2, value_3, value_4), 1)) %>%
    select(component, parameter, iteration, value_selected)
}

# Combinar todos los data frames en la lista en uno solo
combined_results <- bind_rows(result_list) %>%
  pivot_wider(names_from = iteration, values_from = value_selected)
  
# Mostrar las primeras filas del resultado combinado
head(combined_results)

# Guardar el resultado combinado en un archivo CSV
fwrite(combined_results, "full_sensitivity_analysis/ParametersGridForSensitivity.csv")





