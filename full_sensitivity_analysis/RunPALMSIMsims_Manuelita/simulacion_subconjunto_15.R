# Cargar el modelo
source('full_sensitivity_analysis/1.RunPALMSIMsims_function.R')

mat <- readRDS(here('full_sensitivity_analysis/sobol_sample_matrix.rds'))

# Generar los puntos de corte
puntos <- seq(0, 6900, length.out = 21)

# Crear una lista para almacenar los subconjuntos de la matriz
subconjuntos <- list()

# Dividir la matriz en segmentos con base en los índices de los puntos de corte
for (i in 1:(length(puntos) - 1)) {
  subconjuntos[[i]] <- mat[(puntos[i] + 1):puntos[i + 1], ]
}

##### Run simulations 
subconjunto <- 15

for(i in 1:nrow(subconjuntos[[subconjunto]])){
  mat_sel <- subconjuntos[[subconjunto]][i, ]
  palmsim_function(X = mat_sel, 
                   location = 5,
                   sim_name = paste0('sim_', subconjunto, '_', i),
                   clima_path = here('full_sensitivity_analysis/ClimateData_Manuelita_SAMARIA_322_2009-01-01_2019-01-01_RadiacionNASA.csv'),
                   save_path = 'D:/2024_Calibracion_PALMSIM/AnalisisSensibilidadCompleto/Simulaciones_ZO_Manuelita/Results_')
}

