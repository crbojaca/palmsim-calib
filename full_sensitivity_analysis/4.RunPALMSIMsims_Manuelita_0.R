# Generación de 20 archivos para generar trabajos independientes
for(subconjunto in 1:20){
  script <- paste0(
    "# Cargar el modelo\n",
    "source('full_sensitivity_analysis/1.RunPALMSIMsims_function.R')\n\n",
    "mat <- readRDS(here('full_sensitivity_analysis/sobol_sample_matrix.rds'))\n\n",
    "# Generar los puntos de corte\n",
    "puntos <- seq(0, 6900, length.out = 21)\n\n",
    "# Crear una lista para almacenar los subconjuntos de la matriz\n",
    "subconjuntos <- list()\n\n",
    "# Dividir la matriz en segmentos con base en los índices de los puntos de corte\n",
    "for (i in 1:(length(puntos) - 1)) {\n",
    "  subconjuntos[[i]] <- mat[(puntos[i] + 1):puntos[i + 1], ]\n",
    "}\n\n",
    "##### Run simulations \n",
    "subconjunto <- ", subconjunto, "\n\n",
    "for(i in 1:nrow(subconjuntos[[subconjunto]])){\n",
    "  mat_sel <- subconjuntos[[subconjunto]][i, ]\n",
    "  palmsim_function(X = mat_sel, \n",
    "                   location = 5,\n",
    "                   sim_name = paste0('sim_', subconjunto, '_', i),\n",
    "                   clima_path = here('full_sensitivity_analysis/ClimateData_Manuelita_SAMARIA_322_2009-01-01_2019-01-01_RadiacionNASA.csv'),\n",
    "                   save_path = 'D:/2024_Calibracion_PALMSIM/AnalisisSensibilidadCompleto/Simulaciones_ZO_Manuelita/Results_')\n",
    "}\n"
  )
  
  # Guardar el contenido en un archivo .R
  file_name <- paste0("full_sensitivity_analysis/RunPALMSIMsims_Manuelita/simulacion_subconjunto_", subconjunto, ".R")
  writeLines(script, con = file_name)
}
