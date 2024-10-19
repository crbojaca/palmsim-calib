library(data.table)
library(tidyverse)
library(job)
library(reticulate)
library(optimx)

obs_data <- fread(here('calibration_lhoat/obs_data/obs_yield.csv'))

locations <- fread('calibration_lhoat/InfoMejoresLotesSeleccionadosParaCalibracion.csv') %>%
  mutate(Siembra = dmy(Siembra),
         FechaFinal = mdy(FechaFinal))

parametros_calib <- c('roots_specific_maintenance',
                      'roots_loss_param',
                      'roots_conversion_efficiency',
                      'trunk_conversion_efficiency',
                      'trunk_specific_maintenance')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Función para asignar valores a los parámetros  #################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

assign_values <- function(component, new_values) {
  
  component_params <- py$parameters[[component]]
  
  param_names <- names(component_params)
  
  calibration_params <- intersect(param_names, parametros_calib)
  
  for (i in seq_along(calibration_params)) {
    
    param_name <- calibration_params[i]
    
    component_params[[param_name]]$value <- new_values[i]
    
  }
  
  py$parameters[[component]] <- component_params
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################### Run the simulations ######################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

py_run_string("import pandas as pd")

py_run_string("from palmsim import PalmField")

py_run_string("from palmsim import get_parameters, set_parameters")

py_run_string("parameters = get_parameters()")

palmsim_function <- function(sel_location, clima_path, params){
  
  require(reticulate)
  
  py_run_string(paste("weather = pd.read_csv('", clima_path, "', index_col = 'Date')", sep = ""))
  
  py_run_string("weather.index = pd.to_datetime(weather.index)")
  
  assign_values('roots', params[1:3])
  
  assign_values('trunk', params[4:5])
  
  py_run_string(paste0("pf = PalmField(year_of_planting = ", year(sel_location$Siembra), 
                       ", month_of_planting = ", data.table::month(sel_location$Siembra), 
                       ", day_of_planting = ", day(sel_location$Siembra),
                       ", planting_density = ", 143,
                       ", dt = ", 1, 
                       ", latitude = ", sel_location$Latitud,
                       ", soil_depth = ", sel_location$profundidad,
                       ", soil_texture_class = '", sel_location$textura, "')"))
  
  py_run_string("set_parameters(pf, parameters)")
  
  # Couple the weather data
  py_run_string("pf.weather.radiation_series = weather['solar (MJ/m2/day)']")
  
  py_run_string("pf.weather.rainfall_series = weather['precip (mm/day)']")
  
  py_run_string("pf.weather.temperature_series = weather['temperature (degC)']")
  
  py_run_string("pf.weather.humidity_series = weather['humidity (%)']")
  
  # Run the simulation
  py_run_string(paste0("df = pf.run(duration = ",
                       as.integer(as.numeric(difftime(sel_location$FechaFinal, sel_location$Siembra, units = 'days'))), ")"))
  
  df <- py$df
  
  df_sim <- df %>%
    group_by(Fecha = floor_date(ymd(date), 'month')) %>%
    summarise(bunch_count = sum(`generative_bunch_count_daily (1/ha/day)`),
              bunch_weight = mean(`generative_bunch_weight (kg)`),
              FFB_sim = sum(bunch_count * bunch_weight / 1000))
  
  return(df_sim)
  
  print('Simulación finalizada')
  
  flush.console()
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############## Función para calcular el error cuadrático medio #################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calculate_rmse <- function(observed, simulated) {
  
  sqrt(mean((observed - simulated)^2, na.rm = TRUE))
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################### Función objetivo para la optimización ######################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
objective_function <- function(params, locations) {
  
  total_rmse <- 0
  
  for(i in 1:nrow(locations)) {
    
    sel_location <- locations[i, ]
    
    clima_path <- here('calibration_lhoat/climate', sel_location$clima)
    
    observed_data <- obs_data %>%
      dplyr::filter(Empresa == sel_location$Empresa, 
                    Finca == sel_location$Finca,
                    Lote == sel_location$Lote) %>%
      dplyr::select(Fecha, RFF_ton_ha) %>%
      arrange(Fecha)
    
    simulated_data <- palmsim_function(sel_location, clima_path, params) %>%
      dplyr::filter(Fecha %in% observed_data$Fecha)
    
    rmse <- calculate_rmse(observed_data$RFF_ton_ha, simulated_data$FFB_sim)
    
    total_rmse <- total_rmse + rmse
    
  }
  
  return(total_rmse / nrow(locations))
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################### Función principal de calibración ########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calibrate_palmsim <- function(locations, initial_params) {
  
  # Definir límites como +/- 5% de los valores iniciales
  lower <- initial_params * 0.90
  
  upper <- initial_params * 1.10
  
  # List of optimization methods to try
  methods <- c("L-BFGS-B")
  
  optimization_result <- optimx(par = initial_params,
                               fn = objective_function,
                               locations = locations,
                               method = methods,
                               lower = lower,
                               upper = upper,
                               control = list(maxit = 1000, trace = 1)
  )
  
  # Select the best result
  best_result <- summary(optimization_result, order = "value")[1, ]
  
  return(list(params = as.numeric(best_result[1:length(initial_params)]), 
              objective_value = best_result$value,
              convergence = best_result$convcode,
              method = best_result$method))
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########################### Ejecutar la calibración ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# parametros_calib <- c('roots_specific_maintenance',
#                       'roots_loss_param',
#                       'roots_conversion_efficiency',
#                       'trunk_conversion_efficiency',
#                       'trunk_specific_maintenance')

# Valores iniciales para los parámetros
initial_params <- c(2.200e-03, 5.820e-04, 6.900e-01, 6.900e-01, 5.000e-04)

calibration_result <- calibrate_palmsim(locations, initial_params)

print("Optimized parameters:")

print(calibration_result$params)

print(paste("Final objective value:", calibration_result$objective_value))

print(paste("Convergence:", calibration_result$convergence))

print(paste("Best method:", calibration_result$method))

# Print full results
print(summary(calibration_result, order = "value"))





