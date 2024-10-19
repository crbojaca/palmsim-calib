library(data.table)
library(tidyverse)
library(job)
library(reticulate)

##### Ejecución de las simulaciones PALMSIM con base en la grilla de parámetros
locations <- fread('calibration_lhoat/InfoMejoresLotesSeleccionadosParaCalibracion.csv') %>%
  mutate(Siembra = dmy(Siembra),
         FechaFinal = mdy(FechaFinal))

climate_files <- list.files('calibration_lhoat/climate/', full.names = FALSE)

py_run_string("import pandas as pd")

py_run_string("from palmsim import PalmField")

palmsim_function <- function(sel_location, clima_path){
  
  require(reticulate)
  
  py_run_string(paste("weather = pd.read_csv('", clima_path, "', index_col = 'Date')", sep = ""))
  
  py_run_string("weather.index = pd.to_datetime(weather.index)")
  
  py_run_string(paste0("pf = PalmField(year_of_planting = ", year(sel_location$Siembra), 
                       ", month_of_planting = ", data.table::month(sel_location$Siembra), 
                       ", day_of_planting = ", day(sel_location$Siembra),
                       ", planting_density = ", 143,
                       ", dt = ", 1, 
                       ", latitude = ", sel_location$Latitud,
                       ", soil_depth = ", sel_location$profundidad,
                       ", soil_texture_class = '", sel_location$textura, "')"))
  
  # Couple the weather data
  py_run_string("pf.weather.radiation_series = weather['solar (MJ/m2/day)']")
  
  py_run_string("pf.weather.rainfall_series = weather['precip (mm/day)']")
  
  py_run_string("pf.weather.temperature_series = weather['temperature (degC)']")
  
  py_run_string("pf.weather.humidity_series = weather['humidity (%)']")
  
  print('Simulación iniciada')
  
  flush.console()
  
  py_run_string(paste0("df = pf.run(duration = ",
                       as.integer(as.numeric(difftime(sel_location$FechaFinal, sel_location$Siembra, units = 'days'))), ")"))
  
  df <- py$df 
  
  return(df)
  
  print('Simulación finalizada')
  
  flush.console()
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################### Run the simulations ######################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for(i in 1:nrow(locations)){
  
  sel_location <- locations[i]
  
  sim_name <- paste('sim', sel_location$Empresa, sel_location$Lote, sep = '_')
  
  clima_path <- paste0(here('calibration_lhoat/climate'), '/', sel_location$clima)
  
  save_path <- paste0(here('calibration_lhoat/initial_sims'), '/', sim_name, '.csv')
  
  output <- palmsim_function(sel_location = sel_location, clima_path = clima_path)
  
  write_csv(output, save_path)
  
  print(paste('Simulación', sim_name, 'finalizada'))
  
  flush.console()
  
}
