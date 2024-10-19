library(data.table)
library(tidyverse)
library(job)
library(reticulate)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############ Función auxiliar para convertir a número valores en lista #########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to convert 'value' fields to numeric
convert_values_to_numeric <- function(x) {
  if (is.list(x)) {
    # Apply recursively to each element if it is a list
    return(lapply(x, convert_values_to_numeric))
  } else if (is.numeric(x)) {
    # Convert to numeric if it's a numeric vector
    return(as.numeric(x))
  } else {
    return(x)
  }
}

# Función para asignar valores nuevos a los parámetros de cada componente
assign_values <- function(component, new_values) {
  # Get the parameters for the component
  component_params <- py$parameters[[component]]
  
  if(component %in% c('trunk', 'roots')){
    update_params <- discard(py$parameters[[component]], str_detect(names(py$parameters[[component]]), "potential_growth_rates"))
    
    new_params <- map2(update_params, new_values, ~ {
      list_modify(.x, value = .y)
    })
    
    keep_params <- keep(py$parameters[[component]], str_detect(names(py$parameters[[component]]), "potential_growth_rates"))
    
    py$parameters[[component]] <- c(new_params, keep_params)
    
  } else {
    
    update_params <- py$parameters[[component]]
    
    new_params <- map2(update_params, new_values, ~ {
      list_modify(.x, value = .y)
    })
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################### Run the simulations ######################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Ejecución de las simulaciones PALMSIM con base en la grilla de parámetros
locations <- fread('20240808_InfoMejoresLotesSeleccionadosParaCalibracion.csv') %>%
  mutate(Siembra = dmy(Siembra),
         FechaFinal = Siembra + years(10))

parameters_grid <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))

py_run_string("import pandas as pd")
py_run_string("from palmsim import PalmField")
py_run_string("from palmsim import get_parameters, set_parameters")
py_run_string("parameters = get_parameters()")

palmsim_function <- function(X, location, sim_name, clima_path, save_path){
  require(reticulate)
  
  py_run_string(paste("weather = pd.read_csv('", clima_path, "', index_col = 'Date')", sep = ""))
  py_run_string("weather.index = pd.to_datetime(weather.index)")
  
  sel_location <- locations[location, ]
  
  sel_param <- parameters_grid %>%
    select(component, parameter) %>%
    cbind(value = X)
  
  assign_values('fronds', sel_param$value[sel_param$component == 'fronds'])
  assign_values('trunk', sel_param$value[sel_param$component == 'trunk'])
  assign_values('roots', sel_param$value[sel_param$component == 'roots'])
  assign_values('assimilates', sel_param$value[sel_param$component == 'assimilates'])
  assign_values('soil', sel_param$value[sel_param$component == 'soil'])
  assign_values('indeterminate', sel_param$value[sel_param$component == 'indeterminate'])
  assign_values('male', sel_param$value[sel_param$component == 'male'])
  assign_values('female', sel_param$value[sel_param$component == 'female'])
  assign_values('stalk', sel_param$value[sel_param$component == 'stalk'])
  assign_values('mesocarp_fibers', sel_param$value[sel_param$component == 'mesocarp_fibers'])
  assign_values('mesocarp_oil', sel_param$value[sel_param$component == 'mesocarp_oil'])
  
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
  
  print('Simulación finalizada')
  flush.console()
  
  fwrite(df, paste0(save_path, sim_name, '.csv'))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################## Output variable: FFB max  #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
palmsim_function_ffb <- function(X){
  require(reticulate)
  require(data.table)
  
  locations <- fread('20240808_InfoMejoresLotesSeleccionadosParaCalibracion.csv') %>%
    mutate(Siembra = dmy(Siembra),
           FechaFinal = Siembra + years(10))
  
  parameters_grid <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))
  
  py_run_string("import pandas as pd")
  py_run_string("from palmsim import PalmField")
  py_run_string("from palmsim import get_parameters, set_parameters")
  py_run_string("parameters = get_parameters()")
  
  clima_path <- here('full_sensitivity_analysis/ClimateData_Manuelita_SAMARIA_322_2009-01-01_2019-01-01_RadiacionNASA.csv')
  
  py_run_string(paste("weather = pd.read_csv('", clima_path, "', index_col = 'Date')", sep = ""))
  py_run_string("weather.index = pd.to_datetime(weather.index)")
  
  sel_location <- locations[5, ]
  
  sel_param <- parameters_grid %>%
    select(component, parameter) %>%
    cbind(value = X)
  
  assign_values('fronds', sel_param$value[sel_param$component == 'fronds'])
  assign_values('trunk', sel_param$value[sel_param$component == 'trunk'])
  assign_values('roots', sel_param$value[sel_param$component == 'roots'])
  assign_values('assimilates', sel_param$value[sel_param$component == 'assimilates'])
  assign_values('soil', sel_param$value[sel_param$component == 'soil'])
  assign_values('indeterminate', sel_param$value[sel_param$component == 'indeterminate'])
  assign_values('male', sel_param$value[sel_param$component == 'male'])
  assign_values('female', sel_param$value[sel_param$component == 'female'])
  assign_values('stalk', sel_param$value[sel_param$component == 'stalk'])
  assign_values('mesocarp_fibers', sel_param$value[sel_param$component == 'mesocarp_fibers'])
  assign_values('mesocarp_oil', sel_param$value[sel_param$component == 'mesocarp_oil'])
  
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
  
  ffb <- tail(df$`FFB_production (kg/ha/yr)`, n = 1)
  
  return(ffb)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################## Output variable: FFB sum  #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
palmsim_function_ffb_sum <- function(X){
  require(reticulate)
  require(data.table)
  
  locations <- fread('20240808_InfoMejoresLotesSeleccionadosParaCalibracion.csv') %>%
    mutate(Siembra = dmy(Siembra),
           FechaFinal = Siembra + years(10))
  
  parameters_grid <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))
  
  py_run_string("import pandas as pd")
  py_run_string("from palmsim import PalmField")
  py_run_string("from palmsim import get_parameters, set_parameters")
  py_run_string("parameters = get_parameters()")
  
  clima_path <- here('full_sensitivity_analysis/ClimateData_Manuelita_SAMARIA_322_2009-01-01_2019-01-01_RadiacionNASA.csv')
  
  py_run_string(paste("weather = pd.read_csv('", clima_path, "', index_col = 'Date')", sep = ""))
  py_run_string("weather.index = pd.to_datetime(weather.index)")
  
  sel_location <- locations[5, ]
  
  sel_param <- parameters_grid %>%
    select(component, parameter) %>%
    cbind(value = X)
  
  assign_values('fronds', sel_param$value[sel_param$component == 'fronds'])
  assign_values('trunk', sel_param$value[sel_param$component == 'trunk'])
  assign_values('roots', sel_param$value[sel_param$component == 'roots'])
  assign_values('assimilates', sel_param$value[sel_param$component == 'assimilates'])
  assign_values('soil', sel_param$value[sel_param$component == 'soil'])
  assign_values('indeterminate', sel_param$value[sel_param$component == 'indeterminate'])
  assign_values('male', sel_param$value[sel_param$component == 'male'])
  assign_values('female', sel_param$value[sel_param$component == 'female'])
  assign_values('stalk', sel_param$value[sel_param$component == 'stalk'])
  assign_values('mesocarp_fibers', sel_param$value[sel_param$component == 'mesocarp_fibers'])
  assign_values('mesocarp_oil', sel_param$value[sel_param$component == 'mesocarp_oil'])
  
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
  
  ffb <- sum(df$`FFB_production (kg/ha/yr)`)
  
  return(ffb)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################### Output variable: Total mass ##############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

palmsim_function_mt <- function(X){
  require(reticulate)
  require(data.table)
  
  locations <- fread('20240808_InfoMejoresLotesSeleccionadosParaCalibracion.csv') %>%
    mutate(Siembra = dmy(Siembra),
           FechaFinal = Siembra + years(10))
  
  parameters_grid <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))
  
  py_run_string("import pandas as pd")
  py_run_string("from palmsim import PalmField")
  py_run_string("from palmsim import get_parameters, set_parameters")
  py_run_string("parameters = get_parameters()")
  
  clima_path <- here('full_sensitivity_analysis/ClimateData_Manuelita_SAMARIA_322_2009-01-01_2019-01-01_RadiacionNASA.csv')
  
  py_run_string(paste("weather = pd.read_csv('", clima_path, "', index_col = 'Date')", sep = ""))
  py_run_string("weather.index = pd.to_datetime(weather.index)")
  
  sel_location <- locations[5, ]
  
  sel_param <- parameters_grid %>%
    select(component, parameter) %>%
    cbind(value = X)
  
  assign_values('fronds', sel_param$value[sel_param$component == 'fronds'])
  assign_values('trunk', sel_param$value[sel_param$component == 'trunk'])
  assign_values('roots', sel_param$value[sel_param$component == 'roots'])
  assign_values('assimilates', sel_param$value[sel_param$component == 'assimilates'])
  assign_values('soil', sel_param$value[sel_param$component == 'soil'])
  assign_values('indeterminate', sel_param$value[sel_param$component == 'indeterminate'])
  assign_values('male', sel_param$value[sel_param$component == 'male'])
  assign_values('female', sel_param$value[sel_param$component == 'female'])
  assign_values('stalk', sel_param$value[sel_param$component == 'stalk'])
  assign_values('mesocarp_fibers', sel_param$value[sel_param$component == 'mesocarp_fibers'])
  assign_values('mesocarp_oil', sel_param$value[sel_param$component == 'mesocarp_oil'])
  
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
  
  mass_total <- tail(df$mass_total, n = 1)
  
  return(mass_total)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################### Output variable: Vegetative mass ###########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

palmsim_function_mv <- function(X){
  require(reticulate)
  require(data.table)
  
  locations <- fread('20240808_InfoMejoresLotesSeleccionadosParaCalibracion.csv') %>%
    mutate(Siembra = dmy(Siembra),
           FechaFinal = Siembra + years(10))
  
  parameters_grid <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))
  
  py_run_string("import pandas as pd")
  py_run_string("from palmsim import PalmField")
  py_run_string("from palmsim import get_parameters, set_parameters")
  py_run_string("parameters = get_parameters()")
  
  clima_path <- here('full_sensitivity_analysis/ClimateData_Manuelita_SAMARIA_322_2009-01-01_2019-01-01_RadiacionNASA.csv')
  
  py_run_string(paste("weather = pd.read_csv('", clima_path, "', index_col = 'Date')", sep = ""))
  py_run_string("weather.index = pd.to_datetime(weather.index)")
  
  sel_location <- locations[5, ]
  
  sel_param <- parameters_grid %>%
    select(component, parameter) %>%
    cbind(value = X)
  
  assign_values('fronds', sel_param$value[sel_param$component == 'fronds'])
  assign_values('trunk', sel_param$value[sel_param$component == 'trunk'])
  assign_values('roots', sel_param$value[sel_param$component == 'roots'])
  assign_values('assimilates', sel_param$value[sel_param$component == 'assimilates'])
  assign_values('soil', sel_param$value[sel_param$component == 'soil'])
  assign_values('indeterminate', sel_param$value[sel_param$component == 'indeterminate'])
  assign_values('male', sel_param$value[sel_param$component == 'male'])
  assign_values('female', sel_param$value[sel_param$component == 'female'])
  assign_values('stalk', sel_param$value[sel_param$component == 'stalk'])
  assign_values('mesocarp_fibers', sel_param$value[sel_param$component == 'mesocarp_fibers'])
  assign_values('mesocarp_oil', sel_param$value[sel_param$component == 'mesocarp_oil'])
  
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
  
  mass_vegetative <- tail(df$mass_vegetative, n = 1)
  
  return(mass_vegetative)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################### Output variable: Generative mass ###########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
palmsim_function_mg <- function(X){
  require(reticulate)
  require(data.table)
  
  locations <- fread('20240808_InfoMejoresLotesSeleccionadosParaCalibracion.csv') %>%
    mutate(Siembra = dmy(Siembra),
           FechaFinal = Siembra + years(10))
  
  parameters_grid <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))
  
  py_run_string("import pandas as pd")
  py_run_string("from palmsim import PalmField")
  py_run_string("from palmsim import get_parameters, set_parameters")
  py_run_string("parameters = get_parameters()")
  
  clima_path <- here('full_sensitivity_analysis/ClimateData_Manuelita_SAMARIA_322_2009-01-01_2019-01-01_RadiacionNASA.csv')
  
  py_run_string(paste("weather = pd.read_csv('", clima_path, "', index_col = 'Date')", sep = ""))
  py_run_string("weather.index = pd.to_datetime(weather.index)")
  
  sel_location <- locations[5, ]
  
  sel_param <- parameters_grid %>%
    select(component, parameter) %>%
    cbind(value = X)
  
  assign_values('fronds', sel_param$value[sel_param$component == 'fronds'])
  assign_values('trunk', sel_param$value[sel_param$component == 'trunk'])
  assign_values('roots', sel_param$value[sel_param$component == 'roots'])
  assign_values('assimilates', sel_param$value[sel_param$component == 'assimilates'])
  assign_values('soil', sel_param$value[sel_param$component == 'soil'])
  assign_values('indeterminate', sel_param$value[sel_param$component == 'indeterminate'])
  assign_values('male', sel_param$value[sel_param$component == 'male'])
  assign_values('female', sel_param$value[sel_param$component == 'female'])
  assign_values('stalk', sel_param$value[sel_param$component == 'stalk'])
  assign_values('mesocarp_fibers', sel_param$value[sel_param$component == 'mesocarp_fibers'])
  assign_values('mesocarp_oil', sel_param$value[sel_param$component == 'mesocarp_oil'])
  
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
  
  mass_generative <- tail(df$mass_generative, n = 1)
  
  return(mass_generative)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################# Output variable: Leaf area per palm ##########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
palmsim_function_la <- function(X){
  require(reticulate)
  require(data.table)
  
  locations <- fread('20240808_InfoMejoresLotesSeleccionadosParaCalibracion.csv') %>%
    mutate(Siembra = dmy(Siembra),
           FechaFinal = Siembra + years(10))
  
  parameters_grid <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))
  
  py_run_string("import pandas as pd")
  py_run_string("from palmsim import PalmField")
  py_run_string("from palmsim import get_parameters, set_parameters")
  py_run_string("parameters = get_parameters()")
  
  clima_path <- here('full_sensitivity_analysis/ClimateData_Manuelita_SAMARIA_322_2009-01-01_2019-01-01_RadiacionNASA.csv')
  
  py_run_string(paste("weather = pd.read_csv('", clima_path, "', index_col = 'Date')", sep = ""))
  py_run_string("weather.index = pd.to_datetime(weather.index)")
  
  sel_location <- locations[5, ]
  
  sel_param <- parameters_grid %>%
    select(component, parameter) %>%
    cbind(value = X)
  
  assign_values('fronds', sel_param$value[sel_param$component == 'fronds'])
  assign_values('trunk', sel_param$value[sel_param$component == 'trunk'])
  assign_values('roots', sel_param$value[sel_param$component == 'roots'])
  assign_values('assimilates', sel_param$value[sel_param$component == 'assimilates'])
  assign_values('soil', sel_param$value[sel_param$component == 'soil'])
  assign_values('indeterminate', sel_param$value[sel_param$component == 'indeterminate'])
  assign_values('male', sel_param$value[sel_param$component == 'male'])
  assign_values('female', sel_param$value[sel_param$component == 'female'])
  assign_values('stalk', sel_param$value[sel_param$component == 'stalk'])
  assign_values('mesocarp_fibers', sel_param$value[sel_param$component == 'mesocarp_fibers'])
  assign_values('mesocarp_oil', sel_param$value[sel_param$component == 'mesocarp_oil'])
  
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
  
  mass_generative <- tail(df$`fronds_leaf_area_per_palm (m2)`, n = 1)
  
  return(mass_generative)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################# Output variable: Leaf area per palm ##########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
palmsim_function_nb <- function(X){
  require(reticulate)
  require(data.table)
  
  locations <- fread('20240808_InfoMejoresLotesSeleccionadosParaCalibracion.csv') %>%
    mutate(Siembra = dmy(Siembra),
           FechaFinal = Siembra + years(10))
  
  parameters_grid <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))
  
  py_run_string("import pandas as pd")
  py_run_string("from palmsim import PalmField")
  py_run_string("from palmsim import get_parameters, set_parameters")
  py_run_string("parameters = get_parameters()")
  
  clima_path <- here('full_sensitivity_analysis/ClimateData_Manuelita_SAMARIA_322_2009-01-01_2019-01-01_RadiacionNASA.csv')
  
  py_run_string(paste("weather = pd.read_csv('", clima_path, "', index_col = 'Date')", sep = ""))
  py_run_string("weather.index = pd.to_datetime(weather.index)")
  
  sel_location <- locations[5, ]
  
  sel_param <- parameters_grid %>%
    select(component, parameter) %>%
    cbind(value = X)
  
  assign_values('fronds', sel_param$value[sel_param$component == 'fronds'])
  assign_values('trunk', sel_param$value[sel_param$component == 'trunk'])
  assign_values('roots', sel_param$value[sel_param$component == 'roots'])
  assign_values('assimilates', sel_param$value[sel_param$component == 'assimilates'])
  assign_values('soil', sel_param$value[sel_param$component == 'soil'])
  assign_values('indeterminate', sel_param$value[sel_param$component == 'indeterminate'])
  assign_values('male', sel_param$value[sel_param$component == 'male'])
  assign_values('female', sel_param$value[sel_param$component == 'female'])
  assign_values('stalk', sel_param$value[sel_param$component == 'stalk'])
  assign_values('mesocarp_fibers', sel_param$value[sel_param$component == 'mesocarp_fibers'])
  assign_values('mesocarp_oil', sel_param$value[sel_param$component == 'mesocarp_oil'])
  
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
  
  number_bunches <- sum(df$`generative_bunch_count_daily (1/ha/day)`)
  
  return(number_bunches)
  
}



