palmsim <- function(clima_path, start_date, end_date, density = 143, dt = 1, latitude, soil_depth, soil_texture){
  
  require(reticulate)
  
  py_run_string("import pandas as pd")
  
  py_run_string(paste("weather = pd.read_csv('", clima_path, "', index_col = 'Date')", sep = ""))
  
  py_run_string("weather.index = pd.to_datetime(weather.index)")
  
  py_run_string("from palmsim import PalmField")
  
  py_run_string(paste0("pf = PalmField(year_of_planting = ", as.integer(year(start_date)), 
                       ", month_of_planting = ", as.integer(lubridate::month(start_date)), 
                       ", day_of_planting = ", as.integer(day(start_date)),
                       ", planting_density = ", as.integer(density),
                       ", dt = ", as.integer(dt), 
                       ", latitude = ", latitude,
                       ", soil_depth = ", soil_depth,
                       ", soil_texture_class = '", soil_texture, "')"))
  
  # Couple the weather data
  py_run_string("pf.weather.radiation_series = weather['solar (MJ/m2/day)']")
  
  py_run_string("pf.weather.rainfall_series = weather['precip (mm/day)']")
  
  py_run_string("pf.weather.temperature_series = weather['temperature (degC)']")
  
  py_run_string("pf.weather.humidity_series = weather['humidity (%)']")
  
  # Run the simulation
  py_run_string(paste0("df = pf.run(duration = ",
                       as.integer(as.numeric(difftime(end_date, start_date, units = 'days'))), ")"))
  
  df <- py$df
  
  df$date <- ymd(df$date)
  
  df$YAP <- df$YAP + (yday(df$date) / 365)
  
  df$rendimiento_t_ha <- df$`generative_bunch_count_daily (1/ha/day)` * df$`generative_bunch_weight (kg)` / 1000
  
  return(df)
  
}

sim_run <- palmsim(clima_path = "variedades_CEPV/ClimateData_Variedades_CEPV.csv", 
                   start_date = ymd(20031001), 
                   end_date = ymd(20191231), 
                   density = 143, 
                   dt = 1, 
                   latitude = 6.98492, 
                   soil_depth = 1.0, 
                   soil_texture = "clay loam")

library(data.table)
# fwrite(sim_run, "variedades_CEPV/PALMSIM_output_modelo_original.csv")

fwrite(sim_run, "variedades_CEPV/PALMSIM_output_rootspecmaint0.00264.csv")




