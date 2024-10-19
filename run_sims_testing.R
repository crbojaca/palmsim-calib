library(reticulate)

# Load the model
os <- import('os')
pd <- import('pandas')
sys <- import('sys')

# From palmsim import PalmField
palsim <- import('palmsim')
PalmField <- palsim$PalmField
file_path <- "C:/Users/cbojaca/OneDrive - FEDEPALMA/palmsim-master/examples/input/Bangladesh.csv"

# Import pandas and read the CSV file
py_run_string("import pandas as pd")
py_run_string(paste("weather = pd.read_csv('", file_path, "', index_col = 'Date')", sep = ""))
py_run_string("weather.index = pd.to_datetime(weather.index)")
py_run_string("pf = PalmField(year_of_planting = int(weather.index[0].year), 
                month_of_planting = int(weather.index[0].month),
                day_of_planting = int(weather.index[0].day),
                dt = 10, 
                latitude = 4.5,
                soil_depth = 1,
                soil_texture_class = 'clay')")
py_run_string("pf.weather.radiation_series = weather['solar (MJ/m2/day)']")

# Run the simulation
py_run_string("df = pf.run(duration = 30 * 365)")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


weather <- read.csv('examples/input/Bangladesh.csv', row.names = 1)

pf <- PalmField(year_of_planting = as.integer(1986), 
                month_of_planting = as.integer(1), 
                day_of_planting = as.integer(1),
                dt = 10, 
                latitude = 4.5,
                soil_depth = 1,
                soil_texture_class = 'clay')

# Couple the weather data
pf$weather$radiation_series <- weather[1]

df <- pf$run(3650)








