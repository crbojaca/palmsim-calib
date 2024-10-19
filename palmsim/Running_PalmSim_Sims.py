# Code to run PalmSim simulations
import os
import pandas as pd
import sys

# Set the working directory
os.chdir('C:/Users/cbojaca/OneDrive - FEDEPALMA/palmsim-master/')
os.getcwd() # Get the current working directory

# Read-in the weather csv-files
weather = pd.read_csv('palmsim/carolina_1_2001C.csv')

# Add path to the model
sys.path.append('../palmsim')
sys.path.append('..')
from palmsim import PalmField

# Run the simulation
pf = PalmField(year_of_planting = 2001, month_of_planting = 6, dt = 10, latitude = 4)

pf.weather.radiation_series = weather['ALLSKY_SFC_SW_DWN']
pf.weather.rainfall_series = weather['PRECTOT']
pf.weather.temperature_series = weather['T2M']
pf.weather.humidity_series = weather['RH2M']

df = pf.run(duration = 15 * 365)

# Save the simulation
df.to_csv('palmsim/carolina_1_2001C_test.csv')

# Check submodels output
pf.components
pf.fronds
pf.weather
pf.instance_variables
pf.to_dict


