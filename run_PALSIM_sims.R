library(data.table)
library(tidyverse)
library(job)
library(readxl)

##### Ejecución de varias simulaciones PALMSIM
source('run_PALSIM_function.r')

path_root <- paste0(dirname(here()), '/2024 CEPC Escenarios PalmSim')

climate_file <- paste0(path_root, '/ClimateData_CEPC_Escenario3.csv')

sims_data <- read_xlsx(paste0(path_root, '/Palmas_actuales_CEPC.xlsx'), range = 'A1:I38') %>%
  group_by(`Año siembra`) %>%
  summarise(Siembra = ymd(paste0(unique(`Año siembra`), '0101'))) %>%
  mutate(FechaFinal = ymd(20241231),
         Latitud = 4.36163,
         profundidad = 0.75)

for(i in 1:nrow(sims_data)){
  job({
    sim <- palsim(clima_path = climate_file,
                  start_date = sims_data$Siembra[i],
                  end_date = sims_data$FechaFinal[i],
                  latitude = sims_data$Latitud[i],
                  soil_depth = sims_data$profundidad[i],
                  soil_texture = 'clay loam')
    
    sim_name <- sims_data$`Año siembra`[i]
    
    fwrite(sim, paste0(path_root, '/simulaciones_escenario_3/', today(), '_Siembra_', sim_name, '.csv'))
    
  })
}



