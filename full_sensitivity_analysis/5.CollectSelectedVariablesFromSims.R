library(data.table)

#### Selección de las variables de salida de las simulaciones que se van a utilizar en el análisis de sensibilidad
pdc <- list.files('D:/2024_Calibracion_PALMSIM/AnalisisSensibilidadCompleto/Simulaciones_ZC_PdC/', full.names = TRUE)
sicarare <- list.files('D:/2024_Calibracion_PALMSIM/AnalisisSensibilidadCompleto/Simulaciones_ZN_Sicarare/', full.names = TRUE)
manuelita <- list.files('D:/2024_Calibracion_PALMSIM/AnalisisSensibilidadCompleto/Simulaciones_ZO_Manuelita/', full.names = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########################### OPCIÓN 1 - Selecciono únicamente el dato de FFB del último día de simulación ######################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### Dataframe con los resultados de las simulaciones
places <- c('Simulaciones_ZC_PdC/', 'Simulaciones_ZN_Sicarare/', 'Simulaciones_ZO_Manuelita/') 

df <- data.frame()

for(i in seq_along(places)){
  
  files_sel <- list.files(paste0('D:/2024_Calibracion_PALMSIM/AnalisisSensibilidadCompleto/', places[i]), full.names = TRUE)
  
  location <- if(i == 1) 'PdC' else if(i == 2) 'Sicarare' else 'Manuelita'
  
  for (j in 1:length(files_sel)){
    
    # Extraer los dos números de cada nombre de archivo
    num <- as.numeric(unlist(str_extract_all(files_sel[j], "\\d+")))
    
    # Leer el archivo
    last_value <- fread(files_sel[j], select = 'FFB_production (kg/ha/yr)')[.N, `FFB_production (kg/ha/yr)`]
    
    # Agregar los dos números al dataframe
    data_sim <- data.frame(location = location, group = num[2], sim = num[3], FFB = last_value)
    
    # Agregar los datos al dataframe
    df <- bind_rows(df, data_sim)
    
    print(paste0('Archivo ', j, ' de ', length(files_sel), ' de ', location))
    
    flush.console()
  }
  
}

#### Sort df by location, group and sim
df <- df[order(df$location, df$group, df$sim), ]

#### Save df
fwrite(df, here('full_sensitivity_analysis/OutputSimsLastFFB.csv'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################### OPCIÓN 2 - Selecciono todos los datos para la variable FFB del último día de simulación ###############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- data.frame()

for(i in seq_along(places)){
  
  files_sel <- list.files(paste0('D:/2024_Calibracion_PALMSIM/AnalisisSensibilidadCompleto/', places[i]), full.names = TRUE)
  
  location <- if(i == 1) 'PdC' else if(i == 2) 'Sicarare' else 'Manuelita'
  
  for (j in 1:length(files_sel)){
    
    # Extraer los dos números de cada nombre de archivo
    num <- as.numeric(unlist(str_extract_all(files_sel[j], "\\d+")))
    
    # Leer el archivo
    last_value <- fread(files_sel[j], select = 'FFB_production (kg/ha/yr)')
    
    # Agregar los dos números al dataframe
    data_sim <- data.frame(location = location, group = num[2], sim = num[3], t(last_value))
    
    # Agregar los datos al dataframe
    df <- bind_rows(df, data_sim)
    
    print(paste0('Archivo ', j, ' de ', length(files_sel), ' de ', location))
    
    flush.console()
  }
  
}

#### Sort df by location, group and sim
df <- df[order(df$location, df$group, df$sim), ]

#### Save df
fwrite(df, here('D:/2024_Calibracion_PALMSIM/AnalisisSensibilidadCompleto/OutputSimsAllFFB.csv'))
