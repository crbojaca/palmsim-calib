##### Consulta de datos de producción para los lotes seleccionados #####

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### 1. Import observed data #####
locations <- fread('calibration_lhoat/InfoMejoresLotesSeleccionadosParaCalibracion.csv') %>%
  mutate(Siembra = dmy(Siembra),
         FechaFinal = dmy(FechaFinal))

obs_data <- fread('C:/Users/cbojaca/OneDrive - FEDEPALMA/Protocolos/2024/PRT Calibración Modelo Palma/1.Ejecución/Data/Guineensis/field_data/PlantationsRawDataMonthlyBasisFinal.csv')

obs_locations <- obs_data %>%
  right_join(locations %>% 
               select(Empresa, Finca, Lote)) %>%
  mutate(Siembra = mdy(Siembra),
         Fecha = mdy(Fecha)) %>%
  select(-c(Zona, Densidad, Area, Ciclo, RFF_ton, RFF_ton_palma))

write_csv(obs_locations, 'calibration_lhoat/obs_data/obs_yield.csv')
