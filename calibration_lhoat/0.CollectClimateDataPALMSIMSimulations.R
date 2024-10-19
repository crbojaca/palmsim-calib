library(arrow)
library(data.table)
library(sf)
library(httr)
library(soiltexture)

# Load the data
palmet_coordenadas <- fread('//192.168.103.75/2024_PALMET/PALMET_shiny/PRODUCCION_coordenadas_seleccionadas_por_zona_NASA.csv')
palmet <- open_dataset('//192.168.103.75/2024_PALMET/BD_PALMET_municipios_palmeros_complete')

plots_data <- fread('calibration_lhoat/InfoMejoresLotesSeleccionadosParaCalibracion.csv')
plots_data$Siembra <- dmy(plots_data$Siembra)
plots_data$FechaFinal <- mdy(plots_data$FechaFinal)

#### Collect climate data for the selected plots
for(i in 1:nrow(plots_data)){
  # Filtering criteria
  long <- plots_data$Longitud[i]
  lat <- plots_data$Latitud[i]
  seq_dates <- seq.Date(plots_data$Siembra[i], plots_data$FechaFinal[i], by = 'day')
  
  # Find the nearest point in the dataset
  punto_cercano <- st_nearest_feature(
    st_point(c(long, lat)),
    st_as_sf(palmet_coordenadas %>%
               distinct(x, y) %>%
               collect(), coords = c('x', 'y')
    )
  )
  
  coords_sel <- palmet_coordenadas[punto_cercano, ]
  
  plot_name <- paste0(plots_data$Empresa[i], '_', 
                      plots_data$Finca[i], '_', 
                      plots_data$Lote[i], '_',
                      plots_data$Siembra[i], '_',
                      plots_data$FechaFinal[i])
  
  # Query the data
  out_data <- palmet %>%
    filter(Longitud == coords_sel$x, Latitud == coords_sel$y, Fecha %in% seq_dates) %>%
    dplyr::select(Fecha, Temperatura, HumedadRelativa, RadSolarNasa_MJm2d, PrecipitacionCHIRPS_corregida) %>%
    collect() %>%
    arrange(Fecha) %>%
    mutate(Date = Fecha,
           `temperature (degC)` = round(Temperatura, 2),
           `humidity (%)` = round(HumedadRelativa, 2),
           `solar (MJ/m2/day)` = round(RadSolarNasa_MJm2d, 2),
           `precip (mm/day)` = round(PrecipitacionCHIRPS_corregida, 2)) %>%
    dplyr::select(Date, `temperature (degC)`, `humidity (%)`, `solar (MJ/m2/day)`, `precip (mm/day)`)
  
  # Check if there is NA data
  if(sum(is.na(out_data)) > 0){
    print(paste0('Data for plot ', plot_name, ' has missing values'))
    flush.console()
  }
  
  # Save the data
  write_csv(out_data, paste0('calibration_lhoat/climate/ClimateData_', plot_name, '.csv'))
  
  print(paste0('Data for plot ', plot_name, ' collected'))
  flush.console()
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Collect soil data for the selected plots
texture_query <- function(lon, lat, depth, property){
  url <- "https://rest.isric.org/soilgrids/v2.0/properties/query"
  
  query <- list(
    lon = lon,
    lat = lat,
    property = property,
    depth = depth,
    value = "mean"
  )
  
  headers <- c(
    "accept" = "application/json"
  )
  
  response <- GET(url, query = query, add_headers(.headers = headers))
  
  data <- content(response, "parsed")
  
  ### Select data for output
  out_data <- data$properties$layers[[1]]$depths[[1]]$values$mean
  
  return(out_data / 10) # Convert to percentage
}

clay <- NULL
for(i in 1:nrow(plots_data)){
  clay[i] <- texture_query(lon = plots_data$Longitud[i], 
                           lat = plots_data$Latitud[i], 
                           depth = '0-5cm', 
                           property = 'clay')
  
  print(paste0('Data for plot ', i, ' collected'))
  flush.console()
  
  Sys.sleep(5)
}

silt <- NULL
for(i in 15:nrow(plots_data)){
  silt[i] <- texture_query(lon = plots_data$Longitud[i], 
                           lat = plots_data$Latitud[i], 
                           depth = '0-5cm', 
                           property = 'silt')
  
  print(paste0('Data for plot ', i, ' collected'))
  flush.console()
  
  Sys.sleep(5)
}

sand <- NULL
for(i in 24:nrow(plots_data)){
  sand[i] <- texture_query(lon = plots_data$Longitud[i], 
                           lat = plots_data$Latitud[i], 
                           depth = '0-5cm', 
                           property = 'sand')
  
  print(paste0('Data for plot ', i, ' collected'))
  flush.console()
  
  Sys.sleep(5)
}

plots_data$clay <- clay
plots_data$silt <- silt
plots_data$sand <- sand

### Get texture class
texture_class <- TT.points.in.classes(tri.data = plots_data, 
                                      css.names = c('clay', 'silt', 'sand'),
                                      class.sys = 'USDA.TT')

plots_data$textura <- 'clay loam'

write_csv(plots_data, 'calibration/InfoMejoresLotesSeleccionadosParaCalibracion.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### Mapa con las ubicaciones seleccionadas
library(geodata)
library(ggrepel)

col <- gadm(country = "COL", level = 0, path = tempdir()) %>% st_as_sf()
lotes_sf <- st_as_sf(plots_data, coords = c('Longitud', 'Latitud'), crs = 4326)

ggplot() +
  geom_sf(data = col, fill = 'transparent') +
  geom_sf(data = lotes_sf, aes(color = Empresa)) +
  theme_minimal() +
  labs(title = 'Ubicación de los lotes seleccionados para la calibración',
       x = 'Longitud',
       y = 'Latitud')



