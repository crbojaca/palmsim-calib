library(arrow)
library(data.table)
library(sf)
library(httr)
library(soiltexture)

# Load the data
palmet_coordenadas <- fread('//192.168.103.75/2024_PALMET/PALMET_shiny/PRODUCCION_coordenadas_seleccionadas_por_zona_NASA.csv')

palmet <- open_dataset('//192.168.103.75/2024_PALMET/BD_PALMET_municipios_palmeros_complete')

siembra <- ymd(20031001)

final <- ymd(20191231)

#### Collect climate data
long <- -73.70600

lat <- 6.98492

seq_dates <- seq.Date(siembra, final, by = 'day')

# Find the nearest point in the dataset
punto_cercano <- st_nearest_feature(
  st_point(c(long, lat)),
  st_as_sf(palmet_coordenadas %>%
             distinct(x, y) %>%
             collect(), coords = c('x', 'y')
  )
)

coords_sel <- palmet_coordenadas[punto_cercano, ]

plot_name <- 'Variedades_CEPV'

# Query the data
out_data <- palmet %>%
  filter(Longitud == coords_sel$x, Latitud == coords_sel$y, Fecha %in% seq_dates) %>%
  dplyr::select(Fecha, Temperatura, HumedadRelativa, Radiacion, PrecipitacionCHIRPS_corregida) %>%
  collect() %>%
  arrange(Fecha) %>%
  mutate(Date = Fecha,
         `temperature (degC)` = round(Temperatura, 2),
         `humidity (%)` = round(HumedadRelativa, 2),
         `solar (MJ/m2/day)` = round(Radiacion, 2),
         `precip (mm/day)` = round(PrecipitacionCHIRPS_corregida, 2)) %>%
  dplyr::select(Date, `temperature (degC)`, `humidity (%)`, `solar (MJ/m2/day)`, `precip (mm/day)`)

# Check if there is NA data
if(sum(is.na(out_data)) > 0){
  print(paste0('Data for plot ', plot_name, ' has missing values'))
  flush.console()
}

# Save the data
write_csv(out_data, paste0('variedades_CEPV/ClimateData_', plot_name, '.csv'))

