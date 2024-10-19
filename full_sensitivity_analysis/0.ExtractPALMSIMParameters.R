library(data.table)
library(tidyverse)
library(job)
library(reticulate)

py_run_string("from palmsim import PalmField")
py_run_string("from palmsim import get_parameters, set_parameters")
py_run_string("parameters = get_parameters()")

# Extraer la lista de parámetros
fronds <- py$parameters$fronds
trunk <- py$parameters$trunk
roots <- py$parameters$roots
assimilates <- py$parameters$assimilates
soil <- py$parameters$soil
indeterminate <- py$parameters$indeterminate
male <- py$parameters$male
female <- py$parameters$female
stalk <- py$parameters$stalk
mesocarp_fibers <- py$parameters$mesocarp_fibers
mesocarp_oil <- py$parameters$mesocarp_oil

# Crear una función para extraer la información de cada parámetro
extract_info <- function(param) {
  
  if(length(param$value) != 1){
    value_x <- sapply(param$value, function(v) v[[1]])
    value_y <- sapply(param$value, function(v) v[[2]])
    value <- c(value_x, value_y)
  }else{
    value <- param$value
  }
  
  data.frame(
    value = value,
    unit = ifelse(is.null(param$unit), NA, param$unit),
    uncertainty = ifelse(is.null(param$uncertainty), NA, param$uncertainty),
    info = ifelse(is.null(param$info), NA, param$info),
    source = ifelse(is.null(param$source), NA, param$source),
    stringsAsFactors = FALSE
  )
  
}

# Aplicar la función a cada parámetro y combinar los resultados en un data.frame
fronds_df <- do.call(rbind, lapply(fronds, extract_info))
trunk_df <- do.call(rbind, lapply(trunk, extract_info))
roots_df <- do.call(rbind, lapply(roots, extract_info))
assimilates_df <- do.call(rbind, lapply(assimilates, extract_info))
soil_df <- do.call(rbind, lapply(soil, extract_info))
indeterminate_df <- do.call(rbind, lapply(indeterminate, extract_info))
male_df <- do.call(rbind, lapply(male, extract_info))
female_df <- do.call(rbind, lapply(female, extract_info))
stalk_df <- do.call(rbind, lapply(stalk, extract_info))
mesocarp_fibers_df <- do.call(rbind, lapply(mesocarp_fibers, extract_info))
mesocarp_oil_df <- do.call(rbind, lapply(mesocarp_oil, extract_info))

# Añadir una columna con el nombre de cada parámetro
fronds_df$parameter <- rownames(fronds_df)
trunk_df$parameter <- rownames(trunk_df)
roots_df$parameter <- rownames(roots_df)
assimilates_df$parameter <- rownames(assimilates_df)
soil_df$parameter <- rownames(soil_df)
indeterminate_df$parameter <- rownames(indeterminate_df)
male_df$parameter <- rownames(male_df)
female_df$parameter <- rownames(female_df)
stalk_df$parameter <- rownames(stalk_df)
mesocarp_fibers_df$parameter <- rownames(mesocarp_fibers_df)
mesocarp_oil_df$parameter <- rownames(mesocarp_oil_df)

# Remuevo parámetros que son funciones
trunk_df <- trunk_df %>% filter(!grepl('potential_growth_rates', parameter))
roots_df <- roots_df %>% filter(!grepl('potential_growth_rates', parameter))

# Uno todos los data.frames
parameters_df <- rbind(
  fronds_df %>% mutate(component = 'fronds'),
  trunk_df %>% mutate(component = 'trunk'),
  roots_df %>% mutate(component = 'roots'),
  assimilates_df %>% mutate(component = 'assimilates'),
  soil_df %>% mutate(component = 'soil'),
  indeterminate_df %>% mutate(component = 'indeterminate'),
  male_df %>% mutate(component = 'male'),
  female_df %>% mutate(component = 'female'),
  stalk_df %>% mutate(component = 'stalk'),
  mesocarp_fibers_df %>% mutate(component = 'mesocarp_fibers'),
  mesocarp_oil_df %>% mutate(component = 'mesocarp_oil')
) %>%
  select(component, parameter, everything())

# Guardo los parámetros en un archivo
write_csv(parameters_df, here('full_sensitivity_analysis/full_parameters_list.csv'))
