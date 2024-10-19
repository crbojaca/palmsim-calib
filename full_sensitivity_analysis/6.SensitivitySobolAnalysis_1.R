# Análisis de sensibilidad utilizando el paquete sensobol
library(sensobol)
library(job)

# Cargar el modelo
source('full_sensitivity_analysis/1.RunPALMSIMsims_function.R')

# Generar muestras para el análisis de sensibilidad
param_palmsim <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))

##### Set the sample size
N <- 100

##### Create a vector with parameters name
params <- paste(param_palmsim$component, param_palmsim$parameter, sep = '_')

##### Use Azzini et al. (2020b) estimators to generate the samples
matrices <- c('A', 'B', 'AB')

##### Compute up to second-order effects
first <- 'saltelli'

total <- 'jansen'

order <- 'first'

R <- 10^3

type <- 'percent'

conf <- 0.95

##### Construct the sample matrix
mat <- sobol_matrices(matrices = matrices,
                      N = N,
                      params = params,
                      order = order,
                      type = 'LHS')

for(i in 1:length(params)){
  mat[, params[i]] <- qunif(mat[, params[i]], min = param_palmsim$value[i] * 0.95, max = param_palmsim$value[i] * 1.05)
}

saveRDS(mat, here('full_sensitivity_analysis/sobol_sample_matrix.rds'))



