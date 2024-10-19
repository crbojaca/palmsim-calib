# Análisis de sensibilidad utilizando el paquete sensobol - Parte 2
# Las simulaciones con PALSIM ya fueron realizadas
library(sensobol)

# Cargar las matrices de sobol
mat <- readRDS('full_sensitivity_analysis/sobol_sample_matrix.rds')

matrices <- c('A', 'B', 'AB')

# Generar muestras para el análisis de sensibilidad
param_palmsim <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))

##### Set the sample size
N <- 100

##### Create a vector with parameters name
params <- paste(param_palmsim$component, param_palmsim$parameter, sep = '_')

R <- 10^3

# Cargar los resultados de las simulaciones
output_sims <- fread('full_sensitivity_analysis/OutputSimsLastFFB.csv')

y <- output_sims$FFB

# Calcular los índices de sensibilidad
ind <- sobol_indices(matrices = matrices, 
                     Y = y, 
                     boot = TRUE, 
                     ncpus = 10,
                     params = params,
                     N = N,
                     R = R)

cols <- colnames(ind$results)[1:5]

ind$results[,(cols):=round(.SD,3),.SDcols=(cols)]

plot(ind)

plot_uncertainty(Y = y, N = N)

plot_scatter(data = mat, N = N, Y = y, params = params)

plot_scatter(data = mat, N = N, Y = y, params = params, method = 'bin')





N <- 2^10

k <- 8

params1 <- paste("$x_", 1:k, "$", sep = "")

mat1 <- sobol_matrices(N = N, params = params1)

y1 <- sobol_Fun(mat1)

plot_uncertainty(Y = y1, N = N) + labs(y = "Counts", x = "$y$")








