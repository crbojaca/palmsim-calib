library(tidyverse)
library(here)
library(hydroPSO)

source('full_sensitivity_analysis/1.RunPALMSIMsims_function.R')

param_palmsim <- read_csv(here('full_sensitivity_analysis/full_parameters_list.csv'))

nparam <- nrow(param_palmsim)

# Parameters ranges
lower <- param_palmsim$value * 0.90
upper <- param_palmsim$value * 1.10

names(lower) <- paste0(param_palmsim$component, '_', param_palmsim$parameter)

# Runnig the LH-OAT sensitivity analysis
set.seed(123)

lhoat(fn = palmsim_function_la,
      lower = lower,
      upper = upper,
      control = list(N = 100, 
                     f = 0.005, 
                     write2disk = TRUE, 
                     digits = 3,
                     verbose = TRUE,
                     parallel = 'parallelWin',
                     par.nnodes = 8,
                     do.plots = TRUE,
                     drty.out = here('full_sensitivity_analysis/LH_OAT_LA_2/'))
)

