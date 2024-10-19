# Lista de archivos a ejecutar
manuelita <- list.files(here('full_sensitivity_analysis/RunPALMSIMsims_Manuelita'), 
                        pattern = "simulacion_subconjunto_.*\\.R$",
                        full.names = TRUE)
pdc <- list.files(here('full_sensitivity_analysis/RunPALMSIMsims_PdC'), 
                  pattern = "simulacion_subconjunto_.*\\.R$",
                  full.names = TRUE)
sicarare <- list.files(here('full_sensitivity_analysis/RunPALMSIMsims_Sicarare'), 
                       pattern = "simulacion_subconjunto_.*\\.R$", 
                       full.names = TRUE)

# Enviar cada archivo a un trabajo en background
for (archivo in manuelita) {
  rstudioapi::jobRunScript(archivo, name = archivo, workingDir = here())
}

for (archivo in pdc) {
  rstudioapi::jobRunScript(archivo, name = archivo, workingDir = here())
}

for (archivo in sicarare) {
  rstudioapi::jobRunScript(archivo, name = archivo, workingDir = here())
}
