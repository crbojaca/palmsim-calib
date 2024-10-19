library(data.table)
library(Metrics)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Importar los datos #####
sim_init <- fread('variedades_CEPV/PALMSIM_output_modelo_original.csv') %>%
  mutate(Parámetro = 'Original')
sim_alto_rtspmt <- fread('variedades_CEPV/PALMSIM_output_rootspecmaint0.00264.csv')%>%
  mutate(Parámetro = 'Root specific maintenance = 0.00264')
sim_bajo_rtspmt <- fread('variedades_CEPV/PALMSIM_output_rootspecmaint0.00176.csv') %>%
  mutate(Parámetro = 'Root specific maintenance = 0.00176')
sim_alto_mscfibpot <- fread('variedades_CEPV/PALMSIM_output_mesocarpfibpotmassfract0.28.csv')%>%
  mutate(Parámetro = 'Mesocarp fiber potential mass fraction = 0.28')
sim_bajo_mscfibpot <- fread('variedades_CEPV/PALMSIM_output_mesocarpfibpotmassfract0.42.csv') %>%
  mutate(Parámetro = 'Mesocarp fiber potential mass fraction = 0.42')
sim_alto_trkcnveff <- fread('variedades_CEPV/PALMSIM_output_trunkconveff0.828.csv')%>%
  mutate(Parámetro = 'Trunk conversion efficiency = 0.828')
sim_bajo_trkcnveff <- fread('variedades_CEPV/PALMSIM_output_trunkconveff0.552.csv') %>%
  mutate(Parámetro = 'Trunk conversion efficiency = 0.552')

sims <- rbind(sim_init, sim_alto_rtspmt, sim_bajo_rtspmt,
              sim_bajo_mscfibpot, sim_alto_mscfibpot,
              sim_bajo_trkcnveff, sim_alto_trkcnveff)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Grafico datos #####
sims_year <- sims %>%
  group_by(Año = year(date), Parámetro) %>%
  summarise(Rendimiento = sum(rendimiento_t_ha))

ggplot() +
  geom_point(data = sims_year, 
             aes(x = Año, y = Rendimiento, color = Parámetro)) +
  geom_line(data = sims_year, 
            aes(x = Año, y = Rendimiento, color = Parámetro)) +
  labs(title = 'Variación de parámetros',
       x = 'Año',
       y = 'Rendimiento (t/ha)') +
  theme_bw(base_size = 14,
           base_family = 'Roboto Condensed') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

ggsave('variedades_CEPV/1.CompareSelParamsSimulations.png', width = 10, height = 6, dpi = 300)

