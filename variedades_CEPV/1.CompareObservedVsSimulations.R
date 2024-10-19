library(data.table)
library(Metrics)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Importar los datos #####
obs <- fread('variedades_CEPV/Variedades_CEPV Historial variedades_mes y año_at.csv') %>%
  mutate(Siembra = dmy(Siembra),
         Fecha = dmy(Fecha))
sim_init <- fread('variedades_CEPV/PALMSIM_output_modelo_original.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Agrupo datos observados al promedio de todos por año #####
obs_year <- obs %>%
  group_by(Año, Tratamiento, Repetición, Palma) %>%
  summarise(Rendimiento = sum(Ton_ha, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Año) %>%
  summarise(RendimientoMean = mean(Rendimiento, na.rm = TRUE),
            RendimientoSd = sd(Rendimiento, na.rm = TRUE)) %>%
  mutate(Categoría = 'Observados')

sim_year <- sim_init %>%
  group_by(Año = year(date)) %>%
  summarise(RendimientoMean = sum(rendimiento_t_ha, na.rm = TRUE)) %>%
  mutate(Categoría = 'PALMSIM', RendimientoSd = 0)

obs_sim <- rbind(obs_year, sim_year)

rmse_val <- rmse(obs_year$RendimientoMean, 
                 sim_year %>%
                   filter(Año > 2004) %>%
                   select(RendimientoMean) %>%
                   pull())

ggplot() +
  geom_point(data = obs_sim, 
             aes(x = Año, y = RendimientoMean, color = Categoría)) +
  geom_line(data = obs_sim, 
            aes(x = Año, y = RendimientoMean, color = Categoría, group = Categoría)) +
  geom_errorbar(data = obs_sim, 
                aes(Año, ymin = RendimientoMean - RendimientoSd, ymax = RendimientoMean + RendimientoSd,
                    color = Categoría), 
                width = 0.2) +
  annotate('text', x = 2016, y = 5, family = 'Roboto Condensed', size = 6, 
           label = paste('RMSE =', round(rmse_val, 2), 't/ha')) +
  scale_color_manual(values = c('PALMSIM' = 'darkblue', 'Observados' = 'darkred')) +
  labs(title = 'Comparación rendimiento promedio por año',
       subtitle = 'Observados vs PALMSIM',
       x = 'Año',
       y = 'Rendimiento (t/ha)') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

ggsave('variedades_CEPV/ComparaciónObsSimPALMSIM.png', width = 8, height = 6, dpi = 300)

