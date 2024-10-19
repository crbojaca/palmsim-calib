library(data.table)
library(Metrics)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Importar los datos #####
sim_init <- fread('variedades_CEPV/PALMSIM_output_modelo_original.csv') %>%
  mutate(RootSpecificMaintenance = factor(0.00264))
sim_alto <- fread('variedades_CEPV/PALMSIM_output_rootspecmaint0.00264.csv')%>%
  mutate(RootSpecificMaintenance = factor(0.0022))
sim_bajo <- fread('variedades_CEPV/PALMSIM_output_rootspecmaint0.00176.csv') %>%
  mutate(RootSpecificMaintenance = factor(0.00176))

sims <- rbind(sim_init, sim_alto, sim_bajo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Grafico datos #####
sims_year <- sims %>%
  group_by(Año = year(date), RootSpecificMaintenance) %>%
  summarise(Rendimiento = sum(rendimiento_t_ha))

ggplot() +
  geom_point(data = sims_year, 
             aes(x = Año, y = Rendimiento, color = RootSpecificMaintenance)) +
  geom_line(data = sims_year, 
            aes(x = Año, y = Rendimiento, color = RootSpecificMaintenance)) +
  labs(title = 'Roots specific maintenance',
       x = 'Año',
       y = 'Rendimiento (t/ha)') +
  theme_bw(base_size = 14,
           base_family = 'Roboto Condensed') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

ggsave('variedades_CEPV/1.CompareSpecMaintSimulations.png', width = 10, height = 6, dpi = 300)

