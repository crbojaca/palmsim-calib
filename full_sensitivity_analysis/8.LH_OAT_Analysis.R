##### Análisis de la salida de los resultados LH-OAT #####
library(data.table)

##### Carga de resultados
ffb_out <- fread(here('full_sensitivity_analysis/LH_OAT_FFB/LH_OAT-out.txt'))
ffb_sum_out <- fread(here('full_sensitivity_analysis/LH_OAT_FFB_sum/LH_OAT-out.txt'))
la_out <- fread(here('full_sensitivity_analysis/LH_OAT_LA/LH_OAT-out.txt'))
mg_out <- fread(here('full_sensitivity_analysis/LH_OAT_MG/LH_OAT-out.txt'))
mt_out <- fread(here('full_sensitivity_analysis/LH_OAT_MT/LH_OAT-out.txt'))
mv_out <- fread(here('full_sensitivity_analysis/LH_OAT_MV/LH_OAT-out.txt'))
nb_out <- fread(here('full_sensitivity_analysis/LH_OAT_NB/LH_OAT-out.txt'))

##### Carga de ranking
ffb_ranking <- fread(here('full_sensitivity_analysis/LH_OAT_FFB/LH_OAT-Ranking.txt')) %>%
  mutate(variable = 'FFB')
ffb_sum_ranking <- fread(here('full_sensitivity_analysis/LH_OAT_FFB_sum/LH_OAT-Ranking.txt'))  %>%
  mutate(variable = 'FFB_sum')
la_ranking <- fread(here('full_sensitivity_analysis/LH_OAT_LA/LH_OAT-Ranking.txt'))  %>%
  mutate(variable = 'LeafArea')
mg_ranking <- fread(here('full_sensitivity_analysis/LH_OAT_MG/LH_OAT-Ranking.txt'))  %>%
  mutate(variable = 'MassGenerative')
mt_ranking <- fread(here('full_sensitivity_analysis/LH_OAT_MT/LH_OAT-Ranking.txt'))  %>%
  mutate(variable = 'MassTotal')
mv_ranking <- fread(here('full_sensitivity_analysis/LH_OAT_MV/LH_OAT-Ranking.txt'))  %>%
  mutate(variable = 'MassVegetative')
nb_ranking <- fread(here('full_sensitivity_analysis/LH_OAT_NB/LH_OAT-Ranking.txt'))  %>%
  mutate(variable = 'NumberBunches')

##### Unión de los rankings
ranking <- rbind(ffb_ranking, 
                 ffb_sum_ranking, 
                 mg_ranking, 
                 mt_ranking, 
                 mv_ranking,
                 nb_ranking)

##### Tabla con los rankings de cada variable
ranking %>%
  select(variable, ParameterName, RankingNmbr) %>%
  spread(variable, RankingNmbr) %>%
  arrange(FFB)

