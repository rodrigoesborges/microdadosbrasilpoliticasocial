#mapa das microrregiões do Espírito Santo

#Pacotes necessários
require(tidyverse)
#devtools::install_github("rpradosiqueira/brazilmaps")
require(dplyr)
require(brazilmaps)
require(plotly)



#Mapa municípios ES
mapa_mr_es <- get_brmap("MicroRegion", geo.filter = list(State = 32)) 

mapa_me_es <- get_brmap("MesoRegion", geo.filter = list(State = 32))

mapa <- mapa_mr_es %>%
  ggplot() +
  geom_sf(aes(fill = nome),
          #ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  #muda escala de cores
  #scale_fill_viridis_d(option = 2, begin = 1, end = 0) +
  theme(panel.grid = 
          element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
mapa

mapa_me <- mapa_me_es %>%
  ggplot() +
  geom_sf(aes(fill = nome),
          #ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  #muda escala de cores
  #scale_fill_viridis_d(option = 2, begin = 1, end = 0) +
  theme(panel.grid = 
          element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
mapa_me
