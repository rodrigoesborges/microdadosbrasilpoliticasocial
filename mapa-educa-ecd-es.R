#Mapas de situação da educação da primeira infância nos municípios do ES



#Pacotes necessários
library(tidyverse)
library(brazilmaps)
library(plotly)


#variaveis de ambiente e prefixadas
dadoslocais <- "~/RLocalData/ecd-indicadores"

#Primeira tentativa de mapa: dados de 
educa <- read.csv2(paste0(dadoslocais,"/2017_tab_ecd_ed.csv"), stringsAsFactors = F)
f <- municipesord$CodMunicipio
educa <- cbind(codmunicipio = c(as.numeric(levels(f))[f],32),educa)


#pegar o mapa e juntar aos dados de educação

#taxa de cobertura efetiva de creche
tx_creche <- educa[1:78, c(1,3,5)]

#Retira o final da UF do nome das cidades
tx_creche$local <- gsub("(.*) - ES","\\1",tx_creche$local)

#Como número a taxa
tx_creche$Tx.de.Cobertura.Efetiva...Creche <- as.numeric(tx_creche$Tx.de.Cobertura.Efetiva...Creche)

tx_creche$taxa <- cut(tx_creche$Tx.de.Cobertura.Efetiva...Creche,
                      c(0, 0.2, 0.4, 0.6, 0.8))


get_brmap("City", geo.filter = list(State = 32)) %>%
  left_join(tx_creche, c("nome" = "local")) %>%
  plotly() +
  geom_sf(aes(fill = tx_creche$taxa),
            #ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  #muda escala de cores
  scale_fill_viridis_d(option = 2, begin = 0.8, end = 0.2) +
  theme(panel.grid = 
        element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = "Cobertura de Creche no Espirito Santo - 2017",
       fill = "Taxa de Cobertura Efetiva de Creche")



get_brmap("City", geo.filter = list(State = 32)) %>%
  left_join(tx_creche, c("nome" = "local")) %>%
  ggplot() +
  geom_sf(aes(fill = tx_creche$Tx.de.Cobertura.Efetiva...Creche),
          #ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  #muda escala de cores
  scale_fill_viridis_c(option = 2, direction = -1) +
  theme(panel.grid = 
          element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = "Cobertura de Creche no Espirito Santo - 2017",
       fill = "Taxa de Cobertura Efetiva de Creche")


