#Mapas de situação da educação da primeira infância nos municípios do ES



#Pacotes necessários
library(tidyverse)
devtools::install_github("rpradosiqueira/brazilmaps")
library(brazilmaps)
library(plotly)


#variaveis de ambiente e prefixadas
dadoslocais <- "~/RLocalData/ecd-indicadores"

#Anos da análise
anosel <- seq(2012,2017,1)

#Primeira tentativa de mapa: dados de 
educa <- read.csv2(paste0("data/2012-2017-tab_ecd_ed.csv"))[,-1]
#f <- municipesord$CodMunicipio
#educa <- cbind(codmunicipio = c(as.numeric(levels(f))[f],32),educa)


#pegar o mapa e juntar aos dados de educação

#Mapa municípios ES
mapa_m_es <- get_brmap("City", geo.filter = list(State = 32)) 


#Indicadores do ES
educa <- educa %>% filter(grepl("^32",cod_mun))

#Nome dos indicadores
n_ind_ed <- levels(educa$indicador)

tx <- educa %>% filter(indicador == n_ind_ed[8])


#Estratificar as coberturas para obter menos cores
tx$taxa <- cut(tx$value,
                      c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
#Para plotly
tx$texto <- with(tx, paste(`Município`," - ", `value`))
#Compatibiliza bases para filtragem por código de município
mapa_m_es$cod_mun <- as.numeric(substr(mapa_m_es$City,0,6))


#problema kernels
Sys.setenv("OPENBLAS_CORETYPE"="Haswell")
for (i in 1:length(anosel)) {
  nome <- paste0("mapa_","indicador",anosel[i])
  tx <- tx[tx$ano == anosel[i],]
  mapa <- mapa_m_es %>%
    left_join(tx) %>%
    ggplot() +
    geom_sf(aes(fill = taxa),
            #ajusta tamanho das linhas
            colour = "black", size = 0.1) +
    #muda escala de cores
    scale_fill_viridis_d(option = 2, begin = 1, end = 0.2) +
    theme(panel.grid = 
            element_line(colour = "transparent"),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())+
    labs(title = paste0("Cobertura de Creche no Espirito Santo - ",anosel[i]),
         fill = "Taxa de Cobertura Efetiva de Creche")
  assign(nome, mapa) 
  print(get(nome))
  ggsave(filename = paste0("data/",nome,".png"), width = 120 , height = 90, units = "mm")
}

#Para ver interativamente - problema com fonte de texto
#ggplotly(`mapa tx_creche 2012`) 


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


