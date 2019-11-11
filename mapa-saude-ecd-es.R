#Mapas de situação da saudeção da primeira infância nos municípios do ES



#Pacotes necessários
require(tidyverse)
devtools::install_github("rpradosiqueira/brazilmaps")
require(dplyr)
require(brazilmaps)
require(plotly)


#variaveis de ambiente e prefixadas
dtsav <- "~/RLocalData/ecd-indicadores"

#Anos da análise
anosel <- seq(2012,2017,1)

#Primeira tentativa de mapa: dados de 
saude <- read.csv2("data/2012-2017-indicadores_saude_brutos.csv", stringsAsFactors = F)
saude <- saude %>% pivot_longer(cols = 4:11, names_to = "indicador", values_to = "valor")
saude$cod_mun <- as.numeric(saude$cod_mun)
#pegar o mapa e juntar aos dados de saudeção

#Mapa municípios ES
mapa_m_es <- get_brmap("City", geo.filter = list(State = 32)) 


#Indicadores do ES
saude <- saude %>% filter(grepl("^32",cod_mun))


#Nome dos indicadores
n_ind_sa <- data.frame(indicador = unique(saude$indicador))



n_ind_sa$leg <- c("mortes maternas no parto a cada 1.000 partos",
                  "mortes maternas no parto a cada 1.000 mulheres em idade fértil".
                  "Partos Cesários (% total)",
                  "Fecundidade adolescente ( /1.000 adolescentes 10 a 19)",
                  "Nascidos em hospital (% total)         ",
                  "Gestações que não tiveram consultas pré-natais (% total)",
                  "Gestações com consultas pré-natais insuficientes (% total)",
                  "Gestações com 7 ou mais consultas pré-natais (% total)")

            
                  
                  
n_ind_sa$tit <- c("Primeira Infância - Saúde Materna",
                      "Primeira Infância - Saúde Materna",
                      "Primeira Infância - Saúde Materna",
                      "Primeira Infância - Saúde Materna",
                      "Primeira Infância - e Saúde - Nascidos",
                      "Primeira Infância - Saúde Materna",
                      "Primeira Infância - Saúde Materna",
                      "Primeira Infância - Saúde Materna")


mapas_sa_ecd <- function(ano = anosel,uf = "ES", ci = n_ind_sa) {

  
for (i in 1:nrow(ci)) {                         
tx <- saude %>% filter(indicador == ci[i,1])

#Estratificar as coberturas para obter menos cores
if (max(tx$valor, na.rm = T)<= 1) {
tx$taxa <- cut(tx$valor,
                      c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
}
else {
  m <- max(tx$valor, na.rm = TRUE)
  tx$taxa <- cut(tx$valor,
                 c(0,round(m/5, digits = 0),
                   round(m*2/5, digits = 0),
                   round(m*3/5, digits = 0),
                   round(m*4/5, digits = 0),
                   m))
}

#Compatibiliza bases para filtragem por código de município
mapa_m_es$cod_mun <- as.numeric(substr(mapa_m_es$City,0,6))
txs <- tx

n_part1 <- paste0("mapa_",ci[i,1])
print(n_part1)
ci$leg[i]
#problema kernels
#Sys.setenv("OPENBLAS_CORETYPE"="Haswell")
for (j in 1:length(ano)) {
  nome <- paste0(n_part1,"_",anosel[j])
  tx <- txs[txs$ano == anosel[j],]
  mapa <- mapa_m_es %>%
    left_join(tx) %>%
    ggplot() +
    geom_sf(aes(fill = taxa),
            #ajusta tamanho das linhas
            colour = "black", size = 0.1) +
    #muda escala de core
    scale_fill_viridis_d(option = 2, begin = 1, end = 0) +
    theme(panel.grid = 
            element_line(colour = "transparent"),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())+
    labs(title = paste0(ci$tit[i]," - ",uf," - ",anosel[j]),
         fill = ci$leg[i])
  assign(nome, mapa) 
  print(get(nome))
  ggsave(filename = paste0(dtsav,"/",nome,".png"), dpi = "retina", scale = 1.2)
}
}
  }

mapas_sa_ecd()

#Para ver interativamente - problema com fonte de texto
#ggplotly(`mapa tx_creche 2012`) 

#Para plotly
tx$texto <- with(tx, paste(`Município`," - ", `valor`))


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


