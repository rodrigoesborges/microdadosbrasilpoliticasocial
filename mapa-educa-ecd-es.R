#Mapas de situação da educação da primeira infância nos municípios do ES



#Pacotes necessários
library(tidyverse)
devtools::install_github("rpradosiqueira/brazilmaps")
library(brazilmaps)
library(plotly)


#variaveis de ambiente e prefixadas
dtsav <- "~/RLocalData/ecd-indicadores"

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
n_ind_ed <- data.frame(indicador = levels(educa$indicador))



n_ind_ed$leg <- c("Alunos por Sala - Creches",
                  "Alunos por Sala - Pré-escolas",
                  "Creches para cada 1.000 crianças",
                  "Pré-escolas para cada 1.000 crianças",
                  "Número de creches públicas (% total)",
                  "Número de Pré-escolas (% total)",
                  "Taxa de Cobertura Bruta de Creche (%)",
                  "Taxa de Cobertura Efetiva de Creche (%)",
                  "Taxa de Cobertura Bruta de Pré-escolas (%)",
                  "Taxa de Cobertura Efetiva de Pré-escolas (%)"
)

            
                  
                  
n_ind_ed$tit <- c("Indicadores da Primeira Infância - 0 a 3 anos",
                      "Indicadores da Primeira Infância - 4 a 6 anos",
                      "Indicadores da Primeira Infância - 0 a 3 anos",
                      "Indicadores da Primeira Infância - 4 a 6 anos",
                      "Indicadores da Primeira Infância - 0 a 3 anos",
                      "Indicadores da Primeira Infância - 4 a 6 anos",
                      "Indicadores da Primeira Infância - 0 a 3 anos",
                      "Indicadores da Primeira Infância - 0 a 3 anos",
                      "Indicadores da Primeira Infância - 4 a 6 anos",
                      "Indicadores da Primeira Infância - 4 a 6 anos"
)


mapas_ed_ecd <- function(ano = anosel,uf = "ES", ci = n_ind_ed) {

  
for (i in 1:nrow(ci)) {                         
tx <- educa %>% filter(indicador == ci[i,1])

#Estratificar as coberturas para obter menos cores
if (max(tx$value, na.rm = T)<= 1) {
tx$taxa <- cut(tx$value,
                      c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
}
else {
  m <- max(tx$value, na.rm = TRUE)
  tx$taxa <- cut(tx$value,
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
  ggsave(filename = paste0(dtsav,"/",nome,".png"), width = 140 , height = 90, units = "mm")
}
}
  }

mapas_ed_ecd()

#Para ver interativamente - problema com fonte de texto
#ggplotly(`mapa tx_creche 2012`) 

#Para plotly
tx$texto <- with(tx, paste(`Município`," - ", `value`))


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


