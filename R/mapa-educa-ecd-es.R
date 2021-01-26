#Mapas de situação da educação da primeira infância nos municípios do ES



#Pacotes necessários
library(tidyverse)
#devtools::install_github("rpradosiqueira/brazilmaps")
library(brazilmaps)
library(ggplot2)
library(plotly)
library(data.table)

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



##################AGREGADO POR MESORREGIÕES - MÉDIA PONDERADA
tab_ecd_ed_indicadores$reg_micro <- floor(tab_ecd_ed_indicadores$cod_mun/10000)*1000+as.numeric(tab_ecd_ed_indicadores$`Microrregião Geográfica`)

tab_ecd_ed_indicadores[is.na(tab_ecd_ed_indicadores$pop0a6),]$pop0a6 <- 0

#Fatores e mudança para nomes melhores os indicadores
tab_ecd_ed_indicadores$indicador <- as.factor(tab_ecd_ed_indicadores$indicador)

nom_ind_ed <- c("Alunos de creche por sala",
               "Alunos de pré-escola por sala",
               "Creches para cada 1.000 crianças de 0 a 3 anos",
               "Matrículas públicas em creche (% do total)",
               "Matrículas públicas em pré-escolas (% do total)",
               "Pré-escolas para cada 1.000 crianças de 4 a 6 anos",
               "Proporção de creches públicas",
               "Proporção de Pré-escolas públicas",
               "Taxa de Cobertura Bruta de Creche",
               "Taxa de Cobertura Efetiva de Creche",
               "Taxa de Cobertura Bruta de Pré-escola",
               "Taxa de Cobertura Efetiva de Pré-escola"
)

levels(tab_ecd_ed_indicadores$indicador) <- nom_ind_ed



educa_mr <- tab_ecd_ed_indicadores %>% group_by(reg_micro,ano,indicador) %>% 
  summarise(valor = weighted.mean(valor,pop0a6, na.rm = T), 
            `Microrregião` = max(Nome_Microrregião), 
            cod_mr = max(reg_micro))

educa_mr_br <- tab_ecd_ed_indicadores %>% group_by(ano, indicador) %>% 
  summarise(valor = weighted.mean(valor, pop0a6, na.rm = T),
            `Microrregião` = "Brasil",
            cod_mr = 0)

tab_ecd_ed_indicadores$uf <- floor(tab_ecd_ed_indicadores$cod_mun/10000)

educa_mr_uf <- tab_ecd_ed_indicadores %>% group_by(ano, indicador, uf) %>% 
  summarise(valor = weighted.mean(valor, pop0a6, na.rm = T),`Microrregião` = as.character(max(uf)),
            cod_mr = max(uf)*1000)

educa_mr <- rbind(educa_mr, educa_mr_uf,educa_mr_br)

educa_mr <- educa_mr %>% filter(grepl("^32|^0",cod_mr)) 

educa_mr[educa_mr$cod_mr == 0,]$Microrregião <- "Brasil"

## Para o Espírito Santo apenas
educa_mr_es <- educa_mr[floor(educa_mr$cod_mr/1000) == 32 | educa_mr$cod_mr == 0,]

### apenas Brasil, Espírito Santo e 5 cidades fixas

regsel <- c(32013,32004,32011,32002,32009,32000,0)

#Montanha - 32004P
#Vitória - 32009
#Itapemirim - 32013
#Guarapari - 32010
# Alegre - 32011
# Nova Venécia - 3202

educa_mr_es <- educa_mr_es[educa_mr_es$cod_mr %in% regsel,]

educa_mr_es[educa_mr_es$Microrregião == 32,]$Microrregião <- "Espírito Santo"

educa_mr_es$indicador <- as.character(educa_mr_es$indicador)

graficos_educacao_mr <- lapply(1:length(unique(educa_mr$indicador)), function(x) {
  ggplot(educa_mr_es[educa_mr_es$indicador == unique(educa_mr_es$indicador)[x],],
         aes(x = ano, y = valor, colour = `Microrregião`))+
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_line()+
  theme_classic()+
    labs(title = educa_mr_es$indicador[x])
  }
  )

lapply(1:length(unique(educa_mr$indicador)),function(x) ggplotly(graficos_educacao_mr[[x]]))  


lapply(1:length(unique(educa_mr$indicador)),function(x) {
  print(paste0(dtsav,"/es-microrregiao-",levels(tab_ecd_ed_indicadores$indicador)[x],".png"))
  ggsave(filename = paste0(dtsav,"/es-microrregiao-",levels(tab_ecd_ed_indicadores$indicador)[x],".png"), plot = graficos_educacao_mr[[x]],width = 180 , height = 90, units = "mm")
  })
