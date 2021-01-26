###Junta peças para um simples mapa de calor nacional e estadual, municipal de
### Proporção da população no cadúnico 


# carrega os pacotes necessários
library(DBI)
library(MonetDBLite)
library(survey)
library(dbplyr)
library(dplyr)
library(tidyr)
library(srvyr)
library(gsubfn)
library(data.table)




##1) Carrega CadUnico

source("R/carregacad.R")

#cad18 <- carregacad()

##2) Carrega tabela populacional
library(munipopsbr)
popmf20182020 <- popmunicipal(2017,2020)

popfaixas <- popmf20182020 %>% pivot_wider(names_from = faixa_etaria, values_from = populacao)

popfaixas$Total <- rowSums(popfaixas[,-1:-3])

##3) Calcula o indicador - adapta função (pensar em fazer a função adaptável como uma
##peça?)


propcad <- function(ano = 2017, 
                        uf = 32, 
                        cadunico = paste0("CadUnico/cadunico 2018 design.rds"),
                        tabpop = popfaixas) {
  if(ano != 2018) {dado = paste0("CadUnico/cadunico ",ano," design.rds")}
  #print(str(cadunico))
  #filtroc <- gsubfn("%s",ufc,"cd_ibge %like% '%s' & idade < 7 & marc_pbf")
    cadunico <-carregacad(2018)
#  ifelse(uf >0,
          ufc <-   paste0("^",uf)
        
         cad_unico <- subset(cadunico, grepl(sprintf("%s",ufc),cd_ibge) & marc_pbf)
         #,
#         cad_unico <- subset(cadunico, cd_ibge & marc_pbf) )
  rm(cadunico)

  gc()
  #db <- dbConnect( MonetDBLite() , cadunico$db$dbname).
  #cadunico <- dbGetQuery(db , paste0( "SELECT * FROM pessoa_" ,ano," WHERE idade < 7" ) )
  #dbGetQuery( db , "SELECT stype , dnum , cname , SUM( pw ) FROM apiclus1 GROUP BY stype , dnum , cname" ) )
  propcad <- svyby(~marc_pbf,~cd_ibge,cad_unico, svytotal,na.rm = TRUE )
  #Compatibiliza com códigos do datasus
  propcad$cd_ibge <- substr(propcad$cd_ibge,1,6)
  
  names(propcad) <- c("cod_mun","n_pbf_cadunico", "ben_pbf", "desv_pad_n_pbf", "desv_pad_pbf")
  
  #  & grepl(sprintf("%s",ufc) <- não precisa caso left join, não?
  propscad <- propcad %>% 
    left_join(tabpop[tabpop$ano == ano & grepl(sprintf("%s",ufc),tabpop$cod_mun),], by = "cod_mun") 
  
  propscad <- propscad %>% mutate(cod_mun = cod_mun,
                                   Município = Município,
                                   ano = ano,
                                   "prop_pop_cadunico" = (n_pbf_cadunico+ben_pbf)/Total, 
                                   "prop_pbf" = ben_pbf/Total
  )
  gc()                                 
  propscad 
}


#para varios anos
#tab_cadunico <- rbindlist(lapply(anosel, propcad))
ufs <- read.csv2("data/cod_ufs.csv")


propcad17 <- rbindlist(lapply(ufs$Códigos,propcad, ano = 2017))
###3) Função para plotar mapa - idem - adaptar para flexibilizar como função

propcad18es <- propcad()







