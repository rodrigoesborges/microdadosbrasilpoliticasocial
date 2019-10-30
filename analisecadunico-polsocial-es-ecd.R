# carrega os pacotes necessários
library(DBI)
library(MonetDBLite)
library(survey)
library(dbplyr)
library(dplyr)
library(srvyr)
library(gsubfn)
library(data.table)
#período de análise
anosel <- seq(2012,2017,1)

# define o diretório onde serão depositados os dados - IDEM Montabcadunico.R
output_dir <- file.path( getwd() , "CadUnico" )

#Carraga tabela de compatibilidade datasus-ibge
municodigos <- read.csv2("data/tabcodigosmunibgedatasus.csv")

#carg_cadunico - iterar de acordo com o ano

cad_carr <- function(anos = 2017,dado = "CadUnico/cadunico 2017 design.rds" ) {
  # lê o objeto com o desenho amostral
  pes.design <- readRDS(dado)
  # abre a conexão do objeto com a base em MonetDBLite
  
    pes.design <- open( pes.design, driver = MonetDBLite())
    pes.design
    
}


#total.ecd <- svytotal( ~I(idade < 6) , pes.design , na.rm = TRUE )

#tabela de idades
#sum(as.numeric(tab_bruta_ecd_ed[6,c(6,8)]))
popm_inf <- readRDS("data/estimativas_pop_1a_infancia.rds")

popm_inf <- popm_inf %>% mutate(pop0a6 = round(`0 a 3 anos`+`4 a 6 anos`, digits = 0))

#A função deve incluir campo para UF

calc_ind_cu <- function(ano = 2017, uf = 32) {
    ufc <- paste0("^",uf)
    dado <- paste0("CadUnico/cadunico ",ano," design.rds")
    cadunicoecd<- cad_carr(ano, dado)
    #filtroc <- gsubfn("%s",ufc,"cd_ibge %like% '%s' & idade < 7 & marc_pbf")
    cadunico_ecd <- subset(cadunicoecd, grepl("^32",cd_ibge) & idade < 7 & marc_pbf)
    
    #db <- dbConnect( MonetDBLite() , cadunicoecd$db$dbname).
    #cadunicoecd <- dbGetQuery(db , paste0( "SELECT * FROM pessoa_" ,ano," WHERE idade < 7" ) )
    #dbGetQuery( db , "SELECT stype , dnum , cname , SUM( pw ) FROM apiclus1 GROUP BY stype , dnum , cname" ) )
      ecdpbfmun <- svyby(~I(marc_pbf > 0),~cd_ibge,cadunico_ecd, svytotal,na.rm = TRUE, multicore = T)

      #Compatibiliza com códigos do datasus
      ecdpbfmun$cd_ibge <- substr(ecdpbfmun$cd_ibge,1,6)
      
      names(ecdpbfmun) <- c("cod_mun","n_pbf_cadunico", "ben_pbf", "desv_pad_n_pbf", "desv_pad_pbf")
      
      ecdpbfmun <- ecdpbfmun %>% right_join(popm_inf[popm_inf$ano == ano  & grepl("^32",popm_inf$cod_mun),c(1,2,7)], by = "cod_mun") 
      
      dbDisconnect(cadunicoecd$db$dbname)
        
      ecdpbfmun <- ecdpbfmun %>% mutate("prop_pinf_cadunico" = (n_pbf_cadunico+ben_pbf)/pop0a6, "pbf_pop0a6" = ben_pbf/pop0a6, ano = ano)
      
      ecdpbfmun 
}


tab_ecd_cadunico <- rbindlist(lapply(anosel, calc_ind_cu))
saveRDS(tab_ecd_cadunico, "data/2012-2017-indicadores-cadunico-ES.rds", compress = "gzip")


#### Para o ES

# calcula o número de crianças de 0 a 6 anos no CadÚnico
total.ecd <- svytotal( ~I(idade < 6) , pes.design , na.rm = TRUE )
#                       total    SE
# I(idade < 6)FALSE   66681874 15946.6
# I(idade < 6)TRUE     8778040  6929.7

# calcula intervalo de confiança
confint( total.ecd ) 
#                       2.5 %   97.5 %

# EX.:  calcula o número de crianças de 0 a 6 anos ES e não ES no CadÚnico
##Abaixo n funcionou - continou cálculo com totais
#totales.ecd <- svyby(~idade <6, ~I(grepl("320....",~cd_ibge)), pes.design, svytotal, na.rm = TRUE)

#calcula o intervalo de confiança
#confint(totales.ecd)

## subconjunto apenas das crianças no cadúnico
cadunicoecd <- subset(pes.design, idade < 6)

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ecdpbfes <- svyby(~grepl("^32",cd_ibge), ~marc_pbf, cadunicoecd, svytotal, na.rm = TRUE)

#calcula o intervalo de confiança
confint(ecdpbfes)
#                                        2.5 %     97.5 %
# FALSE:grepl("^32", cd_ibge)FALSE 1908485.47 1921847.45
# TRUE:grepl("^32", cd_ibge)FALSE  6715146.96 6740219.18
# FALSE:grepl("^32", cd_ibge)TRUE    41224.43   43259.15
# TRUE:grepl("^32", cd_ibge)TRUE     91307.06   94589.68
cad_es_0a6 <- sum(ecdpbfes[,3])
bf_es_0a6 <- ecdpbfes[2,3]

bf_es_0a6_pop <- bf_es_0a6/sum(as.numeric(tab_bruta_ecd_ed[6,c(6,8)]))


tab_ecd_cadun <- data.frame(matrix(ncol = 3, nrow=0), stringsAsFactors = FALSE)



  ecdbpcfes <- svyby(~grepl("^32",cd_ibge), ~marc_bpc, cadunicoecd, svytotal, na.rm = TRUE)

# fecha a conexão com a base de dados
#close( pes.design , shutdown = TRUE )

