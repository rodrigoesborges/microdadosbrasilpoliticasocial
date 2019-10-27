# carrega os pacotes necessários
library(DBI)
library(MonetDBLite)
library(survey)
library(dbplyr)
library(dplyr)
library(srvyr)
library(multicore)

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

calc_ind_cu <- function(ano = 2017) {
  ano <- 2017
  for (j in 1:length(ano)) {
    cadunicoecd<- cad_carr(ano)
    
    #db <- dbConnect( MonetDBLite() , cadunicoecd$db$dbname)
    #cadunicoecd <- dbGetQuery(db , paste0( "SELECT * FROM pessoa_" ,ano," WHERE idade < 7" ) )
    #dbGetQuery( db , "SELECT stype , dnum , cname , SUM( pw ) FROM apiclus1 GROUP BY stype , dnum , cname" ) )
# because this is near-instantaneous, no matter how much data y
    print(str(cadunicoecd))
#    for (i in 1:length(municodigos$codigomun)) {
      # exp <- as.character(municodigos$codigomun[i])
      # echo(exp)
      ecdpbfmun <- svyby(~I(idade < 7),~I(cd_ibge), design = cadunicoecd, svytotal,na.rm = TRUE, multicore = T)

      print (ecdpbfmun)
      cad_mun_0a6 <- sum(ecdpbfmun[,3])
      # echo(cad_mun_0a6)
      #ecdpbfmun$cd_ibge <- substr(ecdpbf)
      ecdpbfmun <- ecdpbfmun %>% left_join(popm_inf[ano = ano,c(1,2,7)], by = ) 
      bf_mun_0a6 <- ecdpbfmun[2,3]
      
      bf_mun_0a6_pop <- bf_mun_0a6/
        tab_ecd_cadun <- rbind(tab_ecd_cadun,c(municodigos$municipio[i],bf_mun_0a6,bf_mun_0a6_pop))  
#    }
  }
}



tab_ecd_cadun <- rbindlist(lapply(anosel, ))



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

