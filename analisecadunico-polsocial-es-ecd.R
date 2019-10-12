# carrega os pacotes necessários
library(DBI)
library(MonetDBLite)
library(survey)
library(dbplyr)
library(dplyr)

# define o diretório onde serão depositados os dados - IDEM Montabcadunico.R
output_dir <- file.path( getwd() , "CadUnico" )

# lê o objeto com o desenho amostral
pes.design <- readRDS( file.path( output_dir , "cadunico 2017 design.rds" ) )

  # abre a conexão do objeto com a base em MonetDBLite
pes.design <- open( pes.design , driver = MonetDBLite() )

# conta o número de observações na base
# Caso não tenha sido rodado na mesma sessão, seria preciso isto?
#db <- DBI::dbConnect( MonetDBLite::MonetDBLite() , catalog[  , "dbfolder" ] )
dim( pes.design )
# [1] 14836037       61
# [1] 14548324       62 (2017)




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


for (i in 1:length(municodigos$codigomun)) {
#  exp <- paste0("^",municodigos$codigomun[i])
  exp <- as.character(municodigos$codigomun[i])
  echo(exp)
  ecdpbfmun <- svyby(~grepl('", exp,"',cd_ibge), ~marc_pbf, cadunicoecd, svytotal, na.rm = TRUE)
  echo (ecdpbfmun)
  cad_mun_0a6 <- sum(ecdpbfmun[,3])
  echo(cad_mun_0a6)
  bf_mun_0a6 <- ecdpbfmun[2,3]

  bf_mun_0a6_pop <- bf_mun_0a6/sum(as.numeric(tab_bruta_ecd_ed[6,c(6,8)]))
  tab_ecd_cadun <- rbind(tab_ecd_cadun,c(municodigos$municipio[i],bf_mun_0a6,bf_mun_0a6_pop))  
}

  ecdbpcfes <- svyby(~grepl("^32",cd_ibge), ~marc_bpc, cadunicoecd, svytotal, na.rm = TRUE)

# fecha a conexão com a base de dados
#close( pes.design , shutdown = TRUE )

