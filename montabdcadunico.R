# instala os pacotes:
libs <- c("DBI", "MonetDBLite" , "fst" , "survey")
libs.novas <- libs[ !( libs %in% installed.packages()[ , "Package" ] ) ]
if( length( libs.novas ) ) install.packages( libs.novas )

# carrega os pacotes necessários
library(DBI)
library(MonetDBLite)
library(survey)

# carrega funções do script
##downloader::source_url("https://github.com/guilhermejacob/guilhermejacob.github.io/blob/master/scripts/cadunico.R")
source("cadunico.R")
# define o diretório onde serão depositados os dados
output_dir <- file.path( getwd() , "CadUnico" )

# coleta o "catálogo de dados" no site do MDS:
catalog <- catalog_cadunico( output_dir )

# O próximo passo é opcional:
# Aqui, escolho apenas os dados do último ano
catalog <- catalog[ catalog$year == 2017 , ]

# Vamos usar esse "catálogo" como o argumento da função build_cadunico
# Essa última função vai montar a base de dados
build_cadunico( catalog )
