#compila tabela de tradução de municípios a macrorregiões de planejamento - por enquanto apenas Espírito Santo
#Fonte macrorregiões de planejamento: http://www3.al.es.gov.br/Arquivo/Documents/legislacao/html/LO9768.html
#problema que a tabela é uma imagem
#require("rvest")
require(readxl)
require(readODS)
require(dplyr)
#Regiões IBGE
#ftp://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/2018/DTB_2018.zip

popm_inf <- popm_inf %>% mutate(pop0a6 = round(`0 a 3 anos`+`4 a 6 anos`, digits = 0))

urlmr <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/2018/DTB_2018.zip"

dest_mr <- "data/macrorregioes.zip"
download.file(urlmr, dest_mr)

system(paste0("unzip ",dest_mr)) # consertar para extrair na subpasta data


mr <- read_ods("data/RELATORIO_DTB_BRASIL_DISTRITO.ods", sheet = 1, col_names = T)
mr <- mr[,1:9]
mr <- mr[!duplicated(mr$`Código Município Completo`),]

names(mr)[8] <- "cod_mun"

mrmi <- mr[,c(3,5,6,8)]

mrmi$cod_mun <- floor(mrmi$cod_mun/10) 
mrmi$`Microrregião Geográfica` <- as.numeric(mrmi$`Microrregião Geográfica`)
#Junta coluna com macrorregiao e com população de 0 a 6 às tabelas de indicadores de:
#Educação
#read.csv caso não esteja na memória do ambiente a tabela tab_ecd_ed_indicadores
tab_ecd_ed_indicadores$cod_mun <- as.numeric(tab_ecd_ed_indicadores$cod_mun)
tab_ecd_ed_indicadores <- tab_ecd_ed_indicadores %>% left_join(mrmi[,2:4])

popm_inf$cod_mun <- as.numeric(popm_inf$cod_mun)
tab_ecd_ed_indicadores <- tab_ecd_ed_indicadores %>% left_join(popm_inf[,c(1,3,7)])

##Saúde
indicadores_saude <- read.csv2("data/2012-2017-indicadores_saude_brutos.csv", stringsAsFactors = F)
indicadores_saude$cod_mun <- as.numeric(indicadores_saude$cod_mun)

indicadores_saude <- indicadores_saude %>% left_join(mrmi[,2:4])

indicadores_saude <- indicadores_saude  %>% pivot_longer(cols = c(-1,-2,-3,-12,-13), names_to = "indicador", values_to = "value")



indicadores_saude <- indicadores_saude %>% left_join(popm_inf[,c(1,3,7)])


# Cad unico
cadunicoecd <- readRDS(paste0("data/2012-2017-indicadores-cadunico-ES.rds"))
cadunicoecd <- cadunicoecd[,c(1,6,10,2:5,7:9)] %>% pivot_longer(cols = 4:10, names_to = "indicador")
cadunicoecd$cod_mun <- as.numeric(cadunicoecd$cod_mun)

cadunicoecd <- cadunicoecd %>% left_join(mrmi[,2:4])

cadunicoecd <- cadunicoecd %>% left_join(popm_inf[,c(1,3,7)])
