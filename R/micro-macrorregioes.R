#compila tabela de tradução de municípios a macrorregiões de planejamento - por enquanto apenas Espírito Santo
#Fonte macrorregiões de planejamento: http://www3.al.es.gov.br/Arquivo/Documents/legislacao/html/LO9768.html
#problema que a tabela é uma imagem
#require("rvest")
require(readxl)
require(readODS)
#Regiões IBGE
#ftp://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/2018/DTB_2018.zip

urlmr <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/2018/DTB_2018.zip"

dest_mr <- "data/macrorregioes.zip"
download.file(urlmr, dest_mr)

system(paste0("unzip ",dest_mr)) # consertar para extrair na subpasta data


mr <- read_ods("data/RELATORIO_DTB_BRASIL_DISTRITO.ods", sheet = 1, col_names = T)
mr <- mr[,1:9]
mr <- mr[!duplicated(mr$`Código Município Completo`),]

