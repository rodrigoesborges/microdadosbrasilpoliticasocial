#Tentativa de acesso a censo SUAS

#feito uma vez
ajdrepo <- "ajdamico/lodown"
reposrvyr <- "gergness/srvyr"
install_github(ajdrepo, dependencies = TRUE)

require("lodown")
require("devtools")
require(lodown)
require(survey)


#CENSO SUAS 2017 - 
# Já feito manualmente, primeira exploração
# URL 2017- 
#url2017 <- "https://aplicacoes.mds.gov.br/sagi/pesquisas/documentos/microdado/microdado_210.zip"
#download.file(url2017,pastadados)

pastadados <- "~/RLocalData/censoSUAS/"


nomecenso <- "microdado_210.zip"

unzip(paste0(pastadados,nomecenso), exdir = pastadados, unzip = getOption("unzip"))
      
