  #Análise Dados Censo do Ensino Superior
#install.packages("devtools")
#install.packages("stringi") 
#devtools::install_github("rodrigoesborges/microdadosBrasil")
require(microdadosBrasil)
#pasta para download dos dados brutos (quando muito grandes)
dadoslocais <- "~/RLocalData/censosuperior"

#Baixar dados caso não exisstam
#download_sourceData("CensoEducacaoSuperior",2017, unzip = F, root_path = dadoslocais)

#read_CensoEducacaoSuperior(ft = 'asas',i = 2017) para ver os disponíveis

ies18 <- read_CensoEducacaoSuperior(ft = "ies",i = 2017, root_path = dadoslocais)
