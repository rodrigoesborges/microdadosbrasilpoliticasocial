#Tentativa de acesso a censo SUAS
#Longo prazo - verificar possibilidade de algo parecido a integrar em lodown 
#feito uma vez
# ajdrepo <- "ajdamico/lodown"
# install_github(ajdrepo, dependencies = TRUE)



#reposrvyr <- "gergness/srvyr"
require("devtools")
require(lodown)
require(haven)
require(survey)


#CENSO SUAS 2017 - 
# Já feito manualmente, primeira exploração
# URL 2017- 
#url2017 <- "https://aplicacoes.mds.gov.br/sagi/pesquisas/documentos/microdado/microdado_210.zip"
#download.file(url2017,pastadados)

pastadados <- "~/RLocalData/censoSUAS/"

unzip(paste0(pastadados,nomcarq), exdir = pastadados, unzip = getOption("unzip"))

nomcarq <- "microdado_210.zip"
nomcpad <- "Censo SUAS 2017"
unidinst <- "CRAS"
tipo <- "divulgacao"


      
nomcras <- paste0(pastadados,
                 nomcpad,"/",
                 gsub(" ","_",nomcpad),"_",
                 unidinst,"/",
                 nomcpad,"_",
                 unidinst,"_"
                 ,tipo,".sav")

crastab <- haven::read_spss(paste0(nomcras))


#De interesse - CRAS
#q1 - localização Urb C  Urb P Rural (10)
#q2 - capacidade do centro (11)
#q15.1 - familias PAIF (124)
#q16 - diretamente SCFV - sim/total (126)
#q17.1 - sim/total - sim / 0 a 6 - (127)
#q19.1 - diretamente SCFV - sim/total (137)
#q21 - frequência de visitas - cras à rede - não muito util - (144)
#q28 - PAIF por equipe volante - possivel prop total PAIF (163)
#q38 - pode ser util bairros de abrangência rural 2  1 0 (210)


#Explorar apenas ES
uf <- "^32"
varcras <- c(1,4,7,10,11,18,28,29,32,33,34,124,125,126,127,137,144,136,210)
crastabes <- crastab[grepl(uf,crastab$IBGE7),varcras]



### CREAS não possui questionario adaptado a primeira infância

#De interesse - Abrigos
#q15 - faixa etária e sexo dos abrigados
#q31 - tempo de acompanhamento após desligamento
#informação de q31 será mais fiável caso atendimento seja na própria unidade (?)





