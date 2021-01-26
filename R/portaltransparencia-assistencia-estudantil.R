#Portal da Transparência - Exercício de manipular valores de assistência estudantil nas IFES
#https://github.com/pompylio/portransp
library('devtools')
#instalar apenas ua vez
#install_github('pompylio/portransp')
library(portransp)
require(openxlsx)


#despesas-execucao
orc <- "orcamento-despesa"
pasta <- "~/RLocalData/portaltransparencia/despesas/"

#baixa dados de despesa
potr_download_all(opendata = orc, destfile = pasta, , download.file.mode = "wb", extra = getOption("-k -L"))





# Exemplo de apenas alguns anos
anos <- seq(2015,2018,1)
for (ano in anos) {
  potr_download(opendata = orc, reference = ano, destfile = pasta, , download.file.mode = "wb", extra = getOption("-k -L"))
  unzip(paste0(pasta,orc,"_",ano,".zip"),exdir = pasta)
}


#descompacta dados
nomepad <- "OrcamentoDespesa.zip.csv"

#ler dados 
orc2017 <- read.csv2(paste0(pasta,"2017_",nomepad),header = T, check.names = FALSE, fileEncoding = "iso-8859-1")
#criterios
nomeprograma <- "EDUCACAO DE QUALIDADE PARA TODOS"
nomeacao <- "ASSISTENCIA AOS ESTUDANTES DAS INSTITUICOES FEDERAIS DE EDUCACAO PROFISSIONAL E TECNOLOGICA"

orc <- orc2017[orc2017$`NOME PROGRAMA ORÇAMENTÁRIO` == nomeprograma & orc2017$`NOME AÇÃO` == nomeacao,]

orc2016 <- read.csv2(paste0(pasta,"2016_",nomepad),header = T, check.names = FALSE, fileEncoding = "iso-8859-1")

orc16 <- orc2016[orc2016$`NOME PROGRAMA ORÇAMENTÁRIO` == nomeprograma & grepl("ASSISTENCIA*",orc2016$`NOME AÇÃO`),]

orc <- rbind(orc,orc16)

##Vamos montar uma só com todos os anos?
for (ano in anos) {
  
  
}

library(ggplot2)
library(scales)
#referencia https://ggplot2.tidyverse.org/
#ggplot exemplo de gráfico simples
ggplot(orc, aes(orc$EXERCÍCIO, orc$`ORÇAMENTO REALIZADO (R$)`))+
         geom_bar(stat = "identity", aes(colour = orc$`NOME UNIDADE ORÇAMENTÁRIA`))+
          scale_y_continuous(breaks= pretty_breaks())











#ref. futura https://plot.ly/r/












#seria possivel adaptar de:
#https://raw.githubusercontent.com/pompylio/portransp-panel/master/function/function.R
#link <- "https://raw.githubusercontent.com/pompylio/portransp-panel/master/function/function.R"
#download.file(link,destfile = "funcaoorcpainel.R", method = "curl",mode = "wb")


#Aparentemente fechado, não serviria
#http://plataformanilopecanha.mec.gov.br/2019.html