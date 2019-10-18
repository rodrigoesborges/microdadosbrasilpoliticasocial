# O objetivo deste script é baixar e processar as estimativas
# populacionais do IBGE para construção de indicadores 
# nacionais, estaduais ou municipais com os demais
# microdados disponíveis

#dados municipais
# As bases são o SIDRA e o DATASUS
devtools::install_github('rodrigoesborges/RSidra')
require(RSIDRA)
#Necessário caso seja atualizado o pacote datasus
#require(devtools)
#devtools::install_github('rodrigoesborges/datasus', force = TRUE)
require(datasus)
require(stringr)
require(tidyverse)
require(dplyr)
#library(stringr)
#library(scales)
# ----- dados populacionais básicos -----------------------------------------------------------

#Tabela no SIDRA
#tirar a tabela
#tabela população '6579'
t <- 6579
#Variável retirada através de SIDRA_variaveis(t)
#variavel do SIDRA contagem pop 9324
vpop <- 9324
#nível de desagregação municipal : 6
nivmun <- 6
#periodo
anoin <- 2017
anofim <- 2017
popmun17 <- API_SIDRA(t, nivel = nivmun, variavel = vpop, inicio = anoin, fim = anofim)

municodl <- data.frame(codigomun = popmun17$`Município (Código)`, municipio = popmun17$Município, stringsAsFactors = FALSE)
#municodl$codatasus <- as.numeric(substr(municodl$codigomun,1,6))
#write.csv2(municodl,"data/tabcodigosmunibgedatasus.csv", row.names = FALSE)

popmunes17 <- popmun17[grepl("^32",popmun17$`Município (Código)`),c(1,5,6,length(popmun17))]







#Cod UF IBGE '32'
#require(datasus)
#======= Necessidade da pirâmide etária=========
#===== 1) 2015 = Pirâmide etária municipal / pirâmide etária estadual = fator para cada faixa
#======2) Pir. Etária Estadual 2017
#======3) Aplicar os fatores de 2 em 1, ou seja, multiplicar o vetor 2 em cada linha de 1
#======4) Balancear para que soma seja 100% da pirâmide etária balanceada
#======5) aplicar os totais, deixar proporcional de forma que a soma total do mun = proj mun 2017

#1) 2015 DATASUS/RIPSA
# Obter dados de população por faixa por municipio e total ES
# Datasus RIPSA último ano 2015
popes <- novapop_popbr_mun(unidade_da_federacao = "Espírito Santo", periodo = "2015")

#calcular piramide etaria municipal = valor da coluna / pela soma da linha (última coluna, com total)

piramesm <- popes %>% 
  mutate_at(vars(c(-Município,-Total)), funs(. / Total))

#separa municipios e códigos para junção final
municipesord <- as.data.frame(str_match(as.character(piramesm[-1,1]), "^(\\d+) (.*)"))[,-1]

municod <- c("CodMunicipio","Municipio")

names(municipesord) <- municod
#Primeira linha é a pirâmide estadual total
pirames <- as.vector(piramesm[1,c(-1,-19)], mode = "numeric")

#dividir todas as linhas
piramfator <- t(t(piramesm[-1,c(-1,-19)])/pirames)


#2) piramide etaria ES 2017
#Faixa etária tem mais categorias, reduzir ao denominador comum
ultfaixa <- names(popes)[length(popes)-1]
popesult <- ibge_projpop_bruf(unidade_da_federacao = "Espírito Santo", coluna = "Faixa Etária 1", periodo = "2017")

popesult <- popesult[-1,c(-1,-length(popesult))] %>%
            mutate(ultfaixa = rowSums(.[17:19])) %>%
            select(-17,-18,-19)

names(popesult)[length(popesult)] <- ultfaixa
piramesult <- popesult /rowSums(popesult)

#3) 2 em 1
piramesult <- as.vector(piramesult, mode = 'numeric')

piramfatoratual <- t(t(piramfator)*piramesult)

#4)balancear para que seja a soma 100%
piramfatoratual <- piramfatoratual/rowSums(piramfatoratual)


piramfatoratual <- cbind(municipesord,piramfatoratual)

#5) Multiplicar percentuais pela população total
#Juntar totais como coluna a partir do código do município
#Acertar nomes das colunas
popmunes17 <- popmunes17[,c(2,4)]
names(popmunes17) <- c(municod[1],"populacaototal")

#Trunca código município para bater com fonte DATASUS
popmunes17[,1] <- substr(popmunes17[,1],1,6)

es17popidades <- merge(piramfatoratual,popmunes17, by = municod[1])

es17popidades[,3:19] <- round(es17popidades[,3:19]*as.vector(es17popidades[,20]), 0)
es17popidades$CodMunicipio <- as.numeric(as.character(es17popidades$CodMunicipio))
# Passos se nao datasus/RIPSA - com dados do CENSO:
#   
#   Sinopse do Censo 2010 - ibge
# 
# 
# Exemplo URL Cidade de Vitória
# https://censo2010.ibge.gov.br/sinopse/webservice/frm_piramide.php?codigo=320530.




###População por grupos de IDADE - IBGE PNAD Contínua trimestral - Tab 5918 - Capitais há - conferir depois
t <- 5918
vpop <- 606
nivmun <- 6
grupoidade <- 58
anoin <- "201704"
anofim <- "201804"
#cod_cat = rep("all", 1)
popmun <- API_SIDRA(t, nivel = nivmun, variavel = vpop, inicio = anoin, fim = anofim)

