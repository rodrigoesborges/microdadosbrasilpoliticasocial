#Anotações limpa est_pop_m_0a6.R
###Futuramente possível complemento
# Passos se nao datasus/RIPSA - com dados do CENSO:
#   
#   Sinopse do Censo 2010 - ibge
# 
# 
# Exemplo URL Cidade de Vitória
# https://censo2010.ibge.gov.br/sinopse/webservice/frm_piramide.php?codigo=320530.




###População por grupos de IDADE - IBGE PNAD Contínua trimestral - Tab 5918 - Capitais há - conferir depois
#Grupos só de:
#0 a 13
#14 a 17
#18 a 24
#25 a 39
#40 a 59
#Total (inclui 60 ou mais)
#t <- 5918
#vpop <- 606
#nivmun <- 6
#grupoidade <- 58
#anoin <- "201704"
#anofim <- "201804"
#cod_cat = rep("all", 1)
#popmun <- API_SIDRA(t, nivel = nivmun, variavel = vpop, inicio = anoin, fim = anofim)




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



#2) piramide etaria ES 2017

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

