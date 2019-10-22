# O objetivo deste script é baixar e processar as estimativas
# populacionais do IBGE para construção de indicadores 
# nacionais, estaduais ou municipais com os demais
# microdados disponíveis - foco inicial em população de primeira infância
#0 a 6 anos

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
require(data.table)
# Correção de valores nulos extraídos do datasus
source("R/zr_fun.R")
#library(stringr)
#library(scales)

#Definição do período de estimativas populacionais
#Ano inicial atual primeiro ano com microdados disponíveis CadÚnico
anoin <- 2012
afim <- 2017
anosel <- seq(anoin,afim,1)



###1) Obtenção das população por faixa etária e consolidação em uma base geral de população por faixa etária por município:
#Inicialização de tabela base de população por faixa etária
popmf <- data.frame(matrix(ncol = 4, nrow=0))

#Definição do último ano disponível
uano <- 2015
#Definição do primeiro ano de estudo - atualmente primeiro ano com microdados de CadÚnico disponíveis
pano <- anoin
rp_anos <- as.character(seq(pano,uano, 1))

for (i in 1:length(rp_anos)) {
  pmf <- novapop_popbr_mun(periodo = rp_anos[i])
  pmf <- pmf %>% gather(faixa_etaria,populacao,-1) %>%
    separate(`Município`, c("cod_mun","Município"),sep="\\s", extra = "merge", fill = "left")
  pmf <-  cbind(ano = rep(rp_anos[i],nrow(pmf)),pmf)
  popmf <- rbind(popmf,pmf)
}

popmf$ano <- as.numeric(as.character(popmf$ano))

###2) Estimação da população por faixa etária para anos posteriores ao último disponível

#======= Necessidade da pirâmide etária=========
#===== 1) 2015 / último ano disponível = Pirâmide etária municipal / pirâmide etária estadual = fator para cada faixa
#======2) Pir. Etária Estaduais para cada ano > último ano disponível
#======3) Aplicar os fatores de 2 em 1, ou seja, multiplicar o vetor 2 em cada linha de 1
#======4) Balancear para que soma seja 100% da pirâmide etária balanceada
#======5) aplicar os totais, deixar proporcional de forma que a soma total do mun = proj mun para o ano

#Última pirâmide disponível
rp_uano <- popmf[popmf$ano == uano  & popmf$cod_mun != "TOTAL",]

#faixas etárias
fxet <- unique(rp_uano$faixa_etaria)

#pop_uf pelo RIPSA, último ano disponivel
rp_uano_uf <- setDT(rp_uano)[,.(populacao = sum(populacao, na.rm = TRUE)),
                              by = .(uf = substr(cod_mun,1,2), faixa_etaria = faixa_etaria, ano = ano)]

#piram_uf pelo RIPSA, último ano disponível
prp_uano_uf <- setDT(rp_uano_uf)[,propuf := populacao/sum(populacao[faixa_etaria != "Total"], na.rm = TRUE), by = uf]

#piram_mun pelo RIPSA, último ano disponível
prp_uano_m <- setDT(rp_uano)[,prop := populacao/sum(populacao[faixa_etaria != "Total"], na.rm = TRUE), 
                      by = c("ano","cod_mun","Município")]

#Cálculo do 'Fator´  para último ano disponível de pirâmide etária

prp_uano_m <- (setDT(prp_uano_m %>% mutate(uf = substr(cod_mun,1,2)))[setDT(prp_uano_uf), 
                                                                      on = c("uf", "faixa_etaria"), 
                                                                      propuf := propuf])

prp_uano_m$fator <- with(prp_uano_m, (prop/propuf))

#ano fora fator
prp_uano_m$ano <- as.numeric(as.character(prp_uano_m$ano))

#seleciona apenas variáveis de interesse
prp_uano_m <- prp_uano_m %>% select(c(ano,uf,cod_mun,Município,faixa_etaria,fator))


#Pirâmide etária estadual dos ano correspondente
piram_uf_fxet <- function(a) {
  pop_uf_fxet_ano <- ibge_projpop_bruf(coluna = "Faixa Etária 1", periodo = a)
  #Equaliza faixas etárias
  nom_ufs <- as.character(pop_uf_fxet_ano[,1]) 
  pop_uf_fxet_ano <- pop_uf_fxet_ano[,c(-1,-length(pop_uf_fxet_ano))] %>%
    mutate(ultfaixa = rowSums(.[17:19])) %>%
    select(-17,-18,-19)
  
  #Nomeia corretamente a última faixa
  names(pop_uf_fxet_ano)[length(pop_uf_fxet_ano)] <- "80 anos ou mais"
  
  pr_uf_ano <- cbind(ano = a,nom_ufs,pop_uf_fxet_ano/rowSums(pop_uf_fxet_ano))
  pr_uf_ano <- pr_uf_ano %>% separate(nom_ufs,into = c("uf", "Unidade da Federação"), sep = "\\s", extra = "merge", fill = "left")
}

pr_uf_ano <- data.frame(matrix(ncol = 20, nrow = 0), stringsAsFactors = FALSE)

pr_mun_ano <- prp_uano_m

for (i in 1:length(anosel[anosel>uano])) {
  a <- i+uano
  pr_uf_ano <- rbind(pr_uf_ano,piram_uf_fxet(a))
  pr_mun_ano <- rbind(pr_mun_ano, prp_uano_m %>% mutate(ano = a)) 
}

pr_uf_ano <- pr_uf_ano %>% gather(faixa_etaria,prop_u,c(-1,-2,-3))

pr_mun_ano <- setDT(pr_mun_ano)[setDT(pr_uf_ano), on = c("uf", "faixa_etaria","ano"), 
                                prop_m := prop_u*fator ]
pr_mun_ano <- pr_mun_ano[!(is.na(pr_mun_ano$prop_m)),]

#4) Balanceamento para corrigir erros numéricos que levam a soma de fatores pouco diferente de 1
pr_mun_ano <- setDT(pr_mun_ano)[,prop_m := prop_m/sum(prop_m, na.rm = TRUE),by = c("cod_mun","ano")]

#pr_mun_ano$prop_m <- with(pr_mun_ano,prop_m/som_mun) 

pr_mun_ano <- pr_mun_ano %>% select(ano,uf,cod_mun,Município,faixa_etaria,prop_m)


#5) Multiplicar percentuais pela população total do município

# ----- dados populacionais básicos municipais mais recentes-----------------------------------------------------------

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
popmun_ano <- API_SIDRA(t, nivel = nivmun, variavel = vpop, inicio = uano+1, fim = afim)

#Filtra variáveis e compatibiliza código de município com formato DATASUS
popmun_ano <- popmun_ano %>% 
  mutate(cod_mun = substr(popmun_ano$`Município (Código)`,1,6)) %>%
  select(ano = `Ano (Código)`, cod_mun, populacao = Valor)


#5) Multiplicar percentuais pela população total

pop_mun_fext_ano <- setDT(pr_mun_ano)[setDT(popmun_ano), 
                                      on = c("cod_mun","ano"), 
                                      populacao := i.populacao * prop_m]



#6) Juntar populações oficialmente estimadas com adições deste script (3 a 5)   
popmf <- rbind(popmf,pop_mun_fext_ano[,c(1,3:5,7)])

#7) Reagrupar as colunas de 0 a 4 e de 5 a 9 para 0 a 3 (creche) , de 4 a 5 (pré-escola) e de 6 a 9, datasus
#DATASUS fornece dados de nascidos e óbitos - estes de 0 a 1 e de 1 a 4
#Tábuas de mortalidade do IBGE fornecem critério para dividir mortes de 1 a 4 entre 1 a 3 e 3 a 4

muni_cods <- unique(popmf$cod_mun[!(is.na(popmf$cod_mun))])


#Nascidos vivos por ano e por município  
nascidos <- lapply((anoin-6):afim,sinasc_nv_mun, linha = "Município", coluna = "Não ativa", conteudo = 1)

#OBS ao captar os obitos de uma vez, o que ocorre é a ausência de municípios sem registro de óbitos
obitos0a1 <- lapply((anoin-6):afim,sim_inf10_mun, linha = "Município", coluna = "Não ativa", conteudo = 1)  


obitos1a4 <- lapply((anoin-6):afim,sim_obt10_mun, linha = "Município", 
                    coluna = "Não ativa", conteudo = 1, faixa_etaria = "1 a 4 anos")  
obitos5a9 <- lapply((anoin-6):afim,sim_obt10_mun, linha = "Município", 
                    coluna = "Não ativa", conteudo = 1, faixa_etaria = "5 a 9 anos")  

#Passos restantes - preparação dos dados
#1) Adicionar zeros para municípios faltantes em cada um dos objetos
#2) Consolidar as sublistas em uma mesma lista, adicionando o ano e o tipo de informação
#3) Consolidar os diferentes objetos em uma grande tabela só sus_nasc_ob


#Preparação de indicadores a partir da tábua de mortalidade ibge
#1) Recuperar tabelão
#2) Calcular proporção de mortes de 1 a 2 no contingente de 1 a 4 para cada ano
#3) Calcular 

#Cálculo dos seguintes contingentes:

# 1) População de 3 a 4 anos em ano x 
#######  Ex. ano 2012 - 
######1) pegar nascidos ano -3 - em 2009 (3 a 4 em 2012) - 
######2) média de óbitos 0 a 1 ano- 3 e ano - 22009 e 2010






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

