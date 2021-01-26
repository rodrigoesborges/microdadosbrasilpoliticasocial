  
#install.packages("devtools")
#install.packages("stringi") 
require(data.table)
require(datasus)
library(dplyr)
require(tidyverse)
devtools::install_github("rodrigoesborges/microdadosBrasil")
require(microdadosBrasil)
#pasta para download dos dados brutos (quando muito grandes)
dadoslocais <- "~/RLocalData/censoescolar"
#Ler dados de estimativa populacional
source("est_pop_m_0a6.R")
source("R/zr_fun.R")

#Período de Análise
anosel <-  seq(2012,2017,1)

#Dados a selecionar de matrícula
#censoinfos <- c("NU_MES","NU_IDADE_REFERENCIA","NU_IDADE","TP_ETAPA_ENSINO","CO_MUNICIPIO", "CO_UF", "TP_DEPENDENCIA") - Ano de 2017

##Dados a selecionar de escola/turma
#censoinfoesc <- c("CO_UF", "CO_MUNICIPIO", "TP_DEPENDENCIA", "IN_COMUM_CRECHE", "IN_COMUM_PRE", "IN_ESP_EXCLUSIVA_CRECHE", "IN_ESP_EXCLUSIVA_PRE")

#Dados a selecionar de turma

#censoinfoturma <- c("ID_TURMA","NU_MATRICULAS","TP_ETAPA_ENSINO", "CO_UF","CO_MUNICIPIO")

#Harmonização manual de variáveis
cnhar <- data.frame("ano" = seq(2012,2017,1),
                        "idade" = c("NU_ANO",rep("NUM_IDADE",2),rep("NU_IDADE",3)),
                        "idaderef" = c("NUM_IDADE",rep("NUM_IDADE_REFERENCIA",2),rep("NU_IDADE_REFERENCIA",3)),
                        "ensino" = c(rep("FK_COD_ETAPA_ENSINO",3),rep("TP_ETAPA_ENSINO",3)),
                        "municod" = c(rep("COD_MUNICIPIO_ESCOLA",3),rep("CO_MUNICIPIO",3)), 
                        "ufcod" = c(rep("FK_COD_ESTADO_ESCOLA",3),rep("CO_UF",3)),
                        "turma" = c(rep("PK_COD_TURMA",3),rep("ID_TURMA",3)),
                        "nmat" = c(rep("NUM_MATRICULAS",3),rep("NU_MATRICULAS",3)),
                        "muniet" = c(rep("FK_COD_MUNICIPIO",3),rep("CO_MUNICIPIO",3)),
                        "depadm" = c(rep("ID_DEPENDENCIA_ADM_ESC",3),rep("TP_DEPENDENCIA",3)),
                        "crec" = c(rep("ID_REG_INFANTIL_CRECHE",3),rep("IN_COMUM_CRECHE",3)),
                        "cres" = c(rep("ID_ESP_INFANTIL_CRECHE",3),rep("IN_ESP_EXCLUSIVA_CRECHE",3)),
                        "prec" = c(rep("ID_REG_INFANTIL_PREESCOLA",3),rep("IN_COMUM_PRE",3)),
                        "pres" = c(rep("ID_ESP_INFANTIL_PREESCOLA",3),rep("IN_ESP_EXCLUSIVA_PRE",3)),
                        stringsAsFactors = FALSE)

##Atenção - NU_IDADE_REFERENCIA inexistente para anos <2013 , calculado a partir de NU_ANO (selecionado no lugar) e NU_IDADE 

#Criação da lista de dados de matrícula, escola e turma de acordo com cada ano para filtragem dos dados
info <- list()
infoesc <- list()
infotur <- list()

for (i in 1:length(cnhar$ano)) {
  info[[i]] <-  c("NU_MES",cnhar$idaderef[i],cnhar$idade[i],cnhar$ensino[i],cnhar$municod[i], cnhar$ufcod[i],cnhar$depadm[i])
  infoesc[[i]] <- c(gsub("_ESCOLA","",cnhar$ufcod[i]), cnhar$muniet[i],
                    gsub("_ESC","",cnhar$depadm[i]),cnhar$crec[i],cnhar$cres[i],
                    cnhar$prec[i],cnhar$pres[i] )
  infotur[[i]] <- c(cnhar$turma[i],cnhar$nmat[i],cnhar$ensino[i],gsub("_ESCOLA","",cnhar$ufcod[i]),cnhar$muniet[i])
}





#Função para preparar dados de matrícula e salvar como rds
tab_prep <- function(periodo,dt,info,tipo) {
  
  for (i in 1:length(periodo)) {
    #Baixar dados e descompactar caso não existam na pasta de dados locais
    #download_sourceData("CensoEscolar",i, unzip = T, root_path = dadoslocais)
    #####Descompactar - contorna problema em pacote microdados Brasil - Problema descompactar sub-arquivos 
    #dcpre <- paste0(dt,"/micro_censo_escolar_",periodo[i])
    #dtdir <- paste0(dcpre,"/",periodo[i],"/DADOS")
    #unzip(paste0(dcpre,".zip"),exdir = dcpre, overwrite = F)
    #dproj <- getwd()
    #setwd(dtdir)
    #system('unrar e -r -o- "*.rar"')
    #setwd(dproj)
    ######
    ##Definição de nomes
    vn <- tipo
    arq <- paste0("data/",periodo[i],"-",vn,"-subdc.rds")
    print(info[[i]])
    #Ler dados de matrícula do censo
    censoed <- read_CensoEscolar(ft = vn,i = periodo[i], vars_subset = info[[i]], root_path = dt, harmonize_varnames = TRUE)
    #Salva em arquivo rds
    #Compatibiliza nomes das colunas
    ##Atenção - NU_IDADE_REFERENCIA inexistente para anos <2013 , calculado a partir de NU_ANO e NU_IDADE 
#    if (anosel[i] < 2013) {
#       matriculas[matriculas$NU_MES > 5,2] <- matriculas[matriculas$NU_MES > 5,3]-1
    #       matriculas[matriculas$NU_MES < 6,2] <- matriculas[matriculas$NU_MES < 6,3]
    #    }
    names(censoed) <- info[[length(info)]]
    saveRDS(censoed,arq, compress = "gzip")
  }
}



#função para carregar dados do censo

esc_carg <- function(ano, dt) {
  matriculas <- readRDS(paste0("data/",ano,"-matricula-subdc.rds"))
  escolas <- readRDS(paste0("data/",ano,"-escola-subdc.rds"))
  turmas <- readRDS(paste0("data/",ano,"-turma-subdc.rds"))
  microd <- list("matriculas" = matriculas,"escolas" = escolas,"turmas" = turmas)
  microd
}

tab_prep(anosel,dadoslocais,info,"matricula")
tab_prep(anosel,dadoslocais,infoesc,"escola")
tab_prep(anosel,dadoslocais,infotur,"turma")


#Inicializar tabela de resultados
ind_nomes = c("Tx de Cobertura Bruta - Creche",
              "Tx de Cobertura Efetiva - Creche",
              "Tx de Cobertura - Pré-escola",
              "Tx de Cobertura efetiva  - Pré-escola", 
              "Creches e Pre-escolas / 1.000 crianças de 0 a 6",
              "Creches e Pré-escolas públicas (% do total)",
              "Nº de alunos por turma" ,
              "Ano")

tab_ecd_ed <- data.frame(matrix(ncol = 1+length(ind_nomes), nrow=0), stringsAsFactors = FALSE)
names(tab_ecd_ed) <- c("local",ind_nomes)

tab_bruta_ecd_ed <- data.frame(matrix(ncol = 11, nrow=0), stringsAsFactors = FALSE)
nbruta <- c("municipio" ,
            "matr_creche" ,
            "matr_creche_mref" ,
            "matr_presc" ,
            "matr_4e5_ref" ,
            "pop_mun_0a4" ,
            "pop_adic_3_a_4" ,
            "pop_0a3",
            "pop_total_4_a_6" ,
            "pop_adic_0_a_6",
            "ano")


#Número de matrículas em Creche / população de crianças na faixa etária
#A) Indicador municipal
#Define municípios dos quais retirar informação - caso de subseleção
#municipios <- c("Vitória", "Serra", "Cariacica", "Campo Grande", "Vila Velha", "São Mateus")
#& grepl(paste(municipios,collapse="|"),municodigos$municipio)
# Define todos os municípios do ES
ufmat <- "32"
municodigos <- read.csv2("data/tabcodigosmunibgedatasus.csv", stringsAsFactors = FALSE)[,-1]

municodigos <- municodigos[grepl(paste0("^",ufmat),municodigos$codigomun),]

#Funcao para indicadores de educacao


ind_ecd_ed <- function (a = 2017, tbase = popm_inf) {
  #Carrega esc_carg do ano
  m <- "CO_MUNICIPIO"
  microd <- esc_carg(a,dadoslocais)
  matriculas <- microd$matriculas
  escolas <- microd$escolas
  turmas <- microd$turmas
  
  #Carrega subset correspondente de tbase
  tbase <- tbase[tbase$ano == a,]
  
  #Calculo dos resultados brutos
  #A) n_matriculas_creche por municipio
  
  #aggregate(x ~ cod_mun, data = mats, NROW)
  m_1_mun <- matriculas %>% filter(TP_ETAPA_ENSINO == 1) %>% count(CO_MUNICIPIO)
  
  #B) n_matriculas_creche - criancas da idade certa
  m_1_mun_ef <- matriculas %>% filter(TP_ETAPA_ENSINO == 1 & NU_IDADE_REFERENCIA < 4) %>% count(CO_MUNICIPIO)
  
  #C) n_matriculas_pre_escola por municipio

  m_2_mun <- matriculas %>% filter(TP_ETAPA_ENSINO == 2) %>% count(CO_MUNICIPIO)

  #D) n_matriculas_pre-escola - criancas da idade certa
  m_2_mun_ef <- matriculas %>% filter(TP_ETAPA_ENSINO == 2 & NU_IDADE_REFERENCIA > 3 & NU_IDADE_REFERENCIA < 7) %>% count(CO_MUNICIPIO)
  
  #E) Alunos / sala creche por município
  #A pensar futuramente - há municípios com matrículas sem sala
  
  s_1_mun <- turmas[TP_ETAPA_ENSINO == 1,] %>% count(CO_MUNICIPIO)
  
  al_1_sala_mun <- m_1_mun %>% inner_join(s_1_mun, by = m) %>% mutate(al_sala = n.x/n.y)
  
  
  #F) Alunos / sala pre escola por município
  
  s_2_mun <- turmas[TP_ETAPA_ENSINO == 2,] %>% count(CO_MUNICIPIO)
  
  al_2_sala_mun <- m_2_mun %>% inner_join(s_2_mun, by = m) %>% mutate(al_sala = n.x/n.y)
  
  
  #G) Creches por município
 e_1_mun <- escolas[IN_COMUM_CRECHE == 1 | IN_ESP_EXCLUSIVA_CRECHE == 1,] %>% count(CO_MUNICIPIO)
  
  #H) Pré-escolas por município
 e_2_mun <- escolas[IN_COMUM_PRE == 1 | IN_ESP_EXCLUSIVA_PRE == 1,] %>% count(CO_MUNICIPIO)
 
  #I) Creches públicas por município

 e_1_pub <- escolas[(IN_COMUM_CRECHE == 1 | IN_ESP_EXCLUSIVA_CRECHE == 1) & TP_DEPENDENCIA != 4,] %>% 
   count(CO_MUNICIPIO)
 
  
  #J) Pré-escolas públicas por município
 
  e_2_pub <-  escolas[(IN_COMUM_PRE == 1 | IN_ESP_EXCLUSIVA_PRE == 1) & TP_DEPENDENCIA != 4,] %>% 
    count(CO_MUNICIPIO)
  
  #K) Matrículas em creches públicas / Matrículas totais
  
  m_c_pub <- matriculas %>% filter(TP_ETAPA_ENSINO == 1 & NU_IDADE_REFERENCIA < 4 & TP_DEPENDENCIA != 4) %>% count(CO_MUNICIPIO)
  
  #L) Matrículas em pré-escolas públicas / Matrículas totais
  m_p_pub <- matriculas %>% filter(TP_ETAPA_ENSINO == 2 & NU_IDADE_REFERENCIA > 3 & NU_IDADE_REFERENCIA < 7 & TP_DEPENDENCIA != 4) %>%
    count(CO_MUNICIPIO)
  #taxa de cobertura bruta equivalente a:
  #https://observatoriocrianca.org.br/cenario-infancia/temas/educacao-infantil/1081-taxa-de-cobertura-em-creche?filters=1,77;3209,77;21,77
  
  #juntar tudo como colunas
  
  r_bruto <- m_1_mun %>% full_join(m_1_mun_ef, by = m) %>% full_join(m_2_mun, by = m) %>% full_join(m_2_mun_ef, by = m) %>% 
    full_join(al_1_sala_mun[,c(1,4)], by = m) %>% full_join(al_2_sala_mun[,c(1,4)], by = m) %>% full_join(e_1_mun, by = m) %>% 
    full_join(e_1_pub, by = m)%>% full_join(e_2_mun, by = m) %>% full_join(e_2_pub, by = m) %>% full_join(m_c_pub, by = m) %>%
    full_join(m_p_pub, by = m)
  
  
  r_bruto$CO_MUNICIPIO <- substr(r_bruto$CO_MUNICIPIO,1,6)
  
  names(r_bruto) <- c("cod_mun","m_1_mun","m_1_mun_ef","m_2_mun","m_2_mun_ef","al_1_sala_mun","al_2_sala_mun",
                      "n_creches","n_creches_pub","n_pre","n_pre_pub","matpub_c","matpub_p")
  
  r_bruto <- tbase %>% full_join(r_bruto, by = "cod_mun")
  
    #INDICADORES
  
  #1) Taxa de Cobertura Bruta e Efetiva - Creche e Pré-escola , alunos por turma creche e pre escola, n_creches_pub/total, n_pre_pub/total,
  #n_creches/1.000 hab da idade, n_pre/1.000 hab da idade
  ecd_ed_indicadores <- r_bruto %>% transmute("cod_mun" = cod_mun, "Município" = Município, ano = a,
                                      tx_cobertura_creche_bruta = (m_1_mun)/(`0 a 3 anos`), 
                                      tx_cobertura_creche_efetiva = m_1_mun_ef/`0 a 3 anos`,
                                      tx_cobertura_pre_bruta = m_2_mun/`4 a 6 anos`, 
                                      `tx_cobertura_pre_efetiva` = m_2_mun_ef/`4 a 6 anos`, 
                                      `alunos de creche por sala` = al_1_sala_mun,
                                      `alunos de pre por sala` = al_2_sala_mun,
                                      `prop. de creches pub` = n_creches_pub/n_creches,
                                      `prop. de pre-escolas pub` = n_pre_pub/n_pre,
                                      `creches por 1.000 crianças` = n_creches*1000/`0 a 3 anos`,
                                    `pré-escolas por 1.000 crianças` = n_pre*1000/`4 a 6 anos`,
                                    `matrículas públicas em creche do total` = matpub_c/m_1_mun_ef,
                                    `matrículas públicas em pré-escolas do total` = matpub_p/m_2_mun_ef)

  ecd_ed_indicadores
}

tab_ecd_ed_indicadores <- rbindlist(lapply(anosel,ind_ecd_ed))

tab_ecd_ed_indicadores <- tab_ecd_ed_indicadores %>% pivot_longer(c(-1,-2,-3),"indicador",values_to = "valor")

write.csv2(tab_ecd_ed_indicadores,"data/2012-2017-tab_ecd_ed.csv")
#  - Gasto público em educação com creches e pŕe-escolas por município, ES e BR (absoluto e per capita)
#           -  Não há no Censo Escolar - procurar no #portal da transparencia(?)
#           - há algum lugar que junte dados municipais?

  