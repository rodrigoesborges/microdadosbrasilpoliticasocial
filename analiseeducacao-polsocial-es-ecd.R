
#install.packages("devtools")
#install.packages("stringi") 
require(data.table)
require(datasus)
library(dplyr)
#require(tidyverse)
devtools::install_github("rodrigoesborges/microdadosBrasil")
require(microdadosBrasil)
#pasta para download dos dados brutos (quando muito grandes)
dadoslocais <- "~/RLocalData/censoescolar"
#Ler dados de estimativa populacional
source("estimativas-populacao-ibge.R")
source("R/zr_fun.R")

#Período de Análise
anosel <-  seq(2016,2017,1)

#Dados a selecionar de matrícula
censoinfos <- c("NU_MES","NU_IDADE_REFERENCIA","NU_IDADE","TP_ETAPA_ENSINO","CO_MUNICIPIO", "CO_UF")

##Dados a selecionar de escola/turma
censoinfoesc <- c("CO_UF", "CO_MUNICIPIO", "TP_DEPENDENCIA", 
                  "IN_COMUM_CRECHE", "IN_COMUM_PRE",
                  "IN_ESP_EXCLUSIVA_CRECHE", "IN_ESP_EXCLUSIVA_PRE")

#Dados a selecionar de turma

censoinfoturma <- c("ID_TURMA","NU_MATRICULAS","TP_ETAPA_ENSINO",
                    "CO_UF","CO_MUNICIPIO")


#Função para preparar dados de matrícula e salvar como rds
mat_prep <- function(periodo,dt,info) {
  
  for (i in 1:length(periodo)) {
    #Baixar dados e descompactar caso não existam na pasta de dados locais
    #download_sourceData("CensoEscolar",i, unzip = T, root_path = dadoslocais)
    #Descompactar - contorna problema em pacote microdados Brasil - Problema descompactar sub-arquivos 
    dcpre <- paste0(dt,"/micro_censo_escolar_",periodo[i])
    dtdir <- paste0(dcpre,"/",periodo[i],"/DADOS")
    unzip(paste0(dcpre,".zip"),exdir = dcpre, overwrite = F)
    dproj <- getwd()
    setwd(dtdir)
    system('unrar e -r -o- "*.rar"')
    setwd(dproj)
    #Definição de nomes
    vn <- "matricula"
    arq <- paste0("data/",vn,periodo[i],"subdc.rds")
    
    #Ler dados de matrícula do censo
    censoed <- read_CensoEscolar(ft = vn,i = periodo[i], vars_subset = info, root_path = dt)
    #Salva em arquivo rds
    write_rds(censoed,arq, compress = "gz")
  }
}


mat_prep(anosel,dadoslocais,censoinfos)

#função para carregar dados do censo

esc_carg <- function(ano, esc, turma, dt) {
  matriculas <- readRDS(paste0("data/matricula",ano,"subdc.rds"))
  escolas <- read_CensoEscolar(ft = "escola",i = ano, vars_subset = esc, root_path = dt)
  turmas <- read_CensoEscolar(ft = "turma",i = ano, vars_subset = turma, root_path = dt)
  microd <- list(matriculas,escolas,turmas)
}

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

for (j in 1:length(anosel)) {
  
  #carregar dados correspondentes
  esc_carg(anosel[j],censoinfoesc,censoinfoturma,dadoslocais)
  matriculas <- microd$matriculas
  escolas <- microd$escolas
  turmas <- microd$turmas
  ####vetores de anos para estimar população de:
  ### 0 a 4:
  anos_0a4 = seq(anosel[j]-3,anosel[j],1)
  ### 
#fazer para seleção de municípios
for (i in 1:length(municodigos$codigomun)) {
  mat_m17 <- matriculas[CO_MUNICIPIO == municodigos[i,1],-6]
  crechemun17 <- nrow(mat_m17[TP_ETAPA_ENSINO==1,1])
  prescmun17 <- nrow(mat_m17[TP_ETAPA_ENSINO == 2,])
    #para cobertura efetiva, verificar criancas da
  #idade 0 a 3 matriculadas
  #Há tanto em creches como em Pré-escola
  #(adiantados)
  
  matr_0a3_m <- nrow(mat_m17[ NU_IDADE < 4,1])

  #matr_0a3_mref2 <- matr_0a3_m +
  #nrow(mat_m17[NU_IDADE == 4 & NU_MES == 6,])
  
  matr_0a3_mref <- nrow(mat_m17[ NU_IDADE_REFERENCIA < 4,1])

  matr_0a3_mjun <- matr_0a3_mref - nrow(mat_m17[NU_IDADE == 4 & 
                                                  NU_MES == 6,])

  popmtotalidade <- es17popidades[es17popidades$CodMunicipio ==
                                    municodigos[i,3],3]

#indice cobertura com pop 0 a 4  
coberturacreche <- crechemun17/popmtotalidade
#adiciona linha, com nome do município e indicador
#nascidos vivos residência da mãe datasus

#nascidos vivos de 0 a 4
nv_0a4 <- zr(sinasc_nv_mun(periodo = anos_0a4, municipio = municodigos[i,3])[2,2])
###retirado +sinasc_pnv_mun(municipio = municodigos[i,3])[,2] (dados preliminares já def)

#obitos acumulados de 0 a 1 anos para os mesmos anos
#teste
ob10_0a4 <- zr(sim_inf10_mun(periodo = anos_0a4, municipio = municodigos[i,3])[2,2])
#retirado dado preliminar, adicionado dado definitivo
#+sim_pinf10_mun(municipio = municodigos[i,3])[,2]

#acrescimopop sem contar migrações
popadic_0a4 <- nv_0a4 - ob10_0a4

##mesmo que o anterior de 0 a 3
nv_0a3 <- zr(sinasc_nv_mun(periodo = anos_0a3, municipio = municodigos[i,3])[2,2])
#retirados acima dados preliminares
print(paste("População nascidos vivos de 0 a 3 é ",nv_0a3," no município ",municodigos$municipio[i]))
#obitos acumulados de 0 a 1 anos para os mesmos anos
ob10_0a3 <- zr(sim_inf10_mun(periodo = anos_0a3, municipio = municodigos[i,3])[2,2])
#retiradas informações preliminares - não disponíveis atualmente
#+sim_pinf10_mun(municipio = municodigos[i,3])[,2]

#acrescimopop sem contar migrações nem mortes de 1 a 4
popadic_0a3 <- nv_0a3 - ob10_0a3
#vamos só retirar os que estariam entre 3 e 4 do dado "firme" de população total 
#de 0 a 4 anos?
popadic_3a4 <- popadic_0a4 - popadic_0a3

popmtotal0a3 <- popmtotalidade - popadic_3a4

#taxa de cobertura bruta equivalente a:
#https://observatoriocrianca.org.br/cenario-infancia/temas/educacao-infantil/1081-taxa-de-cobertura-em-creche?filters=1,77;3209,77;21,77

#Indicador 2 - col 3
cob_creche_bruta_m_17 <- crechemun17/popmtotal0a3
#tab_ecd_ed[i,3] <- cob_creche_bruta_m_17

cob_efet_0a3m <- matr_0a3_m/popmtotal0a3

#Indicador 3 - col 4
cob_efet_0a3mref <- matr_0a3_mref/popmtotal0a3
#tab_ecd_ed[i,4] <- cob_efet_0a3mref

##### Pré - Escola ##############
######

nv_0a6 <- zr(sinasc_nv_mun(periodo = anos_0a6, municipio = municodigos[i,3])[2,2])
#retirado preliminares +sinasc_pnv_mun(municipio = municodigos[i,3])[,2]
#obitos acumulados de 0 a 1 anos para os mesmos anos
ob10_0a6 <- zr(sim_inf10_mun(periodo = anos_0a6, municipio = municodigos[i,3])[2,2])
#retirado preliminares +sim_pinf10_mun(municipio = municodigos[i,3])[,2]
#acrescimopop sem contar migrações
popadic_0a6 <- nv_0a6 - ob10_0a6

#vamos só somar os que estariam entre 4 e 5 do dado "firme" de população total 
#de 0 a 4 anos?
popadic_4a6 <- popadic_0a6 - popadic_0a4
popmtotal4a6 <- popadic_4a6

popmtotal0a6 <- popmtotalidade + popadic_4a6

print(popmtotal0a6)
#a pop total de 4 a 6 para observatorio da criança VIX é ~ 8.320 (migração? pq?)

#taxa de cobertura bruta que deveria ser equivalente a:
# observatoriodacrianca
coberturapresc <- prescmun17/popmtotal4a6

#para cobertura efetiva, verificar criancas da idade 4 e 5 matriculadas
matr_4e5_m <- nrow(mat_m17 %>% filter (NU_IDADE >= 4 & NU_IDADE < 6 & 
                                         TP_ETAPA_ENSINO == 2))

matr_4e5_mref <- nrow(mat_m17 %>% filter (NU_IDADE_REFERENCIA >= 4 & 
                                            NU_IDADE_REFERENCIA < 6 &
                                            TP_ETAPA_ENSINO == 2))



cob_presc_efet_4e5m <- matr_4e5_m/popmtotal4a6


cob_efet_4e5mref <- matr_4e5_mref/popmtotal4a6

### Creches e pre escolas / 1.000 crianças de 0 a 6
escm17 <- escolas[(CO_MUNICIPIO ==  municodigos[i,1] ) & 
                        (IN_COMUM_CRECHE == 1 | IN_COMUM_PRE == 1 | 
                           IN_ESP_EXCLUSIVA_CRECHE == 1 | IN_ESP_EXCLUSIVA_PRE == 1),]

escolas_m_mil_17 <- nrow(escm17)*1000/popmtotal0a6

#N. de creches e turmas de pré-escolas públicas (% total) 
pubtotal <- nrow(escm17[TP_DEPENDENCIA != 4])/nrow(escm17)

#### Nº de alunos por turma / Nº alunos por professor

censoinfoturma <- c("ID_TURMA","NU_MATRICULAS","TP_ETAPA_ENSINO",
                    "CO_UF","CO_MUNICIPIO")
turmas_m_17 <- turmas[CO_MUNICIPIO == municodigos[i,1] & TP_ETAPA_ENSINO <= 2,
                          1:2 ]
alturma_m_17 <- sum(turmas_m_17$NU_MATRICULAS)/length(unique(turmas_m_17$ID_TURMA))


tab_bruta_ecd_ed <- rbind(tab_bruta_ecd_ed, c(municodigos$municipio[i],crechemun17,matr_0a3_mref,prescmun17,matr_4e5_mref,popmtotalidade,popadic_3a4,popmtotal0a3,popmtotal4a6,popadic_0a6,anosel[j]), stringsAsFactors = FALSE)
names(tab_bruta_ecd_ed) <- nbruta
ind_ed_res <- c(municodigos$municipio[i],cob_creche_bruta_m_17,
                cob_efet_0a3mref,coberturapresc,cob_efet_4e5mref,
                escolas_m_mil_17,pubtotal,alturma_m_17,anosel[j])
tab_ecd_ed <- rbind(tab_ecd_ed,ind_ed_res, stringsAsFactors = FALSE)
names(tab_ecd_ed) <- c("local",ind_nomes)
}



  #para o ES
  mat_uf_17 <- matriculas[CO_UF==32,-6]

crechees17 <- nrow(mat_uf_17[TP_ETAPA_ENSINO==1,1])

#matriculados em creche menores de 4 anos
matr_0a3_uf <- nrow(mat_uf_17[NU_IDADE < 4,1])
matr_0a3_ufref <- nrow(mat_uf_17[NU_IDADE_REFERENCIA < 4,1])
matr_0a3_ufjun <- matr_0a3_ufref -nrow(mat_uf_17[NU_IDADE == 4 & NU_MES == 6,])


popestotalecd <- sum(es17popidades$`0 a 4 anos`)

#popadic ES de 0 a 3 anos estimada por nascimentos e óbitos

##Calculando pop. adicional 0 a 4 por nascidos e óbitos infantis
nves_0a4 <- zr(sinasc_nv_bruf(periodo = anos_0a4, unidade_da_federacao = "Espírito Santo")[2,2])
               #SUPOSTAMENTE DADOS NÃO MAIS PRELIMINARES PARA 2017 - DESCARTADO - +sinasc_pnv_bruf(unidade_da_federacao = "Espírito Santo")[2,2])

#obitos acumulados de 0 a 1 anos para os mesmos anos
ob10es_0a4 <- zr(sim_inf10_bruf(periodo = anos_0a4, unidade_da_federacao = "Espírito Santo")[2,2])
#acrescimopop sem contar migrações nem mortes de 1 a 4
popadices_0a4 <- nves_0a4 - ob10es_0a4

##mesmo que o anterior de 0 a 3
nves_0a3 <- zr(sinasc_nv_bruf(anos_0a3, unidade_da_federacao = "Espírito Santo")[2,2])
               #SUPOSTAMENTE.... ver acima comentário +sinasc_pnv_bruf(unidade_da_federacao = "Espírito Santo")[2,2])

#obitos acumulados de 0 a 1 anos para os mesmos anos
ob10es_0a3 <- zr(sim_inf10_bruf(periodo = anos_0a3, unidade_da_federacao = "Espírito Santo")[2,2])
#acrescimopop sem contar migrações
popadices_0a3 <- nves_0a3 - ob10es_0a3

popadices_3a4 <- popadices_0a4 - popadices_0a3

popes0a3 <- popestotalecd - popadices_3a4

coberturaces <- crechees17/popestotalecd
#Taxa de cobertura bruta de creche próxima a
# observatoriocrianca.org.br
cobcrechebrutaes17 <- crechees17/popes0a3

cob_efet_0a3uf <- matr_0a3_uf/popes0a3
cob_efet_0a3ufref <- matr_0a3_ufref/popes0a3
#Taxa de cobertura líquida próxima ao mesmo indicador anterior:
cob_efet_0a3ufjun <- matr_0a3_ufjun/popes0a3


#Número de matrículas em Pré-escola / população de crianças na faixa etária
#ES

#nmatriculados em etapa prescola
presces17 <- nrow(mat_uf_17[TP_ETAPA_ENSINO == 2])

#matriculados que tem entre 4 e 5 anos
matr_4e5_uf <- nrow(mat_uf_17 %>% filter(NU_IDADE >= 4 &
                                               NU_IDADE < 6 & 
                                              TP_ETAPA_ENSINO == 2))

matr_4e5_ufref <- nrow(mat_uf_17 %>% filter(NU_IDADE_REFERENCIA >= 4 &
                                              NU_IDADE_REFERENCIA < 6 & 
                                              TP_ETAPA_ENSINO == 2))
### OBSCRIANCA - FECHARAM PARA OS DA IDADE MATRICULADOS EM PREESCOLA -TP ACIM

#popadic ES de 0 a 6 anos estimada por nascimentos e óbitos

##Calculando pop. adicional 0 a 6 por nascidos e óbitos infantis
nves_0a6 <- zr(sinasc_nv_bruf(periodo = anos_0a6, unidade_da_federacao = "Espírito Santo")[2,2])
               #SUPOSTAMENTE...+sinasc_pnv_bruf(unidade_da_federacao = "Espírito Santo")[2,2])

#obitos acumulados de 0 a 1 anos para os mesmos anos
ob10es_0a6 <- zr(sim_inf10_bruf(periodo = anos_0a6, unidade_da_federacao = "Espírito Santo")[2,2])
#acrescimopop sem contar migrações nem mortes de 1 a 4
popadices_0a6 <- nves_0a6 - ob10es_0a6


popadices_4a6 <- popadices_0a6 - popadices_0a4
popes4a6 <- popadices_4a6

#Taxa de cobertura bruta de presc próxima a
# observatoriocrianca.org.br
cobprescbrutaes17 <- presces17/popes4a6

cob_efet_4a6uf <- matr_4e5_uf/popes4a6
cob_efet_4a6ufref <- matr_4e5_ufref/popes4a6
#Taxa de cobertura líquida próxima ao mesmo indicador anterior:
#pop estimada 4 e 5 ES observatorio crianca ~ 113.120

#Nº de creches e turmas de pré-escolas no ES / 1.000 crianças 

esces17 <- escolas[(CO_UF ==  32 ) & 
                        (IN_COMUM_CRECHE == 1 | IN_COMUM_PRE == 1 | 
                           IN_ESP_EXCLUSIVA_CRECHE == 1 | IN_ESP_EXCLUSIVA_PRE == 1),]

escolas_es_mil_17 <- nrow(esces17)*1000/(popestotalecd+popes4a6)

#N. de creches e turmas de pré-escolas públicas (% total) 
pubtotales <- nrow(esces17[TP_DEPENDENCIA != 4])/nrow(esces17)

#Nº de alunos por turma / Nº alunos por professor

censoinfoturma <- c("ID_TURMA","NU_MATRICULAS","TP_ETAPA_ENSINO",
                    "CO_UF","CO_MUNICIPIO")
turmas_es_17 <- turmas[CO_UF == 32 & TP_ETAPA_ENSINO <= 2,
                          1:2 ]
alturma_es_17 <- sum(turmas_es_17$NU_MATRICULAS)/length(unique(turmas_es_17$ID_TURMA))

ed_res_es <- c("Espírito Santo",cobcrechebrutaes17,cob_efet_0a3ufref,
                   cobprescbrutaes17,cob_efet_4a6ufref,
                escolas_es_mil_17,pubtotales,alturma_es_17,anosel[j])

tab_ecd_ed <- rbind(tab_ecd_ed,ed_res_es, stringsAsFactors = FALSE)
names(tab_ecd_ed) <- c("local",ind_nomes)
write.csv2(tab_ecd_ed,'data/tab_ecd_ed.csv')

tab_bruta_ecd_ed <- rbind(tab_bruta_ecd_ed[1:78,], c("Espírito Santo",
                                              crechees17,
                                              matr_0a3_ufref,
                                              presces17,
                                              matr_4e5_ufref,
                                              sum(es17popidades$`0 a 4 anos`),
                                              popadices_3a4,
                                              popes0a3,
                                              popes4a6,
                                              popadices_0a6,
                                              anosel[j]),
                          stringsAsFactors = FALSE)
names(tab_bruta_ecd_ed) <- nbruta
write.csv2(tab_bruta_ecd_ed, 'data/tab_bruta_ecd_ed.csv')

#  - Gasto público em educação com creches e pŕe-escolas por município, ES e BR (absoluto e per capita)
#           -  Não há no Censo Escolar - procurar no #portal da transparencia(?)
#           - há algum lugar que junte dados municipais?
}
