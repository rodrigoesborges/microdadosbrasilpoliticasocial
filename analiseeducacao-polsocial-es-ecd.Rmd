
#install.packages("devtools")
#install.packages("stringi") 
require(data.table)
require(tidyverse)
devtools::install_github("rodrigoesborges/microdadosBrasil")
require(microdadosBrasil)
#pasta para download dos dados brutos (quando muito grandes)
dadoslocais <- "~/RLocalData/censoescolar"
#Baixar dados caso não existam
#download_sourceData("CensoEscolar",2016, unzip = F, root_path = dadoslocais)
#read_CensoEscolar(ft = 'asas',i = 2017) #para ver os disponíveis
#Ler dados de estimativa populacional
source("estimativas-populacao-ibge.R")

#Nomes das variáveis - Anexos - Anexo 1 tabelas auxiliares - Dicionarios...
censoinfos <- c("NU_MES","NU_IDADE_REFERENCIA","NU_IDADE","TP_ETAPA_ENSINO","CO_MUNICIPIO", "CO_UF")

#Ler dados do Censo
# matricula2017 <- read_CensoEscolar(ft = "matricula",i = 2017, vars_subset =
#                                      censoinfos, root_path = dadoslocais)
#write_rds(matricula2017,"data/matricula2017subc.rds", compress = "gz")
#Carregar dados do censo
matricula2017 <- readRDS("data/matricula2017subc.rds")


#Inicializar tabela de resultados
ind_nomes = c("Tx de Cobertura Bruta - Creche",
              "Tx de Cobertura Efetiva - Creche",
              "Tx de Cobertura - Pré-escola",
              "Tx de Cobertura efetiva  - Pré-escola", 
              "Nº de alunos por turma" )

tab_ecd_ed <- data.frame(matrix(ncol = 1+length(ind_nomes), nrow=0), stringsAsFactors = FALSE)
names(tab_ecd_ed) <- c("local",ind_nomes)


#Número de matrículas em Creche / população de crianças na faixa etária
#A) Indicador municipal
municipios <- c("Vitória", "Serra", "Cariacica", "Campo Grande", "Vila Velha", "São Mateus")
ufmat <- "32"
municodigos <- read.csv2("data/tabcodigosmunibgedatasus.csv", stringsAsFactors = FALSE)[,-1]
municodigos <- municodigos[grepl(paste0("^",ufmat),municodigos$codigomun) & grepl(paste(municipios,collapse="|"),municodigos$municipio),]

#fazer para seleção de municípios
for (i in 1:length(municodigos$codigomun)) {
  mat_m17 <- matricula2017[CO_MUNICIPIO == municodigos[i,1],-6]
  crechemun17 <- nrow(mat_m17[TP_ETAPA_ENSINO==1,1])
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
nv_0a4 <- sinasc_nv_mun(periodo = c("2014","2015","2016"), municipio = municodigos[i,3])[,2]+sinasc_pnv_mun(municipio = municodigos[i,3])[,2]

#obitos acumulados de 0 a 1 anos para os mesmos anos
ob10_0a4 <- sim_inf10_mun(periodo = c("2014","2015","2016"), municipio = municodigos[i,3])[,2]+sim_pinf10_mun(municipio = municodigos[i,3])[,2]


#acrescimopop sem contar migrações
popadic_0a4 <- nv_0a4 - ob10_0a4

##mesmo que o anterior de 0 a 3
nv_0a3 <- sinasc_nv_mun(periodo = c("2015","2016"), municipio = municodigos[i,3])[,2]+sinasc_pnv_mun(municipio = municodigos[i,3])[,2]

#obitos acumulados de 0 a 1 anos para os mesmos anos
ob10_0a3 <- sim_inf10_mun(periodo = c("2015","2016"), municipio = municodigos[i,3])[,2]+sim_pinf10_mun(municipio = municodigos[i,3])[,2]
#acrescimopop sem contar migrações nem mortes de 1 a 4
popadic_0a3 <- nv_0a3 - ob10_0a3

#vamos só retirar os que estariam entre 3 e 4 do dado "firme" de população total 
#de 0 a 4 anos?
popadic_3a4 <- popadic_0a4 - popadic_0a3
popadic_3a4 <- popadic_3a4[1]
popmtotal0a3 <- popmtotalidade - popadic_3a4
popmtotal0a3 <- popmtotal0a3[1]
#taxa de cobertura bruta equivalente a:
#https://observatoriocrianca.org.br/cenario-infancia/temas/educacao-infantil/1081-taxa-de-cobertura-em-creche?filters=1,77;3209,77;21,77

#Indicador 2 - col 3
cob_creche_bruta_m_17 <- crechemun17/popmtotal0a3
#tab_ecd_ed[i,3] <- cob_creche_bruta_m_17

cob_efet_0a3m <- matr_0a3_m/popmtotal0a3

#Indicador 3 - col 4
cob_efet_0a3mref <- matr_0a3_mref/popmtotal0a3
#tab_ecd_ed[i,4] <- cob_efet_0a3mref

##### Pré - Escola
nv_0a6 <- sinasc_nv_mun(periodo = c("2012","2013","2014","2015","2016"), municipio = municodigos[i,3])[,2]+sinasc_pnv_mun(municipio = municodigos[i,3])[,2]

#obitos acumulados de 0 a 1 anos para os mesmos anos
ob10_0a6 <- sim_inf10_mun(periodo = c("2012","2013","2014","2015","2016"), municipio = municodigos[i,3])[,2]+sim_pinf10_mun(municipio = municodigos[i,3])[,2]


#acrescimopop sem contar migrações
popadic_0a6 <- nv_0a6 - ob10_0a6

#vamos só somar os que estariam entre 4 e 5 do dado "firme" de população total 
#de 0 a 4 anos?
popadic_4a6 <- popadic_0a6 - popadic_0a4
popmtotal0a6 <- popmtotalidade + popadic_4a6
popmtotal4a6 <- popadic_4a6[1]

#a pop total de 4 a 6 para observatorio da criança VIX é ~ 8.320 (migração? pq?)


#taxa de cobertura bruta que deveria ser equivalente a:
# observatoriodacrianca
coberturapresc <- prescmun17/popmtotal4a6


cob_presc_efet_4e5m <- matr_4e5_m/popmtotal4a6


cob_efet_4e5mref <- matr_4e5_mref/popmtotal4a6




tab_ecd_ed <- rbind(tab_ecd_ed,c(municodigos$municipio[i],cob_creche_bruta_m_17,cob_efet_0a3mref,coberturapresc,cob_efet_4e5mref,""), stringsAsFactors = FALSE)


}
names(tab_ecd_ed) <- c("local",ind_nomes)

#para o ES
mat_uf_17 <- matricula2017[CO_UF==32,-6]

crechees17 <- nrow(mat_uf_17[TP_ETAPA_ENSINO==1,1])

#matriculados em creche menores de 4 anos
matr_0a3_uf <- nrow(mat_uf_17[NU_IDADE < 4,1])
matr_0a3_ufref <- nrow(mat_uf_17[NU_IDADE_REFERENCIA < 4,1])
matr_0a3_ufjun <- matr_0a3_ufref -nrow(mat_uf_17[NU_IDADE == 4 & NU_MES == 6,])


popestotalecd <- sum(es17popidades$`0 a 4 anos`)

#popadic ES de 0 a 3 anos estimada por nascimentos e óbitos

##Calculando pop. adicional 0 a 4 por nascidos e óbitos infantis
nves_0a4 <- sinasc_nv_bruf(periodo = c("2014","2015","2016"), unidade_da_federacao = "Espírito Santo")[,2]+sinasc_pnv_bruf(unidade_da_federacao = "Espírito Santo")[,2]

#obitos acumulados de 0 a 1 anos para os mesmos anos
ob10es_0a4 <- sim_inf10_bruf(periodo = c("2014","2015","2016"), unidade_da_federacao = "Espírito Santo")[,2]+sim_pinf10_bruf(unidade_da_federacao = "Espírito Santo")[,2]
#acrescimopop sem contar migrações nem mortes de 1 a 4
popadices_0a4 <- nves_0a4 - ob10es_0a4

##mesmo que o anterior de 0 a 3
nves_0a3 <- sinasc_nv_bruf(periodo = c("2015","2016"), unidade_da_federacao = "Espírito Santo")[,2]+sinasc_pnv_bruf(unidade_da_federacao = "Espírito Santo")[,2]

#obitos acumulados de 0 a 1 anos para os mesmos anos
ob10es_0a3 <- sim_inf10_bruf(periodo = c("2015","2016"), unidade_da_federacao = "Espírito Santo")[,2]+sim_pinf10_bruf(unidade_da_federacao = "Espírito Santo")[,2]
#acrescimopop sem contar migrações
popadices_0a3 <- nves_0a3 - ob10es_0a3

popadices_3a4 <- popadices_0a4 - popadices_0a3

popes0a3 <- popestotalecd - popadices_3a4
popes0a3 <- popes0a3[1]
coberturaces <- crechees17/popestotalecd
#Taxa de cobertura bruta de creche próxima a
# observatoriocrianca.org.br
cobcrechebrutaes17 <- crechees17/popes0a3

cob_efet_0a3uf <- matr_0a3_uf/popes0a3
cob_efet_0a3ufref <- matr_0a3_ufref/popes0a3
#Taxa de cobertura líquida próxima ao mesmo indicador anterior:
cob_efet_0a3ufjun <- matr_0a3_ufjun/popes0a3


tab_ecd_ed <- rbind(tab_ecd_ed,c("Espírito Santo",cobcrechebrutaes17,cob_efet_0a3ufref,"","",""), stringsAsFactors = FALSE)


#Número de matrículas em Pré-escola / população de crianças na faixa etária
#A) Indicador municipal

#municipal

prescmun17 <- nrow(mat_m17[TP_ETAPA_ENSINO == 2,])


#para cobertura efetiva, verificar criancas da idade 4 e 5 matriculadas
matr_4e5_m <- nrow(mat_m17 %>% filter (NU_IDADE >= 4 & NU_IDADE < 6 & 
                                         TP_ETAPA_ENSINO == 2))

matr_4e5_mref <- nrow(mat_m17 %>% filter (NU_IDADE_REFERENCIA >= 4 & 
                                            NU_IDADE_REFERENCIA < 6 &
                                            TP_ETAPA_ENSINO == 2))

#obs - observatoriocrianca fechou tp = 2
#nascidos vivos residência da mãe datasus


#nascidos vivos de 0 a 6
for (i in 1:length(municodigos$codatasus)) {

}
#para o ES

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
nves_0a6 <- sinasc_nv_bruf(periodo = c("2012","2013","2014","2015","2016"), unidade_da_federacao = "Espírito Santo")[,2]+sinasc_pnv_bruf(unidade_da_federacao = "Espírito Santo")[,2]

#obitos acumulados de 0 a 1 anos para os mesmos anos
ob10es_0a6 <- sim_inf10_bruf(periodo = c("2012","2013","2014","2015","2016"), unidade_da_federacao = "Espírito Santo")[,2]+sim_pinf10_bruf(unidade_da_federacao = "Espírito Santo")[,2]
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

rm(matricula2017)
gc()
censoinfoesc <- c("CO_UF", "CO_MUNICIPIO", "TP_DEPENDENCIA", 
                  "IN_COMUM_CRECHE", "IN_COMUM_PRE",
                  "IN_ESP_EXCLUSIVA_CRECHE", "IN_ESP_EXCLUSIVA_PRE")

escolas2017 <- read_CensoEscolar(ft = "escola",i = 2017, vars_subset = censoinfoesc,                                  root_path = dadoslocais)
#fazer para 1 município
escm17 <- escolas2017[(CO_MUNICIPIO ==  municodigos[1,1] ) & 
                        (IN_COMUM_CRECHE == 1 | IN_COMUM_PRE == 1 | 
                        IN_ESP_EXCLUSIVA_CRECHE == 1 | IN_ESP_EXCLUSIVA_PRE == 1),]
escolas_m_mil_17 <- nrow(escm17)*1000/popmtotal0a6

#N. de creches e turmas de pré-escolas públicas (% total) 
pubtotal <- nrow(escm17[TP_DEPENDENCIA != 4])/nrow(escm17)

#Nº de alunos por turma / Nº alunos por professor

censoinfoturma <- c("ID_TURMA","NU_MATRICULAS","TP_ETAPA_ENSINO",
                    "CO_UF","CO_MUNICIPIO")
turmas2017 <- read_CensoEscolar(ft = "turma",i = 2017, vars_subset = censoinfoturma,                                  root_path = dadoslocais)
turmas_m_17 <- turmas2017[CO_MUNICIPIO == municodigos[1,1] & TP_ETAPA_ENSINO <= 2,
                          1:2 ]
alturma_m_17 <- sum(turmas_m_17$NU_MATRICULAS)/length(unique(turmas_m_17$ID_TURMA))


#  - Gasto público em educação com creches e pŕe-escolas por município, ES e BR (absoluto e per capita)
#           -  Não há no Censo Escolar - procurar no #portal da transparencia(?)
#           - há algum lugar que junte dados municipais?

