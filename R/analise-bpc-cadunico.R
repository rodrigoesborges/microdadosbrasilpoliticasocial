#Compilação do número de beneficiários de BPC por município no estado do Espírito Santo
#Adaptado a partir de analisecadunico-polsocial-es-ecd.R

# carrega os pacotes necessários
library(DBI)
library(MonetDBLite)
library(survey)
library(dbplyr)
library(dplyr)
library(srvyr)
library(gsubfn)
library(data.table)
#período de análise
anosel <- seq(2012,2017,1)

# define o diretório onde serão depositados os dados - IDEM Montabcadunico.R
output_dir <- file.path( getwd() , "CadUnico" )

#Carraga tabela de compatibilidade datasus-ibge
municodigos <- read.csv2("data/tabcodigosmunibgedatasus.csv")

#carg_cadunico - iterar de acordo com o ano

cad_carr <- function(anos = 2017,dado = "CadUnico/cadunico 2017 design.rds" ) {
  # lê o objeto com o desenho amostral
  dbloc <- "~/RLocalData/cadunico/MonetDB"
  dbcon <- dbConnect(MonetDBLite(),dbloc)
  dbfora <- db_collect(dbcon, sprintf("SELECT * FROM cadunico_%s",anos))
  dbDisconnect(dbcon)
  #Remonta o objeto survey, sem vínculo a base de dados
  cadunicosvy <-
    svydesign(
      ids = ~id_familia ,
      strata = ~estrato ,
      weight = ~peso_pes ,
      nest = TRUE ,
      data = dbfora
    )
  cadunicosvy
  #Parte anterior, partindo de arquivo RDS gerado por cadunico.R e montabdcadunico.R
  #   pes.design <- readRDS(dado) ;  # # abre a conexão do objeto com a base em MonetDBLite; # pes.design <- open( pes.design, driver = MonetDBLite()) ;  #   pes.design
}

# Criar coluna de popmf / pmf - estimativas de população pop65mais
pop3i <- popmf_det %>% transmute(cod_mun = cod_mun, Município = Município, 
                                 ano = ano, n_idosos = rowSums(popmf_det[,7:19]), 
                                 idosos = rowSums(popmf_det[,20:23]), Total = Total)


#A função deve incluir campo para UF

calc_ind_cu_bpc <- function(ano = 2017, uf = 32) {
  ufc <- paste0("^",uf)
  #ano <- 2017
  #ufc <- "^32"
  dado <- paste0("CadUnico/cadunico ",ano," design.rds")
  cadunicobpc<- cad_carr(ano, dado)
  cadunicobpc <- subset(cadunicobpc, grepl(sprintf("%s",ufc),cd_ibge) & idade > 64)
  #& smf25pc > 0
  ecdbpcmun <- svytable(~cd_ibge+cod_raca_cor_pessoa+smf25pc+cod_sexo_pessoa+cod_curso_frequentou_pessoa_memb,cadunicobpc)
  # nomes originais "cod_sexo_pessoa", "cod_raca_cor_pessoa","cd_ibge", "cod_curso_frequentou_pessoa_memb"
  #Compatibiliza com códigos do datasus
  ecdbpcmun <- as.data.frame(ecdbpcmun)
  ecdbpcmun$cd_ibge <- substr(ecdbpcmun$cd_ibge,1,6)
  
  names(ecdbpcmun) <- c("cod_mun","cor","extrema_pobreza_1-4sm_bpc","sexo","escolaridade","frequencia")

  
  ecdbpcmun <- ecdbpcmun %>% 
     full_join(pop3i[pop3i$ano == ano  & grepl(sprintf("%s",ufc),pop3i$cod_mun),c(1,2,5,6)], by = "cod_mun") 
   
  ecdbpcmun <- ecdbpcmun %>% mutate(ano = ano)
   
  ecdbpcmun 
}


tab_bpc_cadunico <- rbindlist(lapply(anosel, calc_ind_cu_bpc))


#Homogeneização para fatores com nomenclatura retirada do dicionário de microdados
tab_bpc_cadunico <- tab_bpc_cadunico %>% mutate(sexo = as.factor(tab_bpc_cadunico$sexo), 
                                                cor = as.factor(tab_bpc_cadunico$cor), 
                                                escolaridade = as.factor(tab_bpc_cadunico$escolaridade),
                                                direito_bpc_idade = as.factor(tab_bpc_cadunico$`extrema_pobreza_1-4sm_bpc`))
#cod_sexo_pessoa	
#Numeric	1		
#Sexo	
#1 - Masculino
#2 - Feminino

levels(tab_bpc_cadunico$sexo) <- c("masculino","feminino")

levels(tab_bpc_cadunico$cor) <- c('Branca','Preta','Amarela','Parda','Indígena')

levels(tab_bpc_cadunico$escolaridade) <- c('Pré-escola (exceto CA)','Classe de Alfabetização - CA',
                                           'Ensino Fundamental 1ª a 4ª séries, Elementar (Primário),
                                           Primeira fase do 1º grau','Ensino Fundamental 5ª a 8ª séries, 
                                           Médio 1º ciclo (Ginasial), Segunda fase do 1º grau',
                                           'Ensino Fundamental (duração 9 anos)','Ensino Fundamental Especial',
                                           'Ensino Médio, 2º grau, Médio 2º ciclo (Científico, Clássico, Técnico, Normal)',
                                           'Ensino Médio Especial',' Ensino Fundamental EJA - séries iniciais (Supletivo 1ª a 4ª)',
                                           ' Ensino Fundamental EJA - séries finais (Supletivo 5ª a 8ª)',' Ensino Médio EJA (Supletivo)',
                                           ' Superior, Aperfeiçoamento, Especialização, Mestrado, Doutorado',
                                           ' Alfabetização para Adultos (Mobral, etc.)',' Nenhum','Creche')


saveRDS(tab_bpc_cadunico, "data/2012-2017-indicadores-cadunico-ES-bpc-det.rds", compress = "gzip")
write.csv2(tab_bpc_cadunico,"data/2012-2017-indicadores-cadunico-ES-bpc-det.csv", row.names = F )
#### Resumo dos dados do Espírito Santo - Estado - Cadunico

resumo_es_cadunico <- rbindlist(lapply(2012:2017, 
                                       function(x) {data.frame(ano = x, 
                                                               "Beneficiários BPC" = round(sum(tab_bpc_cadunico[ano == x, ]$ben_bpc, na.rm = T), 0),
                                                               "Beneficiários PBF(% idosos)" = sum(tab_bpc_cadunico[ano == x, ]$ben_bpc, na.rm = T)/
                                                                 sum(tab_bpc_cadunico[ano == x, ]$pop65mais))}))

write.csv2(resumo_es_cadunico,"data/2012-2017-cadunico-resumo-bpc-ES.csv", row.names = F)

#### Totalizações por raça e ano

raca_bpc <- aggregate(frequencia~ano+cor, data = tab_bpc_cadunico, sum)

### Totalização por gênero e ano




genero_bpc <- aggregate(frequencia~ano+sexo, data = tab_bpc_cadunico, sum)

### Totalização por escolaridade e ano
escolaridade_bpc <- aggregate(frequencia~ano+escolaridade, data = tab_bpc_cadunico, sum)



