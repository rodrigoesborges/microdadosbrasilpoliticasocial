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
  dado <- paste0("CadUnico/cadunico ",ano," design.rds")
  cadunicobpc<- cad_carr(ano, dado)
  print(str(cadunicobpc))
  cadunico_bpc <- subset(cadunicobpc, grepl(sprintf("%s",ufc),cd_ibge) & idade > 64 & smf25pc)
  
  ecdpbfmun <- svyby(~I(smf25pc > 0),~cd_ibge,cadunico_bpc, svytotal,na.rm = TRUE )
  
  #Compatibiliza com códigos do datasus
  ecdpbfmun$cd_ibge <- substr(ecdpbfmun$cd_ibge,1,6)
  
  names(ecdpbfmun) <- c("cod_mun","n_bpc_cadunico","comdireito_a_bpc","desv_pad_n_bpc_cadunico","desv_pad_bpc")

  
  ecdpbfmun <- ecdpbfmun %>% 
     right_join(pop3i[pop3i$ano == ano  & grepl(sprintf("%s",ufc),pop3i$cod_mun),c(1,2,5,6)], by = "cod_mun") 
   
  ecdpbfmun <- ecdpbfmun %>% mutate("prop_idosos_cadunico" = (n_bpc_cadunico+comdireito_a_bpc)/idosos, "direito_bpc_prop_idosos" = comdireito_a_bpc/idosos, ano = ano)
   
  ecdpbfmun 
}


tab_bpc_cadunico <- rbindlist(lapply(anosel, calc_ind_cu_bpc))




saveRDS(tab_bpc_cadunico, "data/2012-2017-indicadores-cadunico-ES-bpc.rds", compress = "gzip")
write.csv2(tab_bpc_cadunico,"data/2012-2017-indicadores-cadunico-ES-bpc.csv", row.names = F )
#### Resumo dos dados do Espírito Santo - Estado - Cadunico

resumo_es_cadunico <- rbindlist(lapply(2012:2017, 
                                       function(x) {data.frame(ano = x, 
                                                               "Beneficiários BPC" = round(sum(tab_bpc_cadunico[ano == x, ]$ben_bpc, na.rm = T), 0),
                                                               "Beneficiários PBF(% idosos)" = sum(tab_bpc_cadunico[ano == x, ]$ben_bpc, na.rm = T)/
                                                                 sum(tab_ecd_cadunico[ano == x, ]$pop65mais))}))

write.csv2(resumo_es_cadunico,"data/2012-2017-cadunico-resumo-ES.csv", row.names = F)




