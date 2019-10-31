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



popm_inf <- readRDS("data/estimativas_pop_1a_infancia.rds")

popm_inf <- popm_inf %>% mutate(pop0a6 = round(`0 a 3 anos`+`4 a 6 anos`, digits = 0))

#A função deve incluir campo para UF

calc_ind_cu <- function(ano = 2017, uf = 32) {
    ufc <- paste0("^",uf)
    dado <- paste0("CadUnico/cadunico ",ano," design.rds")
    cadunicoecd<- cad_carr(ano, dado)
    print(str(cadunicoecd))
    #filtroc <- gsubfn("%s",ufc,"cd_ibge %like% '%s' & idade < 7 & marc_pbf")
    cadunico_ecd <- subset(cadunicoecd, grepl(sprintf("%s",ufc),cd_ibge) & idade < 7 & marc_pbf)
    
    #db <- dbConnect( MonetDBLite() , cadunicoecd$db$dbname).
    #cadunicoecd <- dbGetQuery(db , paste0( "SELECT * FROM pessoa_" ,ano," WHERE idade < 7" ) )
    #dbGetQuery( db , "SELECT stype , dnum , cname , SUM( pw ) FROM apiclus1 GROUP BY stype , dnum , cname" ) )
      ecdpbfmun <- svyby(~I(marc_pbf > 0),~cd_ibge,cadunico_ecd, svytotal,na.rm = TRUE )

      #Compatibiliza com códigos do datasus
      ecdpbfmun$cd_ibge <- substr(ecdpbfmun$cd_ibge,1,6)
      
      names(ecdpbfmun) <- c("cod_mun","n_pbf_cadunico", "ben_pbf", "desv_pad_n_pbf", "desv_pad_pbf")
      
      ecdpbfmun <- ecdpbfmun %>% 
        right_join(popm_inf[popm_inf$ano == ano  & grepl(sprintf("%s",ufc),popm_inf$cod_mun),c(1,2,7)], by = "cod_mun") 
        
      ecdpbfmun <- ecdpbfmun %>% mutate("prop_pinf_cadunico" = (n_pbf_cadunico+ben_pbf)/pop0a6, "pbf_pop0a6" = ben_pbf/pop0a6, ano = ano)
      
      ecdpbfmun 
}


tab_ecd_cadunico <- rbindlist(lapply(anosel, calc_ind_cu))

saveRDS(tab_ecd_cadunico, "data/2012-2017-indicadores-cadunico-ES.rds", compress = "gzip")
write.csv2(tab_ecd_cadunico,"data/2012-2017-indicadores-cadunico-ES.csv", row.names = F )
###Adaptar para calcular indicadores de todos os municípios, estado a estado
# combosf <-  expand.grid(anosel,0:9)
# names(combosf) <- c("dano","idade")
# 
# #Problema com a funçao - 0 a 1
# pop_var <- rbindlist(lapply(1:nrow(combosf),
#                             function(x) do.call(mapply,c("calc_id",combosf))[,x]))



#### Resumo dos dados do Espírito Santo - Estado - Cadunico

resumo_es_cadunico <- rbindlist(lapply(2012:2017, 
       function(x) {data.frame(ano = x, 
                               "Beneficiários PBF" = round(sum(tab_ecd_cadunico[ano == x, ]$ben_pbf, na.rm = T), 0),
                               "Beneficiários PBF(% crianças)" = sum(tab_ecd_cadunico[ano == x, ]$ben_pbf, na.rm = T)/
                                 sum(tab_ecd_cadunico[ano == x, ]$pop0a6))}))

write.csv2(resumo_es_cadunico,"data/2012-2017-cadunico-resumo-ES.csv", row.names = F)
