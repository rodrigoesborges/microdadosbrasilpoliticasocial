#Dados de famílias e pessoas com renda inferior a 60% do salário mínimo
# carrega os pacotes necessários
library(DBI)
library(MonetDBLite)
library(survey)
library(dbplyr)
library(dplyr)
library('ipeaData')

# define o diretório onde serão depositados os dados - IDEM Montabcadunico.R
output_dir <- file.path( getwd() , "CadUnico" )

# lê o objeto com o desenho amostral
anob <- 2017
fam.design <- readRDS( file.path( output_dir , paste0("familia ",anob," design.rds" )) )

# abre a conexão do objeto com a base em MonetDBLite
fam.design <- open( fam.design , driver = MonetDBLite() )

#Salário Mínimo IPEA
ipeasm <- "MTE12_SALMIN12"
smipea <- ipeadata(ipeasm, type = data.table)
#Anualizar por média
smipea <- aggregate(VALVALOR ~ ANO,smipea ,mean, na.rm = TRUE)
#subconjunto
smipea <- smipea[smipea$ANO > 1994,]
smipea$VALVALOR <- smipea$VALVALOR*0.6
#### Para 2017
smir <- smipea[smipea$ANO == anob, 2]


cfam60 <- data.frame(matrix(ncol=5,nrow=0))
names(cfam60) <- c("ano","rbaixacomfilhos", "rbaixasemfilhos", "rmbacomfilhos", "rmbasemfilhos")

fam.sm60 = data.frame(matrix(ncol=5,nrow=0))
names(fam.sm60) <- c("ano","familias+0.6SMpc","familias-0.6SMpc","familias-0.6SMtotal", "prop")

# número de famílias com renda total inferior a 60%SM e com renda per capita inferior a 60%SM
totalpc.sm60 <- svytotal( ~I(vlr_renda_media_fam < 562), fam.design , na.rm = TRUE )
total.sm60 <- svytotal( ~I(vlr_renda_media_fam * qtde_pessoas  < 562), fam.design , na.rm = TRUE )


fam.sm60 <- rbind(fam.sm60,c(anob,totalpc.sm60["I(vlr_renda_media_fam < 562)FALSE"],
                             totalpc.sm60["I(vlr_renda_media_fam < 562)TRUE"],
                             total.sm60["I(vlr_renda_media_fam * qtde_pessoas < 562)TRUE"]))
fam.sm60$prop <- fam.sm60[,3]/(fam.sm60[,2]+fam.sm60[,3])

close( fam.design , shutdown = TRUE )               

## Total per capita abaixo de 60% SM- E famílias com crianças de 0 a 9 teste
##Passo 1 - criar a coluna que será adicionada aos dados base originais dentro do BD MonetDBLite
db <- dbConnect(MonetDBLite::MonetDBLite(), catalog[1,"dbfolder"])
familiasid0a9 <- dbGetQuery(db, paste0("SELECT DISTINCT id_familia from cadunico_",anob," WHERE idade < 10"))

#Passado para a compilação original das tabelas - não consegui / muito lenta a atualização utilizando query sql
# Esse era o comando de atualização da coluna
dbExecute(db, "UPDATE familia_2017 SET filhosanove = 1 WHERE id_familia IN (?)", params = paste0(familiasid0a9$id_familia,collapse = ", ")) 

##Famílias com filhos 0 a 9 e com renda < 0,6SM
famfi60 <- svyby(~I(f0a9 == 1), ~I(smf60 == 1), fam.design, svytotal, na.rm = TRUE)


cfam60 <- rbind(cfam60,c(anob,famfi60$`I(f0a9 == 1)TRUE`[2], 
               famfi60$`I(f0a9 == 1)FALSE`[2], 
               famfi60$`I(f0a9 == 1)TRUE`[1], 
               famfi60$`I(f0a9 == 1)FALSE`[1]))
write.csv(cfam60,"data/2012-2017-cadunico-familias0a9rt060SM.csv")
