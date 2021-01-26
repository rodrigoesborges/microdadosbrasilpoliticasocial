###Baixar e processar tábuas completas de mortaliade do IBGE
require('readODS')
### Definição da pasta onde foi montado o ftp do IBGE
dtibge <- "~/RLocalData/IBGEftp"

#Definição do caminho base para a informação do IBGE que será coletada - Tábua de Mortalidade
basep <- "/Tabuas_Completas_de_Mortalidade/Tabuas_Completas_de_Mortalidade_"

#Definição dos anos para fazer a seleção das pastas
anosel <-  seq(2012,2017,1)

#Tábuas para ambos sexos
flt <- "ambos*"

#Diretório para cópia das tabelas dos diferentes anos do IBGE
dfin <- "data/ibgem"

#Obtenção dos nomes de arquivos ODS/ZIP correspondentes disponíveis:
narq <- sapply(paste0(dtibge, basep, lapply(anosel,paste0,"/ods")),
               list.files, pattern = flt)

#Salvar em base relacionando ano ao arquivo ODS/ZIP
nm_tabuas <- data.frame(ano = anosel, 
                        nomearquivo = as.character(narq), stringsAsFactors = F)

nm_tabuas$end_completo <- paste0(row.names(nm_tabuas),"/")

rownames(nm_tabuas) <- NULL
nm_tabuas$arqf <- paste0(dfin,"/",nm_tabuas$ano,"-ambos_os_sexos.ods")

#Copia local (fora do FTP IBGE) dos arquivos e arquivos extraídos
lisz <- do.call(paste0,nm_tabuas[grepl("zip",nm_tabuas$nomearquivo),3:1])

#Extrai tabelas de arquivos zip
sapply(lisz[1:2],unzip, exdir = dfin)

#Renomeia
file.rename(paste0(dfin,"/ambos.ods"),paste0(dfin,"/2012-ambos_os_sexos.ods"))
file.rename(paste0(dfin,"/ambos_os_sexos.ods"),paste0(dfin,"/2013-ambos_os_sexos.ods"))

#Copia das fontes ftp os arquivos já descompactados para o arquivo correto
#arquivos já descompacatdos
nm_tabuas_d <- nm_tabuas[!grepl("zip",nm_tabuas$nomearquivo),]

for (i in 1:nrow(nm_tabuas_d)) {
  file.copy(from = paste0(nm_tabuas_d[i,]$end_completo,nm_tabuas_d[i,]$nomearquivo),
            to = nm_tabuas[i,]$arqf)
}


#Processar as planilhas para data frames em R
#Nomes das colunas padrão
nom_cols <- apply(read_ods(nm_tabuas$arqf[1], range = "A3:G5", na = " "),2,paste, collapse = "\n")
nom_cols[length(nom_cols)+1] <- "ano"
nom_cols <- gsub("\nNA","",nom_cols)


for (i in 1:nrow(nm_tabuas)) {
  nome <- paste0("tm_ibge",nm_tabuas$ano[i])
  assign(nome,cbind(read_ods(nm_tabuas$arqf[i], range = "A6:G45"),ano = nm_tabuas$ano[i]))
  pedaco2 <- cbind(read_ods(nm_tabuas$arqf[i],range = "A62:G102"),ano = nm_tabuas$ano[i])
  assign(nome,rbind(get(nome),pedaco2))
  rm(pedaco2)
}
                        
tm_ibge <- rbindlist(mget(ls(pattern = "^tm_ibge\\d+")))
rm(list = ls(pattern = "^tm_ibge\\d+"))
names(tm_ibge) <- nom_cols

write.csv2(tm_ibge, paste0(dfin,"/tm_ibge_",nm_tabuas$ano[1],"-",nm_tabuas$ano[nrow(nm_tabuas)],".csv"),row.names = F)

