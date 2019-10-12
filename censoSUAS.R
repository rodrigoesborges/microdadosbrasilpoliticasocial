#Tentativa de acesso a censo SUAS
#Longo prazo - verificar possibilidade de algo parecido a integrar em lodown 
#feito uma vez
# ajdrepo <- "ajdamico/lodown"
# install_github(ajdrepo, dependencies = TRUE)



#reposrvyr <- "gergness/srvyr"
require("devtools")
require(haven)
require(survey)
require(data.table)


#CENSO SUAS 2017 - 
# Já feito manualmente, primeira exploração
# URL 2017- 
#url2017 <- "https://aplicacoes.mds.gov.br/sagi/pesquisas/documentos/microdado/microdado_210.zip"
#download.file(url2017,pastadados)

pastadados <- "~/RLocalData/censoSUAS/"
nomcarq <- "microdado_210.zip"
nomcpad <- "Censo SUAS 2017"
unidinst <- "CRAS"
tipo <- "divulgacao"

unzip(paste0(pastadados,nomcarq), exdir = pastadados, unzip = getOption("unzip"))



      
nomcras <- paste0(pastadados,
                 nomcpad,"/",
                 gsub(" ","_",nomcpad),"_",
                 unidinst,"/",
                 nomcpad,"_",
                 unidinst,"_"
                 ,tipo,".sav")

crastab <- haven::read_spss(paste0(nomcras))


#De interesse - CRAS
#q1 - localização Urb C  Urb P Rural (10)
#q2 - capacidade do centro (11)
#q15.1 - familias PAIF (124)
#q16 - diretamente SCFV - sim/total (126)
#q17.1 - sim/total - sim / 0 a 6 - (127)
#q19.1 - diretamente SCFV - sim/total (137)
#q21 - frequência de visitas - cras à rede - não muito util - (144)
#q28 - PAIF por equipe volante - possivel prop total PAIF (163)
#q38 - pode ser util bairros de abrangência rural 2  1 0 (210)

#Inicializar tabela de resultados
ind_as_nomes = c("munípio/UF",
                 "Fam At Paif",
                 "Fam At Paif por Cras",
                 "Cras scfv 0a6",
                 "Famílias atendidas em Cras com scfv " )

tab_ecd_as <- data.frame(matrix(ncol = length(ind_as_nomes), nrow=0), stringsAsFactors = FALSE)
names(tab_ecd_as) <- ind_as_nomes



#Explorar apenas ES
uf <- "^32"
varcras <- c(1,4,7,10,11,18,28,29,32,33,34,124,125,126,127,137,144,136,210)
crastabes <- as.data.table(crastab[grepl(uf,crastab$IBGE7),varcras])

# Para o ES
fampaifes_17 <- sum(crastabes$q15.1, na.rm = TRUE)
#Famílias atendidas por CRAS
craspaifes17 <- fampaifes_17/nrow(crastabes)

#N. de CRAS que ofertam o SCFV para crianças de 0 a 6 anos
cras_scfv_es_17 <- nrow(crastabes[crastabes$q17.1 == 1,1])
fam_total_atendidas_cras_scfv_es_17 <- sum(crastabes[crastabes$q17.1 == 1, crastabes$q15.1], na.rm = TRUE)

tab_ecd_as <- rbind(tab_ecd_as, c("Espírito Santo",fampaifes_17,craspaifes17,
                                  cras_scfv_es_17,fam_total_atendidas_cras_scfv_es_17),
                    stringsAsFactors = FALSE)
names(tab_ecd_as) <- ind_as_nomes

### Para os municípios
# pegando municod anterior
for (i in 1:length(municodigos$codigomun)) {
  crastabmun <- crastabes[IBGE7 == municodigos[i,1],]
  fampaifmun_17 <- sum(crastabmun$q15.1, na.rm = TRUE)
  craspaifmun17 <- fampaifmun_17/nrow(crastabmun)
  cras_scfv_mun_17 <- nrow(crastabmun[crastabmun$q17.1 == 1,1])
  fam_total_atendidas_cras_scfv_mun_17 <- sum(crastabmun[crastabmun$q17.1 == 1,]$q15.1, na.rm = TRUE)
  tab_ecd_as <- rbind(tab_ecd_as, c(municodigos[i,2],
                                    fampaifmun_17,
                                    craspaifmun17,
                                    cras_scfv_mun_17,
                                    fam_total_atendidas_cras_scfv_mun_17),
                      stringsAsFactors = FALSE)
  names(tab_ecd_as) <- ind_as_nomes
  
}

write.csv2(tab_ecd_as,"data/ecd_assistencia_ind.csv")
### CREAS não possui questionario adaptado a  primeira infância


#De interesse - Abrigos
#q15 - faixa etária e sexo dos abrigados
#q31 - tempo de acompanhamento após desligamento
#informação de q31 será mais fiável caso atendimento seja na própria unidade (?)


# 1) famílias (total) atendidas pelo PAIF / 
# 2) n. CRAS que oferecem de 0 a 6
# 3) No de CRAS que ofertam o SCFV para crianças de 0 a 6 anos


