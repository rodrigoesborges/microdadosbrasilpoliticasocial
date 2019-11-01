# produção de sumários estatísticos de alguns dados iniciais

###Produção de resumos estatísticos dos dados
require(qwraps2)

#Requisitos - resultados de ecd em assistência, educação e saúde

#Definição de constantes / padrões
dtsav <- "~/RLocalData/ecd-indicadores"

#Anos da análise
anosel <- seq(2012,2017,1)


####Carrega os resultados de cada área

#Educação
educa <- read.csv2("data/2012-2017-tab_ecd_ed.csv")[,-1]

#Assistência
cadunicoecd <- readRDS(paste0("data/2012-2017-indicadores-cadunico-ES.rds"))
#cadunicoecd <- cadunicoecd[,c(1,6,10,2:5,7:9)] %>% pivot_longer(cols = 4:10, names_to = "indicador")
cadunicoecd$cod_mun <- as.numeric(cadunicoecd$cod_mun)

#Saúde
saude <- read.csv2("data/2012-2017-indicadores_saude_brutos.csv")
#saude <- saude %>% pivot_longer(cols = 4:10, names_to = "indicador", values_to = "valor")
#saude$cod_mun <- as.numeric(levels(saude$cod_mun))[saude$cod_mun]


#Exemplo - resumos para saúde

saude <- saude %>% group_by(ano)
saude <- saude[complete.cases(saude),]


#Qsummary já resolve, roda reinventada
# obj_tit <- names(saude[,-1:-3])
# 
# obj_resumos <- lapply(1:length(obj_tit),function(x) { 
#   list("min" = substitute(~ min(.data$y),list(y = as.name(obj_tit[x]))),
#        "max" = substitute(~ max(.data$y),list(y = as.name(obj_tit[x])))
# )})
       # ,
 #       "média" = substitute(~ mean(.data$y),list(y = as.name(obj_tit[x]))),
 #       "Mediana (1Qtr, 3 Qtr)" = substitute(~ median_iqr(.data$y),list(y = as.name(obj_tit[x])))
 #  )
 # })

# names(obj_resumos) <- obj_tit

saude <- saude %>% distinct()

saude <- saude %>% group_by(ano)
saude <- saude %>% ungroup()
sumarios_auto <- saude %>% select(4:10) %>% qsummary(.)
resultados_saude_sumario <- summary_table(saude,sumarios_auto)


#CadUnico

cadunicoecd <- cadunicoecd %>% group_by(ano)
cadunicoecd <- cadunicoecd[,c(1,6,10,2:5,7:9)]
sum_as_auto <- cadunicoecd %>% select(c(2,3,7,8,9)) %>% qsummary(.)
resultados_as_sumario <- summary_table(cadunicoecd,sum_as_auto)

#Possibilidade de refinar futuramente
# #1 = indicador absoluto 2 = indicador relativo (% , valor 0 a 1)
# obj_resumos$tipo <- c(1,2,1,2,2,2,2)
# 
# obj_tip_cont <- list(list("min" = ~ min(get(paste0(".data$"obj_resumos[,1]))),
#                           "max"
#                           ))
