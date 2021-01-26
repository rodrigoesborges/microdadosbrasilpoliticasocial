# O objetivo deste script é baixar e processar as estimativas
# populacionais do IBGE para construção de indicadores 
# nacionais, estaduais ou municipais com os demais
# microdados disponíveis - foco inicial em população de primeira infância
#0 a 6 anos

#dados municipais
# As bases são o SIDRA e o DATASUS
devtools::install_github('rodrigoesborges/RSidra')
require(RSIDRA)
#Necessário caso seja atualizado o pacote datasus
#require(devtools)
#devtools::install_github('rodrigoesborges/datasus', force = TRUE)
require(datasus)
require(stringr)
require(tidyverse)
require(dplyr)
require(data.table)
# Correção de valores nulos extraídos do datasus
source("R/zr_fun.R")
#library(stringr)
#library(scales)

#Definição do período de estimativas populacionais
#Ano inicial atual primeiro ano com microdados disponíveis CadÚnico
anoin <- 2012
afim <- 2017
anosel <- seq(anoin,afim,1)



###1) Obtenção das população por faixa etária e consolidação em uma base geral de população por faixa etária por município:
#Inicialização de tabela base de população por faixa etária
popmf <- data.frame(matrix(ncol = 4, nrow=0))

#Definição do último ano disponível
uano <- 2015
#Definição do primeiro ano de estudo - atualmente primeiro ano com microdados de CadÚnico disponíveis
pano <- anoin
rp_anos <- as.character(seq(pano,uano, 1))

for (i in 1:length(rp_anos)) {
  pmf <- novapop_popbr_mun(periodo = rp_anos[i])
  pmf <- pmf %>% gather(faixa_etaria,populacao,-1) %>%
    separate(`Município`, c("cod_mun","Município"),sep="\\s", extra = "merge", fill = "left")
  pmf <-  cbind(ano = rep(rp_anos[i],nrow(pmf)),pmf)
  popmf <- rbind(popmf,pmf)
}

popmf$ano <- as.numeric(as.character(popmf$ano))

###2) Estimação da população por faixa etária para anos posteriores ao último disponível

#======= Necessidade da pirâmide etária=========
#===== 1) 2015 / último ano disponível = Pirâmide etária municipal / pirâmide etária estadual = fator para cada faixa
#======2) Pir. Etária Estaduais para cada ano > último ano disponível
#======3) Aplicar os fatores de 2 em 1, ou seja, multiplicar o vetor 2 em cada linha de 1
#======4) Balancear para que soma seja 100% da pirâmide etária balanceada
#======5) aplicar os totais, deixar proporcional de forma que a soma total do mun = proj mun para o ano

#Última pirâmide disponível
rp_uano <- popmf[popmf$ano == uano  & popmf$cod_mun != "TOTAL",]

#faixas etárias
fxet <- unique(rp_uano$faixa_etaria)

#pop_uf pelo RIPSA, último ano disponivel
rp_uano_uf <- setDT(rp_uano)[,.(populacao = sum(populacao, na.rm = TRUE)),
                              by = .(uf = substr(cod_mun,1,2), faixa_etaria = faixa_etaria, ano = ano)]

#piram_uf pelo RIPSA, último ano disponível
prp_uano_uf <- setDT(rp_uano_uf)[,propuf := populacao/sum(populacao[faixa_etaria != "Total"], na.rm = TRUE), by = uf]

#piram_mun pelo RIPSA, último ano disponível
prp_uano_m <- setDT(rp_uano)[,prop := populacao/sum(populacao[faixa_etaria != "Total"], na.rm = TRUE), 
                      by = c("ano","cod_mun","Município")]

#Cálculo do 'Fator´  para último ano disponível de pirâmide etária

prp_uano_m <- (setDT(prp_uano_m %>% mutate(uf = substr(cod_mun,1,2)))[setDT(prp_uano_uf), 
                                                                      on = c("uf", "faixa_etaria"), 
                                                                      propuf := propuf])

prp_uano_m$fator <- with(prp_uano_m, (prop/propuf))

#ano fora fator
prp_uano_m$ano <- as.numeric(as.character(prp_uano_m$ano))

#seleciona apenas variáveis de interesse
prp_uano_m <- prp_uano_m %>% select(c(ano,uf,cod_mun,Município,faixa_etaria,fator))


#Pirâmide etária estadual dos ano correspondente
piram_uf_fxet <- function(a) {
  pop_uf_fxet_ano <- ibge_projpop_bruf(coluna = "Faixa Etária 1", periodo = a)
  #Equaliza faixas etárias
  nom_ufs <- as.character(pop_uf_fxet_ano[,1]) 
  pop_uf_fxet_ano <- pop_uf_fxet_ano[,c(-1,-length(pop_uf_fxet_ano))] %>%
    mutate(ultfaixa = rowSums(.[17:19])) %>%
    select(-17,-18,-19)
  
  #Nomeia corretamente a última faixa
  names(pop_uf_fxet_ano)[length(pop_uf_fxet_ano)] <- "80 anos ou mais"
  
  pr_uf_ano <- cbind(ano = a,nom_ufs,pop_uf_fxet_ano/rowSums(pop_uf_fxet_ano))
  pr_uf_ano <- pr_uf_ano %>% separate(nom_ufs,into = c("uf", "Unidade da Federação"), sep = "\\s", extra = "merge", fill = "left")
}

pr_uf_ano <- data.frame(matrix(ncol = 20, nrow = 0), stringsAsFactors = FALSE)

pr_mun_ano <- prp_uano_m

for (i in 1:length(anosel[anosel>uano])) {
  a <- i+uano
  pr_uf_ano <- rbind(pr_uf_ano,piram_uf_fxet(a))
  pr_mun_ano <- rbind(pr_mun_ano, prp_uano_m %>% mutate(ano = a)) 
}

pr_uf_ano <- pr_uf_ano %>% gather(faixa_etaria,prop_u,c(-1,-2,-3))

pr_mun_ano <- setDT(pr_mun_ano)[setDT(pr_uf_ano), on = c("uf", "faixa_etaria","ano"), 
                                prop_m := prop_u*fator ]
pr_mun_ano <- pr_mun_ano[!(is.na(pr_mun_ano$prop_m)),]

#4) Balanceamento para corrigir erros numéricos que levam a soma de fatores pouco diferente de 1
pr_mun_ano <- setDT(pr_mun_ano)[,prop_m := prop_m/sum(prop_m, na.rm = TRUE),by = c("cod_mun","ano")]

#pr_mun_ano$prop_m <- with(pr_mun_ano,prop_m/som_mun) 

pr_mun_ano <- pr_mun_ano %>% select(ano,uf,cod_mun,Município,faixa_etaria,prop_m)


#5) Multiplicar percentuais pela população total do município

# ----- dados populacionais básicos municipais mais recentes-----------------------------------------------------------

#Tabela no SIDRA
#tirar a tabela
#tabela população '6579'
t <- 6579
#Variável retirada através de SIDRA_variaveis(t)
#variavel do SIDRA contagem pop 9324
vpop <- 9324
#nível de desagregação municipal : 6
nivmun <- 6
#periodo
popmun_ano <- API_SIDRA(t, nivel = nivmun, variavel = vpop, inicio = uano+1, fim = afim)

#Filtra variáveis e compatibiliza código de município com formato DATASUS
popmun_ano <- popmun_ano %>% 
  mutate(cod_mun = substr(popmun_ano$`Município (Código)`,1,6)) %>%
  select(ano = `Ano (Código)`, cod_mun, populacao = Valor)


#5) Multiplicar percentuais pela população total

pop_mun_fext_ano <- setDT(pr_mun_ano)[setDT(popmun_ano), 
                                      on = c("cod_mun","ano"), 
                                      populacao := i.populacao * prop_m]



#6) Juntar populações oficialmente estimadas com adições deste script (3 a 5)   
popmf <- rbind(popmf,pop_mun_fext_ano[,c(1,3:5,7)])

#7) Reagrupar as colunas de 0 a 4 e de 5 a 9 para 0 a 3 (creche) , de 4 a 5 (pré-escola) e de 6 a 9, datasus
#DATASUS fornece dados de nascidos e óbitos - estes de 0 a 1 e de 1 a 4
#Tábuas de mortalidade do IBGE fornecem critério para dividir mortes de 1 a 4 entre 1 a 3 e 3 a 4

muni_cods <- unique(popmf$cod_mun[!(is.na(popmf$cod_mun))])


#Nascidos vivos por ano e por município  
nascidos <- rbindlist(lapply((anoin-6):afim,
                   function(x) data.frame(sinasc_nv_mun(periodo = x),
                                          ano = x, indicador ="nascidos vivos")))

#OBS ao captar os obitos de uma vez, o que ocorre é a ausência de municípios sem registro de óbitos
obitos0a1 <- rbindlist(lapply((anoin-6):afim,
                              function(x) data.frame(sim_inf10_mun(periodo = x),
                                                     ano = x, indicador ="mortes 0 a 1 ano")))


obitos1a4 <- rbindlist(lapply((anoin-6):afim,
                              function(x) data.frame(sim_obt10_mun(faixa_etaria = "1 a 4 anos", periodo = x),
                                                     ano = x, indicador ="mortes 1 a 4 anos")))

obitos5a9 <- rbindlist(lapply((anoin-6):afim,
                    function(x) data.frame(sim_obt10_mun(faixa_etaria = "5 a 9 anos", periodo = x),
                                           ano = x, indicador ="mortes 5 a 9 anos")))

#Consolidar os diferentes objetos em uma grande tabela só sus_nasc_ob OK
sus_nasc_ob <- rbind(nascidos,obitos0a1,obitos1a4,obitos5a9, use.names = F)
names(sus_nasc_ob)[2] <- "Nasc. ou Mortes"
sus_nasc_ob <- sus_nasc_ob %>% 
  separate(Município,into = c("cod_mun","Município"), sep = " ",extra = "merge")




#Passos restantes - preparação dos dados
#1) Adicionar zeros para municípios faltantes em cada um dos objetos


#Preparação de indicadores a partir da tábua de mortalidade ibge
#1) Recuperar tabela com tábuas de mortalidadez
tm_ibge <- read.csv2('data/ibgem/tm_ibge_2006-2017.csv', stringsAsFactors = F)

#Converter a numérico a coluna 1
tm_ibge[,1] <- as.numeric(tm_ibge[,1])
#80 a mais reduzido para 80
tm_ibge[is.na(tm_ibge$Idades.Exatas..X.),1] <- 80
#converter a numérico coluna de óbitos
tm_ibge[,3] <- as.numeric(tm_ibge[,3])


#2) Calcular proporção de mortes de 1 a 2 no contingente entre 1 e 5 para cada ano

#Criação de função customizada para poder rodar em loop

props_calc <- function(a) {
  
#Ex 2006
#Denominador
den_prop1 <- sum(tm_ibge[tm_ibge$ano == a & tm_ibge$Idades.Exatas..X. >= 1 & tm_ibge$Idades.Exatas..X. < 5, 3])

#Indicador para 2
ind_1a2 <- data.frame(valor = tm_ibge[tm_ibge$ano == a & tm_ibge$Idades.Exatas..X. == 1,3]/den_prop1, 
                indicador = "prop. obitos de 1 a 2", ano = a, stringsAsFactors = F)

#3) Calcular proporção de mortes de 2 a 3 no contingente entre 1 e 5 para cada ano
ind_2a3 <- data.frame(valor = tm_ibge[tm_ibge$ano == a & tm_ibge$Idades.Exatas..X. == 2,3]/den_prop1,
                indicador = "prop. obitos de 2 a 3", ano = a, stringsAsFactors = F)

#4) Calcular proporção de mortes de 3 a 4 no contingente entre 1 e 5 para cada ano
ind_3a4 <- data.frame(valor = tm_ibge[tm_ibge$ano == a & tm_ibge$Idades.Exatas..X. == 3,3]/den_prop1,
                indicador = "prop. obitos de 3 a 4", ano = a, stringsAsFactors = F)
#5) Calcular proporção de mortes de 4 a 5 no contingente entre 1 e 5 para cada ano
ind_4a5 <- data.frame(valor = tm_ibge[tm_ibge$ano == a & tm_ibge$Idades.Exatas..X. == 4,3]/den_prop1,
                indicador = "prop. obitos de 4 a 5", ano = a, stringsAsFactors = F)

###_-----------
#6) Calcular proporção de mortes de 5 a 6 no contingente entre 5 e 10 para cada ano
#Denominador faixa etária 2
den_prop2 <- sum(tm_ibge[tm_ibge$ano == a & tm_ibge$Idades.Exatas..X. >= 5 & tm_ibge$Idades.Exatas..X. < 10, 3])

ind_5a6 <- data.frame(valor = tm_ibge[tm_ibge$ano == a & tm_ibge$Idades.Exatas..X. == 5,3]/den_prop2,
                indicador = "prop. obitos de 5 a 6", ano = a, stringsAsFactors = F)

#7) Calcular proporção de mortes de 6 a 7 no contingente entre 5 e 10 para cada ano
ind_6a7 <- data.frame(valor = tm_ibge[tm_ibge$ano == a & tm_ibge$Idades.Exatas..X. == 6,3]/den_prop2,
                indicador = "prop. obitos de 6 a 7", ano = a, stringsAsFactors = F)
rbindlist(mget(ls(pattern = "ind_\\d")))
}

#Calcula todas as proporções e insere em uma tabela
props_tm_ibge <- rbindlist(lapply(unique(tm_ibge$ano),props_calc))


#Temporario para ate 9
gambi <- props_tm_ibge[ano == 2006,]
for (i in 2003:2005) {
gambi$ano <- i
props_tm_ibge <- rbind(props_tm_ibge,gambi)
}

#Cálculo dos seguintes contingentes:
# A) População de ano a ano (máx 9) em ano x

calc_id <- function(dano = 2012,idade = 4, tabm = props_tm_ibge,tabsus = sus_nasc_ob) {
  #############PROBLEMA - VETORIZAR PARA FAZER POR MUNICIPIO
  ######1) pegar nascidos ano - 4 - em 2008 (4 a 5 em 2012) - 
  
  nasc <- tabsus[indicador == "nascidos vivos" &  ano == (dano-idade),1:3]
  names(nasc)[3] <- "nascimentos"
  ######2) média de óbitos 0 a 1 ano - 4 e ano-3 - 2008 e 2009
  obs_0 <- tabsus[ano == (dano-idade) & indicador == "mortes 0 a 1 ano",1:3]
  obs0b <- tabsus[ano == (dano+1-idade) & indicador == "mortes 0 a 1 ano",1:3]
#  print(nasc[cod_mun == "355030",])
  obs_0 <- full_join(obs_0,obs0b, by = c("cod_mun","Município")) %>% 
    mutate_if(is.numeric,coalesce,0) %>%
    transmute(cod_mun = cod_mun, Município = Município,'Nasc. ou Mortes' = rowMeans(select(., matches("Nasc. ou Mortes.*"))))
  #print(obs_0[obs_0$cod_mun == "355030",])
  ######3) óbitos 1 a 4 x prop. 2 acima ano -3
  if (idade == 0) {
    obs_1 <- tabsus[cod_mun == "99999x",1:3]}
  else{
  for (i in 1:idade) {
    anol <- dano -(idade - i)
    ind_m <- ifelse(i<5,"mortes 1 a 4 anos","mortes 5 a 9 anos")
    ind_p <- paste0("prop. obitos de ",i," a ",i+1)
    n_o <- paste0("obs_",i)
    assign(n_o, tabsus[ano == anol & indicador == "mortes 1 a 4 anos",1:3] )
    set(get(n_o), j = "Nasc. ou Mortes", value = get(n_o)[,3] * tabm[indicador == ind_p & ano == anol]$valor)
  }
  }
  ######6) Estimativa de população 4 a 4 + - Consolida 1 a 6
  indic <- paste0("população ",idade," anos")
  obss <- mget(ls(pattern = "obs_\\d"))
  pop_var <-  nasc %>% full_join(.,Reduce(function(...) full_join(..., by = c("cod_mun","Município")),
                                          obss), by = c("cod_mun","Município")) %>%
    mutate_if(is.numeric,coalesce,0) %>%
    transmute('cod_mun' = cod_mun,'Município' = Município, 'população' = indic, ano = dano,
              'valor' = `nascimentos`-
                floor(rowSums(select(., starts_with("Nasc. ou Mortes")))))
#  print(summary(pop_var))
#  pop_var[pop_var$cod_mun == "355030",]
}

combosf <-  expand.grid(anosel,0:9)
names(combosf) <- c("dano","idade")

#Problema com a funçao - 0 a 1
pop_var <- rbindlist(lapply(1:nrow(combosf),
                            function(x) do.call(mapply,c("calc_id",combosf))[,x]))

#Eliminar municípios sem nome / totais
pop_var <- pop_var[!(is.na(pop_var$Município)),]

#Considerar ao menos 1 criança em cada faixa etária
pop_var[valor < 1,]$valor <- 1


#Formatando longo para largo para facilitar subtotais
pop_var_cols <- pop_var %>% spread(população,valor) %>%
  mutate_if(is.numeric,coalesce,0)



#Subtotais e proporções a aplicar
pop_var_fx <- pop_var_cols %>% transmute(cod_mun = cod_mun,Município = Município, ano = ano,
                                   pop_0a4 = rowSums(select(.,matches("população [0-4]"))),
                                   prop_0a3 = rowSums(select(.,matches("população [0-3]")))/
                                     rowSums(select(.,matches("população [0-4]"))),
                                   prop_4 = rowSums(select(.,matches("população 4")))/
                                     rowSums(select(.,matches("população [0-4]"))),
                                   pop_5a6 = rowSums(select(.,matches("população [5-6]"))),
                                   pop_5a9 = rowSums(select(.,matches("população [5-9]"))),
                                   prop_5a6 = (rowSums(select(.,matches("população [5-6]"))))/ 
                                     rowSums(select(.,matches("população [5-9]")))
                                   ) %>% mutate_if(is.numeric,coalesce,0.05)


popm_inf <- popmf[faixa_etaria %in% c("0 a 4 anos","5 a 9 anos") & !(is.na(cod_mun)),] %>% 
  spread(.,faixa_etaria,populacao) %>% 
  left_join(.,pop_var_fx, by = c("cod_mun","Município","ano")) 

#Corrigir NAs de municípios sem registros ref. datasus
muns_nas <-  popm_inf[is.na(popm_inf$pop_0a4),]$cod_mun 
popm_inf[is.na(popm_inf$pop_0a4),4:10] <- popm_inf[popm_inf$cod_mun %in% muns_nas & popm_inf$ano == 2013, 4:10]
muns_nas5 <- popm_inf[is.na(popm_inf$pop_5a6),]$cod_mun 
popm_inf[is.na(popm_inf$pop_5a6),4:10] <- popm_inf[popm_inf$cod_mun %in% muns_nas5 & popm_inf$ano == 2013, 4:10]
#

popm_inf <- popm_inf %>% transmute(cod_mun = cod_mun, Município = Município, ano = ano,
                      `0 a 3 anos` = `0 a 4 anos`*prop_0a3,
                      `4 a 6 anos` = `0 a 4 anos`*(prop_4) +`5 a 9 anos`*(prop_5a6),
                      `7 a 9 anos` = `5 a 9 anos` - `5 a 9 anos`*(prop_5a6))
popm_inf <- popm_inf  %>% mutate_if(is.numeric,coalesce,0)



saveRDS(popm_inf, "data/estimativas_pop_1a_infancia.rds", compress = "gzip")

popmf <- popmf[Município != "TOTAL",]
popmfj <- popmf %>% pivot_wider(names_from = faixa_etaria, values_from = populacao )
popmf_det <- popm_inf[,-7] %>% full_join(popmfj, 
                                         by = c("cod_mun","Município","ano"))
