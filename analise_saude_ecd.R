#Requisitos
#Pacote datasus
devtools::install_github("rodrigoesborges/datasus")
require(datasus) 
require(tidyverse)
require(dplyr)
require(data.table)

        

#Estimativas de população 
popm_inf <- popm_inf
  #read.csv2("data/")
anosel <- 2012:2017



#indicadores de saúde de primeira infância
#A)Mortalidade no parto
mortes_parto <- rbindlist(lapply(anosel,
                             function(x) data.frame(ano = x, sim_mat10_mun(periodo = x, grupo_cid10 = c(159,160,163,165)),
                                                    indicador ="Mortes relativas ao parto", stringsAsFactors = F))) %>%
  separate(Município,into = c("cod_mun","Município"), sep = " ",extra = "merge", fill = "right")

mortes_parto[is.na(mortes_parto)] <- 0

#Para construir indicador o melhor seria mortalidade no parto em relação ao número de mulheres em idade fértil no município - 15 a 49(?)
#Atualmente simplesmente dividido por 2 da faixa correspondente já disponível

##Estimativa mulheres em idade fértil
popmf_det$mulheresf <- (popmf_det$`15 a 19 anos`+
  popmf_det$`20 a 24 anos`  + popmf_det$`25 a 29 anos`+
  popmf_det$`30 a 34 anos`  + popmf_det$`35 a 39 anos`+
  popmf_det$`40 a 44 anos`  + popmf_det$`45 a 49 anos`)/2

#indicador de mortes no parto por 1.000 mulheres na idade fértil
mortes_parto_ind <- mortes_parto %>% select(1:4) %>% right_join(popmf_det[,c(1,2,3,length(popmf_det))], by = c("cod_mun","Município","ano")) 

mortes_parto_ind[is.na(mortes_parto_ind)] <- 0

mortes_parto_ind$mortes_por_1000 <- 1000*mortes_parto_ind$Óbitos.mulheres.idade.fértil/mortes_parto_ind$mulheresf

#Indicador de mortes no parto a cada 1.000 partos de nascidos vivos
#Estimativa de n. de partos de nascidos vivos

nasc_v_est_partos <- rbindlist(lapply(anosel,
                                      function(x) data.frame(ano = x, sinasc_nv_mun(periodo = x, coluna = "Tipo de gravidez"), 
                                                             stringsAsFactors = F))) %>%
  separate(Município,into = c("cod_mun","Município"), sep = " ",extra = "merge", fill = "right") 
nasc_v_est_partos <- nasc_v_est_partos %>% 
  transmute(cod_mun = cod_mun, ano = ano, Município = Município, estimativa_partos = Total - Dupla - 2*Tripla.e.mais)
#  gather(indicador, valor, c(-1,-2,-3)) %>% mutate(indicador = paste("Gravidez ",indicador))

mortes_parto_ind <- mortes_parto_ind %>% left_join(nasc_v_est_partos, by = c("cod_mun", "Município", "ano")) 


#colocar 1 parto para os NAs - provisório pensar melhor depois
mortes_parto_ind[is.na(mortes_parto_ind$estimativa_partos),]$estimativa_partos <- 1


mortes_parto_ind$mortes_por_1000_partos <- 1000*mortes_parto_ind$Óbitos.mulheres.idade.fértil/mortes_parto_ind$estimativa_partos

mortes_parto_ind <- transmute(mortes_parto_ind, cod_mun = cod_mun, Município = Município, ano = ano, 
                              mortes_mil_partos = mortes_por_1000_partos, mortes_por_1000_m = mortes_por_1000)
#B) Tipo de Parto
nasc_v_tipo_parto <- rbindlist(lapply(anosel,
                                    function(x) data.frame(ano = x, sinasc_nv_mun(periodo = x, coluna = "Tipo de parto"), 
                                                           stringsAsFactors = F))) %>%
  separate(Município,into = c("cod_mun","Município"), sep = " ",extra = "merge", fill = "right") %>% 
  gather(indicador, valor, c(-1,-2,-3)) %>% mutate(indicador = paste("Parto",indicador))

#Inclusão de óbitos fetais ?
nasc_m_tipo_parto <-  rbindlist(lapply(anosel,
                                       function(x) data.frame(ano = x, sim_fet10_mun(periodo = x, coluna = "Tipo parto"), 
                                                               stringsAsFactors = F))) %>%
  separate(Município,into = c("cod_mun","Município"), sep = " ",extra = "merge", fill = "right") %>%
  gather(indicador, valor, c(-1,-2,-3)) %>% mutate(indicador = paste("Parto",indicador))

#Partos - proporção de cesarianas no total

tipo_parto_ind <- nasc_v_tipo_parto %>% pivot_wider(names_from = indicador, values_from = valor) %>% 
  full_join(nasc_m_tipo_parto %>% pivot_wider(names_from = indicador, values_from = valor), by = c("cod_mun","Município","ano")) %>%
  mutate_at(vars(ends_with("y")),coalesce,0)

tipo_parto_ind$cesariatotalvivos <- tipo_parto_ind$`Parto Cesário.x`/tipo_parto_ind$`Parto Total.x`


tipo_parto_ind$cesario_total <- (tipo_parto_ind$`Parto Cesário.x`+tipo_parto_ind$`Parto Cesário.y`)/(tipo_parto_ind$`Parto Total.x`+tipo_parto_ind$`Parto Total.y`)

tipo_parto_ind <- tipo_parto_ind %>% transmute(cod_mun = cod_mun, Município = Município, ano = ano,
                                               prop_cesario = cesario_total)

#C) Nascidos Vivos idade da mãe
nasc_v_idadem <- rbindlist(lapply(anosel,
                                      function(x) data.frame(ano = x, sinasc_nv_mun(periodo = x, coluna = "Idade da mãe"), 
                                                             stringsAsFactors = F)), fill = TRUE) %>%
  separate(Município,into = c("cod_mun","Município"), sep = " ",extra = "merge", fill = "right") %>%
  gather(faixa_etaria_mae, valor, c(-1,-2,-3)) %>% mutate(faixa_etaria_mae = gsub("X","",gsub('\\.'," ",faixa_etaria_mae)))

nasc_v_idadem[is.na(nasc_v_idadem)] <- 0
  
###Indicador possível interessante - taxa de fecundidade por faixa etária número de nascidos vivos x 1.000 da faixa:
#Pegar só de adolescentes

m_id_10a19 <- popmf_det[,c(1,2,3,6,9,10,11)] %>% transmute(ano = ano, cod_mun = cod_mun, m_10a_19 = `10 a 14 anos`+`15 a 19 anos`)
fecundidade_adolescente <- nasc_v_idadem %>% 
  filter(faixa_etaria_mae %in% c("10 a 14 anos","15 a 19 anos")) %>%
  pivot_wider(names_from = faixa_etaria_mae,values_from = valor) %>% 
  transmute(ano = ano, cod_mun = cod_mun, Município = Município, `f. 10 a 19 anos` = `10 a 14 anos`+ `15 a 19 anos`) %>%
  right_join(m_id_10a19, by = c("cod_mun","ano")) 

fecundidade_adolescente[is.na(fecundidade_adolescente$`f. 10 a 19 anos`),]$`f. 10 a 19 anos` <- 0
  
fecundidade_adolescente$nascidosporadolescente <- 1000*fecundidade_adolescente$`f. 10 a 19 anos`/fecundidade_adolescente$m_10a_19
fecundidade_adolescente_ind <- fecundidade_adolescente %>% transmute(cod_mun = cod_mun, Município = Município, ano = ano,
                                                                 fecundidade_adolescente = nascidosporadolescente)

#D)Mortalidade por causas evitáveis x Faixa etária x mun
evit_fxet <- rbindlist(lapply(anosel,
                              function(x) data.frame(ano = x, sim_evita10_mun(periodo = x, coluna = "Faixa Etária"), 
                                                     stringsAsFactors = F)), fill = TRUE) %>%
  separate(Município,into = c("cod_mun","Município"), sep = " ",extra = "merge", fill = "right") %>%
  gather(faixa_etaria_crianca, valor, c(-1,-2,-3)) %>% mutate(faixa_etaria_crianca = gsub("X","",gsub('\\.'," ",faixa_etaria_crianca))) 

evit_fxet[is.na(evit_fxet)] <- 0




#E) Nascimento por Local de Ocorrência - hospital x Outro Estab. Saúde x domicílio x aldeia
nasc_local <- rbindlist(lapply(anosel,
                               function(x) data.frame(ano = x, sinasc_nv_mun(periodo = x, coluna = "Local ocorrência"), 
                                                      stringsAsFactors = F)), fill = TRUE) %>%
  separate(Município,into = c("cod_mun","Município"), sep = " ",extra = "merge", fill = "right") %>%
  gather(indicador, valor, c(-1,-2,-3)) %>% mutate(indicador = gsub("X","Nasc. em",gsub('\\.'," ",indicador)))

nasc_local[is.na(nasc_local)] <- 0

#Indicador - partos em hospital % total
nasc_local_ind <- pivot_wider(nasc_local,names_from = indicador, values_from = valor) %>% mutate(nascidos_hospital = Hospital/Total)

nasc_local_ind <- transmute(nasc_local_ind, cod_mun = cod_mun, Município = Município, ano = ano, "nascidos em hospital" = nascidos_hospital)
#F) Consultas pré-natal
nasc_n_consultas <- rbindlist(lapply(anosel,
                               function(x) data.frame(ano = x, sinasc_nv_mun(periodo = x, coluna = "Consult pré-natal"), 
                                                      stringsAsFactors = F)), fill = TRUE) %>%
  separate(Município,into = c("cod_mun","Município"), sep = " ",extra = "merge", fill = "right") %>%
  gather(indicador, valor, c(-1,-2,-3)) %>% mutate(indicador = gsub("X","",gsub('\\.'," ",indicador)))

nasc_n_consultas[is.na(nasc_n_consultas)] <- 0

#indicador - simplesmente a proporção
nasc_n_consultas_ind <- pivot_wider(nasc_n_consultas,names_from = indicador, values_from = valor) %>% 
  transmute(cod_mun = cod_mun, Município = Município, ano = ano, sem_consultas = Nenhuma/Total, 
            insuficiente = (`De 1 a 3 consultas`+`De 4 a 6 consultas`+`Ignorado`)/Total, prenatal_adequado = `7 ou mais consultas`/Total)

#G) Quantidade de equipes de saúde por município por esfera_juridica



indicadores_saude <- mortes_parto_ind %>% full_join(tipo_parto_ind) %>% full_join(fecundidade_adolescente_ind) %>% full_join(nasc_local_ind) %>%
  full_join(nasc_n_consultas_ind) 

write.csv2(indicadores_saude, "data/2012-2017-indicadores_saude_brutos.csv",row.names = F)
