#Requisitos
#Pacote datasus
devtools::install_github("rodrigoesborges/datasus", force = T)
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


#C) Nascidos Vivos idade da mãe
nasc_v_idadem <- rbindlist(lapply(anosel,
                                      function(x) data.frame(ano = x, sinasc_nv_mun(periodo = x, coluna = "Idade da mãe"), 
                                                             stringsAsFactors = F)), fill = TRUE) %>%
  separate(Município,into = c("cod_mun","Município"), sep = " ",extra = "merge", fill = "right") %>%
  gather(faixa_etaria_mae, valor, c(-1,-2,-3)) %>% mutate(faixa_etaria_mae = gsub("X","",gsub('\\.'," ",faixa_etaria_mae)))

nasc_v_idadem[is.na(nasc_v_idadem)] <- 0
  


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

#F) Consultas pré-natal
nasc_n_consultas <- rbindlist(lapply(anosel,
                               function(x) data.frame(ano = x, sinasc_nv_mun(periodo = x, coluna = "Consult pré-natal"), 
                                                      stringsAsFactors = F)), fill = TRUE) %>%
  separate(Município,into = c("cod_mun","Município"), sep = " ",extra = "merge", fill = "right") %>%
  gather(indicador, valor, c(-1,-2,-3)) %>% mutate(indicador = gsub("X","",gsub('\\.'," ",indicador)))

nasc_n_consultas[is.na(nasc_n_consultas)] <- 0


#G) Quantidade de equipes de saúde por município por esfera_juridica

