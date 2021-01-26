library(RSIDRA)
library(readODS)
library(ipeaData)
library(xlsx)
library(dplyr)
library(tidyr)

tabsidra <- tabelas_SIDRA


tabsidra <- tabsidra[grepl("população",tabsidra$descricao),]


ceitbr <- API_SIDRA(2072)

rendabruta <- ceitbr[ceitbr$`Variável (Código)`== 936 & grepl("4$",ceitbr$`Trimestre (Código)`),c(3,11)]
rendabruta$Ano <- as.numeric(gsub("04$","",rendabruta$`Trimestre (Código)`))

#estimapop <- API_SIDRA(6579)

#ftp://ftp.ibge.gov.br/Projecao_da_Populacao/Projecao_da_Populacao_2013/projecoes_2013_populacao_ods.zip

estimapop <- read_ods("data/unequality/PROJECOES_2013_POPULACAO.ods", range = "A196:bj289")

names(estimapop) <- read_ods("data/unequality/PROJECOES_2013_POPULACAO.ods", range = "A195:bj195")

poptot <- estimapop[estimapop$IDADE == "TOTAL",]

poptot <- poptot %>% pivot_longer(-1, names_to = "Ano", values_to = "População")
poptot <- poptot[!(is.na(poptot$População)),]
poptot$Ano <- as.numeric(poptot$Ano)

rendabrutapc <- rendabruta %>% left_join(poptot)

rendabrutapc <- rendabrutapc[,-1]

rendabrutapc$Valor <- rendabrutapc$Valor*1000000

rendabrutapc$rendabrutapc <- rendabrutapc$Valor/rendabrutapc$População




lpais <- "BR"
wbgini <- "SI.POV.GINI"
ginidados <- wb(indicator = wbgini, startdate = 1970, enddate = 2017, country = lpais)
ginidados$date <- as.numeric(ginidados$date)

ginidados <- ginidados[,c(2,3)]
names(ginidados) <- c("Ano", "gini")

rendabrutapc <- rendabrutapc %>% left_join(ginidados)

rendabrutapc$renda80 <- 1.1*rendabrutapc$rendabrutapc*(1-rendabrutapc$gini/100)
rendabrutapc$renda70 <- 1*rendabrutapc$rendabrutapc*(1-rendabrutapc$gini/100)
rendabrutapc$renda50 <- 0.725*rendabrutapc$rendabrutapc*(1-rendabrutapc$gini/100)
rendabrutapc$renda40 <- 0.6357*rendabrutapc$rendabrutapc*(1-rendabrutapc$gini/100)

rendabrutapc <- rendabrutapc[,c(2,1,4:10)]

rendabrutapc <- rendabrutapc %>% pivot_longer(-1, names_to = "indicador", values_to = "valor")

desrendabr <- rendabrutapc[!(rendabrutapc$indicador %in% c("Valor", "gini","População")),]
desrendabr <- desrendabr[!(is.na(desrendabr$valor)),]

desrendabr$renda <- round(desrendabr$valor/3,2)

### agora deflacionar pelo INPC
#Adiciona coluna para rendimento equivalente a R$ 178 a preços de 2020 (deflac IPC)

ipccod <- "INPC"
ipc <- ipeadata(ipccod, type = data.table)
#Anualizar por últimomes
ipc <- ipc[MES == 12 ,]
#subconjunto apenas nível brasil, a partir de 2000
ipc <- ipc[ANO > 1999 & NIVNOME == "Brasil",]


#apenas ano e valor
ipc <- ipc[,5:6]
names(ipc)[1:2] <- c("Ano","INPC")
#mudar a base - obter o divisor para renda real (2019=100)
#print(ipc)
ipc$INPC <- ipc$INPC*100/ipc$INPC[nrow(ipc)]

ipc$multreflaciona <- 100/ipc$INPC

###

desrendabr <- desrendabr %>% left_join(ipc)

desrendabr <- desrendabr %>% mutate (`Renda Mensal Média em R$ de 2019` = renda*multreflaciona)

grafdesig <- ggplot(desrendabr, aes(x = Ano, y = `Renda Mensal Média em R$ de 2019`, color = `indicador`))+
  theme_classic()+
  #  theme(legend.position="right", legend.title = element_blank())+
  geom_line(size = 1) +
  scale_color_brewer(palette = "Purples")+
  scale_x_continuous(breaks = c(seq(2000,2020, 5)))+
  scale_y_continuous("Renda Mensal (R$ de 2019)",labels = scales::comma_format(big.mark=".", decimal.mark = ","))+
  labs(title = "Brasil Desigualdade de Renda Individual - 2000 a 2019",
       caption = "* Sem descontar os impostos líquidos subtraídos da grande maioria dos trabalhadores\n
       pela política de superávit primário e entregues à burguesia\n
       (3 pessoas por domicílio cf. IBGE - PNADc- tab 6578)")



ggplotly(grafdesig)

htmlwidgets::saveWidget(ggplotly(grafdesig),file.path(getwd(),"data/unequality/brasil_desigualdade.html"), selfcontained = T)


###Calcula indicador da renda média familiar 
#a partir do dado do IBGE tabela 6578, média domicílio = 3

desrendabr$rendadomiciliar <- desrendabr$`Renda Mensal Média em R$ de 2019`*3

grafdesigd <- ggplot(desrendabr, aes(x = Ano, y = rendadomiciliar, color = `indicador`))+
  theme_classic()+
  #  theme(legend.position="right", legend.title = element_blank())+
  geom_line(size = 1) +
  scale_color_brewer(palette = "Purples")+
  scale_x_continuous(breaks = c(seq(2000,2020, 5)))+
  scale_y_continuous("Renda Mensal Domiciliar (R$ de 2019)*",labels = scales::comma_format(big.mark=".", decimal.mark = ","))+
  labs(title = "Brasil Desigualdade de Renda nos Domicílios - 2000 a 2019",
          caption = "* Sem descontar os impostos líquidos subtraídos da grande maioria dos trabalhadores\n
       pela política de superávit primário e entregues à burguesia\n
       (3 pessoas por domicílio cf. IBGE - PNADc- tab 6578)")
  




ggplotly(grafdesigd)

htmlwidgets::saveWidget(ggplotly(grafdesigd),file.path(getwd(),"data/unequality/brasil_desigualdade_domiciliar.html"), selfcontained = T)

tabelarendas <- desrendabr[ ,c("Ano", "indicador", "Renda Mensal Média em R$ de 2019" )]

tabelarendas <- tabelarendas %>% pivot_wider(names_from = indicador, values_from= `Renda Mensal Média em R$ de 2019`)


write.xlsx(tabelarendas,"data/unequality/tabela_desigualdade_renda_individual_BR.xlsx")


tabelarendasf <- desrendabr[ ,c("Ano", "indicador", "rendadomiciliar" )]

tabelarendasf <- tabelarendasf %>% pivot_wider(names_from = indicador, values_from= rendadomiciliar)


write.xlsx(tabelarendasf,"data/unequality/tabela_desigualdade_renda_domiciliar_BR.xlsx")

