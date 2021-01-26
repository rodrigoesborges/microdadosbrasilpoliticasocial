library(tidyverse)
library(ggthemes)
library(extrafont)
library(cowplot)
library(munipopsbr)
library(sidrar)
library(zoo)
library(xts)
extrafont::loadfonts(device = "pdf",quiet = F)

pop_br <- sidrar::get_sidra(5918, 606, c("200001-202003"),classific = list("c58"))

idosos_br <- pop_br %>% filter(`Grupo de idade (Código)` == 3302)%>%select(`Trimestre (Código)`,Valor)

names(idosos_br) <- c("periodo","qtd_idosos")

idosos_br$periodo <- as.Date(as.yearqtr(idosos_br$periodo,"%Y%q")+0.25)-1

idosos_br$periodo <- as.yearmon(idosos_br$periodo)


idosos_br$qtd_idosos <- 10^3*idosos_br$qtd_idosos

idosos_mensal <-  data.frame(periodo = yearmon(2000.03+0:248/12))

idosos_mensal <- idosos_mensal %>% left_join(idosos_br)

idosos_mensal["qtd_idosos"] <- na.spline(idosos_mensal$qtd_idosos)

idosos_mensal <- idosos_mensal %>% filter(!is.na(qtd_idosos))

prop_60_64 <- get_sidra(7358, 606) 

prop_60_64 <- prop_60_64 %>% select(-5:-6) %>% filter(Ano > 1999, Sexo == "Total",
                                  `Idade (Código)` %in% c(93095:93098,113623,49109,49110), Ano < 2021)

prop_60_64 <- prop_60_64 %>% select(Ano, Idade,Valor)

propos <- prop_60_64 %>% pivot_wider(names_from = Idade, values_from = Valor)


propos$idosos <- rowSums(propos[,-1])
proporcao <- propos$`60 a 64 anos`/propos$idosos
proporcao <- data.frame(periodo = as.Date(paste(2000:2020,"07-01",sep="-")),
                        proporcao = proporcao)

prop_mensal <- data.frame(periodo = as.Date(yearmon(2000.03+0:248/12)))

prop_mensal <- prop_mensal %>% left_join(proporcao)

prop_mensal$proporcao <- na.spline(prop_mensal$proporcao)

idosos65_mensal <- prop_mensal %>% left_join(idosos_mensal%>%mutate(periodo = as.Date(periodo)))

idosos65_mensal <- idosos65_mensal %>% transmute(periodo,qtd_idosos = (1-proporcao)*qtd_idosos)

bpc <- read_csv("data/bpc/visdata3-download-18-12-2020 123715.csv")

bpc$Referência <- as.Date(paste("01",bpc$Referência,sep = "/"), format = "%d/%m/%Y")



datas <- data.frame(periodo = as.Date(c("2019-06-18","2020-04-02")),
                    lei = c("Lei 13.846/2019","Lei 13.982/2020" ))


bpc_filt <- bpc %>% filter(lubridate::year(Referência) > 1999)
ggplot(bpc_filt, aes(x = Referência, y = `Idosos que recebem o Benefício de Prestação Continuada (BPC) por Município pagador`))+
         geom_line()+theme_cowplot(font_family = "DroidSerif")+
  ggtitle("BPC no Brasil")+
  scale_x_date(aes(angle = 70),date_breaks = "3 month", date_labels = "%b/%y")+
  scale_y_continuous(labels = scales::comma_format(big.mark ="."))+
  xlab("Período")+ylab("Beneficiados por BPC")+
  geom_vline(aes(xintercept = datas[1,1]), linetype="dotted")+
  geom_text(aes(x = (datas[1,1]-10), y = (mean(as.numeric(bpc_filt[[2]]))-50000), label = datas[1,2], angle = 90, family = "DroidSerif", fontface = "plain"))+
  geom_vline(aes(xintercept = datas[2,1]), linetype="dotted")+
  geom_text(aes(x = (datas[2,1]-10), y = (mean(as.numeric(bpc_filt[[2]]))-50000), label = datas[2,2], angle = 90, family = "DroidSerif", fontface = "plain"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  geom_segment(aes(x = as.Date("2018-10-01"), y = 2075000,xend = as.Date("2020-09-30"), yend = 2075000),linetype = "dashed")+
  geom_text(aes(x = datas[1,1], y = 2085000, label = "Política casuística"))


bpc_cob <- bpc_filt %>% left_join(idosos65_mensal, by = c("Referência" = "periodo"))

bpc_cob$cobertura <- bpc_cob[[2]]/bpc_cob[[3]]

ggplot(bpc_cob, aes(x = Referência, y = 100*cobertura))+
  geom_line()+theme_cowplot(font_family = "DroidSerif")+
  ggtitle("BPC no Brasil")+
  scale_x_date(aes(angle = 70),date_breaks = "3 month", date_labels = "%b/%y")+
  scale_y_continuous(labels = scales::comma_format(big.mark ="."))+
  xlab("Período")+ylab("Idosos (65+) cobertos pelo BPC - % ")+
  geom_vline(aes(xintercept = datas[1,1]), linetype="dotted")+
  geom_text(aes(x = (datas[1,1]-10), y = (mean(bpc_cob$cobertura)), label = datas[1,2], angle = 90, family = "DroidSerif", fontface = "plain"))+
  geom_vline(aes(xintercept = datas[2,1]), linetype="dotted")+
  geom_text(aes(x = (datas[2,1]-10), y = mean((bpc_cob$cobertura)), label = datas[2,2], angle = 90, family = "DroidSerif", fontface = "plain"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))
