###Dados básicos de pobreza e desigualdade
#install.packages("ipeadatar")
remotes::install_github("tomasbarcellos/RCEPAL")
require(ipeadatar)
require(tidyverse)
require(readxl)
require(magrittr)
require(RCEPAL)
require(wbstats)
  
ipeapobrezas <- ipeadatar::search_series("pobreza")
#Extrema pobreza - DISOC_PIY
ext_pobr <- "DISOC_PIY"
pobrezaipea <- ipeadatar::ipeadata(ext_pobr)

pobrezaipea %<>% select(date,ipea = value)
plotly::ggplotly(ggplot(pobrezaipea[2:3],aes(date, ipea))+geom_line()+
  theme_minimal()+
  ggtitle(label = "Proporção de Pessoas na Extrema Pobreza",
          subtitle = "Brasil - 1976 a 2014"))



##sagi e censo
#VIS data 3 - SAGI/MDS
#Fonte - https://aplicacoes.mds.gov.br/sagi/vis/data3/v.php?q[]=5ItjcmeRn%2FSh1aLByMiFcoiRhX5hhoNzgIuIg5h2dHqkfYXn3X2Me6CVuaOw1LyN3My8q7Ccy7fcs35v6sHXwYW8tODlrgfs%2BOyzZc%2FBzKLosIq6nmmK7OvWvLfUgae9ysWYvJJ5z9njvpOpyGzBsXekvbyjyHOGqsbHudoM5a%2FgsMOLy7tzi62hmGKoxneEmIt3wuWc
#Censo 2010 Pop Urbana em extrema pobreza= 8.673.845

cu_ext_pob <- read_csv("data/unequality/2020-10-31-visdata3-pessoas em extrema pobreza CADUNICO.csv")

cu_ext_pob %<>% transmute(date = as.Date(paste0("01/",Referência),tryFormats = "%d/%m/%Y"),
                       value = `Pessoas em situação de extrema pobreza inscritas no Cadastro Único`)

cu_ext_pob %<>% group_by()

#pobrezaiets <- 
#https://www.iets.org.br/IMG/xls/pnad_-_linhas_de_pobreza_-_1985-2014.xls
iets_linnk <- "https://www.iets.org.br/IMG/xls/pnad_-_linhas_de_pobreza_-_1985-2014.xls"
iets_linind <- "https://www.iets.org.br/IMG/xlsx/pnad_-_indicadores_de_pobreza_-_2014.xlsx"

iets_linnk_ext <- "https://www.iets.org.br/IMG/xls/pnad_-_linhas_de_indigencia_-_1985-2014.xls"
iets_linind_ext <- "https://www.iets.org.br/IMG/xlsx/pnad_-_series_-_resumo_de_indigencia_-_1990-2014.xlsx"

f <- tempfile()
download.file(iets_linind_ext, f)

linhas_ext <- read_excel(f)
names(linhas_ext)[-1] <- linhas_ext[2,-1]
tit_iets <- names(linhas_ext)[1]
names(linhas_ext)[1] <- linhas_ext[1,1]
linhas_ext <- linhas_ext[c(3:14,35:45),]

linhas_ext$Ano <- paste0(linhas_ext$Ano,"-01-01")
linhas_ext$Ano <- as.Date(linhas_ext$Ano, tryFormats = "%Y-%m-%d")

linhas_ext$Proporção <- as.numeric(linhas_ext$Proporção)

iets_ext <- linhas_ext%>%select(Ano, iets = Proporção)
iets_ext %<>% mutate(iets = iets*100)
#pobrezacepal <- 
tabelascepal <- RCEPAL::series_CEPAL()

tabelascepal[grepl("pobreza extrema",tabelascepal$nome_serie),]
##Séries 3328 e 3347

cepal_pob_ext <- RCEPAL::CEPAL_stat(3328)
##Pegar para o Brasil
cepal_pob_ext %<>% filter(iso3 == "BRA", `Pobreza extrema y pobreza` == 1363,
                          `Area geográfica_(EH)` == 1365)

cepal_pob_ext %<>% select(date = Años_desc,cepal = valor)

cepal_pob_ext %<>% mutate(date = as.Date(paste0(date,"-01-01",tryFormats = "%Y-%M-%D")))

##pobrezaBM
View(wb_search("poverty"))
wb_ext_pov <- "1.0.HCount.1.90usd"

wb_br_ext_pov <- wb_data(wb_ext_pov,country = "brazil", lang = "pt")
wb_br_ext_pov %<>% transmute(date = paste0(date,"-01-01"),banco_mundial = `1.0.HCount.1.90usd`)
wb_br_ext_pov$date <- as.Date(wb_br_ext_pov$date,tryFormats = "%Y-%m-%d")

##Junção
pobrezas <- full_join(pobrezaipea,iets_ext,by = c("date" = "Ano")) %>% 
  full_join(cepal_pob_ext) %>% full_join(wb_br_ext_pov)

write_csv2(pobrezas,"data/unequality/pobreza_extrema_proporcoes.csv")

pobrezas %<>% pivot_longer(-1,"instituto")
ggplot(pobrezas,aes(date,value, colour = instituto))+
  geom_line(size = 2, show.legend = T)+
  scale_color_viridis_d(aesthetics = "fill")+
  theme_minimal()+
  ggtitle(label = "Proporção de Pessoas na Extrema Pobreza",
          subtitle = "Brasil - 1976 a 2014")


