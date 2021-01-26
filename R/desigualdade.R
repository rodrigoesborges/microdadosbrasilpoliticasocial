#library("devtools")
#install_github("b-rodrigues/wiid4")
require("wiid4")
#install.packages("pwt9")
require("pwt9")
require("wbstats")
library("data.table")
library("ggplot2")
library("dplyr")
library("plotly")
library("tidyr")

pnurl <- "https://sites.google.com/a/newschool.edu/duncan-foley-homepage/home/EPWT/EPWT%20v%204.0.xls"
download.file(pnurl,"data/unequality/epwt4.xls")

dadosrenda <- wiid4
data(pwt9.1)

#lpais <- c("BR","CL","MX")
lpais <- "BR"
wbrni <- "NY.ADJ.NNTY.PC.KD"
rnidados <- wb(indicator = wbrni, startdate = 1970, enddate = 2017, country = lpais)
wbgini <- "SI.POV.GINI"
ginidados <- wb(indicator = wbgini, startdate = 1970, enddate = 2017, country = lpais)

vmibase <- merge(rnidados,ginidados, by.x=c("iso2c","date","iso3c","country"),by.y=c("iso2c","date","iso3c","country"))

vmibase$vmi <- 1.1*(vmibase$value.x * (100-vmibase$value.y))/100

vmibase$renda70 <- 1*(vmibase$value.x * (100-vmibase$value.y))/100

vmibase$renda50 <- 0.725*(vmibase$value.x * (100-vmibase$value.y))/100

vmibase$renda40 <- 0.6357*(vmibase$value.x * (100-vmibase$value.y))/100

vmi <- vmibase[,c(2,4,11:14)]
names(vmi) <- c("ano","pais","Renda da vasta maioria - 80% da pop","renda dos 70% mais pobres", 
                "renda dos 50% mais pobres",
                "renda dos 40% mais pobres")
vmi <- data.table(vmi)

vmi$ano <- as.numeric(vmi$ano)


vmi <- vmi %>% pivot_longer(-1:-2, names_to = "indicador de desigualdade", values_to = "valor")

vmi$valor <- round(vmi$valor/12,2)

grafdesig <- ggplot(vmi, aes(x = ano, y = valor, color = `indicador de desigualdade`))+
  theme_classic()+
  #  theme(legend.position="right", legend.title = element_blank())+
  geom_line(size = 1) +
  scale_color_brewer(palette = "Purples")+
  scale_x_continuous(breaks = c(seq(1980,2020, 5)))+
  scale_y_continuous("Renda Mensal (US$ de 2010)",labels = scales::comma_format(big.mark=".", decimal.mark = ","))+
  ggtitle("Indicadores de desigualdade do Brasil - 1980 a 2018 - US$ de 2010")



ggplotly(grafdesig)

htmlwidgets::saveWidget(ggplotly(grafdesig),file.path(getwd(),"data/unequality/rendabrasil.html"), selfcontained = T)

gwii <- dadosrenda %>% filter(c2 %in% lpais) %>% select (country, year, gini_reported, source_detailed, resource_detailed)


#Para o Brasil (SHAIKH 2014):
#  0,725 <- multiplicador 50% mais pobres
#  0,6357 <- multiplicador 40% mais pobres
  