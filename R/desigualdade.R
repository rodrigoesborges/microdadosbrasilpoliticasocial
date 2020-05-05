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

pnurl <- "https://sites.google.com/a/newschool.edu/duncan-foley-homepage/home/EPWT/EPWT%20v%204.0.xls"
download.file(pnurl,"data/desigualdade/epwt4.xls")

dadosrenda <- wiid4
data(pwt9.1)

lpais <- c("BR","CL","MX")
wbrni <- "NY.ADJ.NNTY.PC.KD"
rnidados <- wb(indicator = wbrni, startdate = 1970, enddate = 2017, country = lpais)
wbgini <- "SI.POV.GINI"
ginidados <- wb(indicator = wbgini, startdate = 1970, enddate = 2017, country = lpais)

vmibase <- merge(rnidados,ginidados, by.x=c("iso2c","date","iso3c","country"),by.y=c("iso2c","date","iso3c","country"))

vmibase$vmi <- 1.1*(vmibase$value.x * (100-vmibase$value.y))/100

vmibase$renda70 <- 1*(vmibase$value.x * (100-vmibase$value.y))/100



vmi <- vmibase[,c(2,4,11)]
names(vmi) <- c("ano","pais","vmi")
vmi <- data.table(vmi)

vmi$ano <- as.numeric(vmi$ano)

ggplot(vmi, aes(x = ano, y = vmi, color = pais))+
  theme_classic()+
  #  theme(legend.position="right", legend.title = element_blank())+
  geom_line(size = 1) +
  scale_color_brewer(palette = "Purples")



gwii <- dadosrenda %>% filter(c2 %in% lpais) %>% select (country, year, gini_reported, source_detailed, resource_detailed)


#Para o Brasil (SHAIKH 2014):
#  0,725 <- multiplicador 50% mais pobres
#  63,57 <- multiplicador 40% mais pobres
  