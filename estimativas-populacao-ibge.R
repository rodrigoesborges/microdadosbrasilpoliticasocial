# O objetivo deste script é baixar e processar as estimativas
# populacionais do IBGE para construção de indicadores 
# nacionais, estaduais ou municipais com os demais
# microdados disponíveis

#dados municipais
# A base é o SIDRA
library(RSIDRA)
library(tidyverse)
#library(stringr)
#library(scales)
# ----- dados populacionais básicos -----------------------------------------------------------

#Tabela no SIDRA
#tirar a tabela
#tabela população '6579'
t <- 6579
#Variável retirada através de SIDRA_variaveis(tbpp)
#variavel do SIDRA contagem pop 9324
vpop <- 9324
#nível de desagregação municipal : 6
nivmun <- 6
#periodo
anoin <- 2017
anofim <- 2018
popmun <- API_SIDRA(t, variavel = vpop, nivel = nivmun, inicio = anoin, fim = anofim)

#Cod UF IBGE '32'

#======= Necessidade da pirâmide etária=========
#===== 1) 2010 = Pirâmide etária municipal / pirâmide etária estadual = fator para cada faixa
#======2) Pir. Etária Estadual 2017-2018
#======3) Aplicar os fatores de 1 em 2
# Passos:
#   
#   Sinopse do Censo 2010 - ibge
# 
# 
# Exemplo URL Cidade de Vitória
# https://censo2010.ibge.gov.br/sinopse/webservice/frm_piramide.php?codigo=320530