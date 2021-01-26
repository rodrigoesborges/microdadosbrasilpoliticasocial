# O objetivo deste script é baixar e processar as estimativas
# populacionais do IBGE para construção de indicadores 
# nacionais, estaduais ou municipais com os demais
# microdados disponíveis

#dados municipais
#Tentativa ao máximo de uso de formatos abertos
require('readODS')


# #uf
# uf = 'ES'
# 
# #necessidade de atualização manual
# ibgelink <- 'ftp://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2018/estimativa_TCU_2018_20190213.ods'
# 
# poproj <- 'dadosB/2018ibgepop.ods'
# 
# download.file(ibgelink,poproj)
# 
# 
# #Manualmente revisitar RANGE - subconjunto de celulas
# celplan <- 'A2:E5572'
# folh <- 'Municípios'
# 
# projpop <- read_ods(poproj, sheet = folh, range = celplan, col_names = TRUE)
# 
# 
# #objetivo futuro - construir BD - série municipal .Como:
# # fazer loop pelos arquivos

## Não utilizado
# Dados apenas estaduais - não muito úteis 
# uf = 'ES'
# ibgelink <- 'ftp://ftp.ibge.gov.br/Projecao_da_Populacao/Projecao_da_Populacao_2018/projecoes_2018_populacao_2010_2060.ods'
# 
# poprojuf <- 'dadosB/2018ibgepopuf.ods'
# 
# download.file(ibgelink,poprojuf)
# 
# projpopuf <- read_ods(poproj,sheet = uf)
