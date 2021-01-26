#gera tabela de compatibilidade códigos de municípios DATASUS e ibge
require(dplyr)
municodigos <- municodl %>% mutate(cod_mun = substr(codigomun,1,6)) %>% separate(municipio, into = c("Município","UF"), sep = " - ", fill = "left")

write.csv2(municodigos, "data/tabcodigosmunibgedatasus.csv", row.names = F)
