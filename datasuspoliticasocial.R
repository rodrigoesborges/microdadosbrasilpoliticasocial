#Indicadores de Saúde - Acesso ao datasus
require(devtools)
#repo original em : rpradosiqueira/datasus
#fork para adicionar consulta à pirâmide etária
#necessário instalar novamente quando novas funções de coleta forem adicionadas
install_github("rodrigoesborges/datasus")

require(datasus)
popes <- novapop_popbr_mun(unidade_da_federacao = "Espírito Santo")
