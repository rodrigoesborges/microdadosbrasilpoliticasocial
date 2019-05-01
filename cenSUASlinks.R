####Script para baixar lista de links do CENSO SUAS
require("XML")

urlcsuas <- "https://aplicacoes.mds.gov.br/sagi/pesquisas/pes-metadados.php"
sesid <- "u288"
tempor <- tempfile()
download.file(urlcsuas,destfile = tempor, method = 'curl', extra = '-k -L')
# parte abaixo retirado de https://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r
mdsmicro <- htmlParse(tempor)
lnksuas <- xpathSApply(mdsmicro, "//a[contains(text(), 'Censo SUAS')]/following-sibling::a/@href")
lnksuas <- as.vector(lnksuas[!grepl("#",lnksuas)])
