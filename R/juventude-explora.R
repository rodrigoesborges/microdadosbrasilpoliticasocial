#juventude alguns

require(RSIDRA)
require(datasus)

anodisp <- "2015"
popmf <- data.frame(matrix(ncol = 4, nrow=0))

pmf <- novapop_popbr_mun(periodo = anodisp)
pmf <- pmf %>% gather(faixa_etaria,populacao,-1) %>%
  separate(`Município`, c("cod_mun","Município"),sep="\\s", extra = "merge", fill = "left")
pmf <-  cbind(ano = rep(anodisp,nrow(pmf)),pmf)
popmf <- rbind(popmf,pmf)
}

popmf$ano <- as.numeric(as.character(popmf$ano))
