#tabuas_mortalidade_ibge-2006-2011

narq <- sapply(paste0(dtibge,basep,anosel),list.files, pattern = flt, ignore.case = T)

narq[6] <- sapply(paste0(dtibge,basep,lapply(anosel[6],paste0,"/ods")),list.files, pattern = flt)



for (i in 1:(nrow(nm_tabuas)-1)) { 
  nome <- paste0("tm_ibge",nm_tabuas$ano[i])
print(nome)
assign(nome,cbind(read_excel(nm_tabuas$arqf[i], range = "A6:g46"), ano = nm_tabuas$ano[i]))
pedaco2 <- cbind(read_excel(nm_tabuas$arqf[i], range = "a62:g103"), ano = nm_tabuas$ano[i])
assign(nome,rbind(get(nome),pedaco2))
rm(pedaco2)
}

tm_ibge2011 <- cbind(read_ods(nm_tabuas$arqf[6], range = "a6:g45"), ano = nm_tabuas$ano[6])
pedaco2 <- cbind(read_ods(nm_tabuas$arqf[6], range = "a62:g102")
                 , ano = nm_tabuas$ano[6])
tm_ibge2011 <- rbind(tm_ibge2011,pedaco2)
rm(pedaco2)
