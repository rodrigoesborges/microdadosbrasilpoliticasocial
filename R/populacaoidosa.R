#Estimativas de idosos 2019



## 60 anos ou mais

popmf20182019 <- popmunicipal(2018,2020)

faixastodas <- unique(popmf20182019$faixa_etaria)

faixaidosos <- faixastodas[13:17]


popidosa <- 
  popmf20182019[popmf20182019$ano == 2019 &
                  substr(popmf20182019$cod_mun,1,2) == 32,]

popidosa <- popidosa %>% pivot_wider(names_from = faixa_etaria, 
                                         values_from = populacao)

popidosa <- popidosa %>% mutate(`população total` = round(rowSums(.[-1:-3]),0))

popidosa <- popidosa %>% mutate(`60 anos ou mais` = round(rowSums(.[16:20]),0))

popidosa <- popidosa %>% mutate(`65 anos ou mais` = round(rowSums(.[17:20]),0))

popidosa <- popidosa %>% mutate(`70 anos ou mais` = round(rowSums(.[18:20]),0))


popidosa <- popidosa[,21:24]

popidosa <- popidosa %>% mutate(prop60mais = round(100*`60 anos ou mais`/`população total`,2),
                                prop65mais = round(100*`65 anos ou mais`/`população total`,2),
                                prop70mais = round(100*`70 anos ou mais`/`população total`,2))

write.csv2(popidosa,"data/popidosa2019.csv", row.names = F)


## 65 anos ou mais



## 70 anos ou mais





## 75 anos ou mais








##80 anos ou mais
