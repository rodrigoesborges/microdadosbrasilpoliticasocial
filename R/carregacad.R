####Função para carregar o cadunico para o ano desejado, já com dados 
# relativos ao SM conforme cadunico e montabunico.R


carregacad <- function(anos = 2018,dado = "CadUnico/cadunico 2018 design.rds" ) {
  # lê o objeto com o desenho amostral
  dbloc <- "~/RLocalData/cadunico/MonetDB"
  dbcon <- dbConnect(MonetDBLite(),dbloc)
  dbfora <- db_collect(dbcon, sprintf("SELECT * FROM cadunico_%s",anos))
  dbDisconnect(dbcon)
  #Remonta o objeto survey, sem vínculo a base de dados
  cadunicosvy <-
    svydesign(
      ids = ~id_familia ,
      strata = ~estrato ,
      weight = ~peso_pes ,
      nest = TRUE ,
      data = dbfora
    )
  cadunicosvy
  #Parte anterior, partindo de arquivo RDS gerado por cadunico.R e montabdcadunico.R
  #   pes.design <- readRDS(dado) ;  # # abre a conexão do objeto com a base em MonetDBLite; # pes.design <- open( pes.design, driver = MonetDBLite()) ;  #   pes.design
}
