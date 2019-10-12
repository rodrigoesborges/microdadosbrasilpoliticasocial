# Função para colocar valor de 1 no lugar de erros, não 0 para evitar divisão infinita
zr <- function(codigo) {
  tcodigo <- try(codigo)
  if(!inherits(tcodigo, "try-error")) {
    tcodigo
  } else {
    1
  }
}
