rm(list = ls())
# ICI



# Carga de los datos
listaDocumentos <- list(c("mp","Juan","Christofer"),c("of","av01","ampr"),c("of","av01","ante"),
                        c("of","av08","arme"),c("of","av02","ante"),c("of","av07","ampr"),
                        c("of","av03","dape"),c("of","av01","meca"),c("of","av02","dape"),
                        c("mp","Antonia"),c("mp","Christian","Mario"),
                        c("mp","Jose","Pedro","Antonela"),c("of","av05","meca"),
                        c("of","av04","dape"),c("of","av02","arme"))


# pregunta 1
estadisticas <- list()
for(documento in listaDocumentos){
  if(documento[1] == "mp"){
    # variable que detecta si encontro o no una estadistica
    encontro <- F
    # revisando si la estadistica tiene datos
    if(length(estadisticas) != 0){
      # revisando las estadisticas guardadas
      for (posicionEstadistica in 1:length(estadisticas)) {
        # se sacan los elementos de  la lista
        estadisticaunlist <- unlist(estadisticas[posicionEstadistica])
        # se revisa si la estadistica existe y se actualiza
        if(estadisticaunlist[1] == (length(documento)-1)){
          estadisticaunlist[2] <- estadisticaunlist[2]+1
          estadisticas[posicionEstadistica] <- list(c(estadisticaunlist))
          encontro <- T
        }
      }
    }
    # se crea una nueva estadistica de algo on registrado
    if(!encontro){
      nuevaEstadistica <- c()
      nuevaEstadistica[1] <- (length(documento)-1)
      nuevaEstadistica[2] <- 1
      estadisticas <- c(estadisticas,list(c(nuevaEstadistica)))
    }
  }
}
# imprimiendo estadistica
for (estadistica in estadisticas) {
  print(paste("Se cuentan con",estadistica[2],"mp de",estadistica[1],"niños"))
}



