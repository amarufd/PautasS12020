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



###
# pregunta 2
listaoficios <- list()
for(documento in listaDocumentos){
  if(documento[1] == "of"){
    # variable que detecta si encontro o no un oficio
    encontro <- F
    # revisando si la estadistica tiene datos
    if(length(listaoficios) != 0){
      # revisando las estadisticas guardadas
      for (posicionoficios in 1:length(listaoficios)) {
        # se sacan los elementos de  la lista
        oficiounlist <- unlist(listaoficios[posicionoficios])
        # se revisa si la estadistica existe y se actualiza
        if(oficiounlist[1] == documento[2]){
          listaoficios[posicionoficios] <- list(c(oficiounlist, documento[3]))
          encontro <- T
        }
      }
    }
    # se crea una nuevoficio
    if(!encontro){
      listaoficios <- c(listaoficios,list(c(documento[2],documento[3])))
    }
  }
}

# imprimiendo oficios
for (oficio in listaoficios) {
  print(oficio)
}

sample(0:1,1)

###
# pregunta 3
aprobados <- 0
rechazados <- 0
conteooficios <- 0
for(documento in listaDocumentos){
  if(documento[1] == "of"){
    # el juez
    juez <- sample(0:1,1)
    if(juez == 1){
      aprobados <- aprobados + 1
    }else{
      rechazados <- rechazados + 1
    }
    # se suma un oficio
    conteooficios <- conteooficios + 1
  }
}
# imprimiendo respuestas del juez
paste("Llegaron", conteooficios ,"oficios de los cuales:", aprobados ,"son  aprobados y", rechazados ,"rechazados")



