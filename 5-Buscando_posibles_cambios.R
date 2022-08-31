arcos2=arcos # Reserva
total2=total # Reserva para comparar
lista3=seq(1:n)[-c(1,lista2)] # Nodos distintos al 1 que no inician rutas

# Posibles cambios de arco llegada al nodo 1
x=arcos[which(arcos[,2]==1),3] # Vector de costes de los arcos de llegada a 1
ult=arcos[which(arcos[,2]==1),1] # Nodos actuales de llegada
ultp=ult # Reserva de los nodos actuales de llegada
for (ruta in 1:r){ # Comprobamos la posibilidad de llegar al nodo 1 por otro camino
  for (i in 2:n){
    if (sum(arcos[which(arcos[,4]==ruta),1]==i)!=0){
      if (C[i,1]<x[ruta]){
        if (sum(i==arcos[which(arcos[,2]==1),1])==0){
          x[ruta]=C[i,1]
          ult[ruta]=i
        }
      }
    }
  }
  if (x[ruta]!=arcos[ruta,3]){
    print(paste('Nodo',ult[ruta],'nuevo nodo de llegada al CHUS en la ruta',ruta))
  }
  else {
    print ('No hay cambios')
  }
}
ultc=ult[which(ult!=ultp)] # Conjunto de nodos que cambian

# Posibles cambios intermedios, tanto en la misma ruta como en rutas distintas
# De momento, criterio manual. Posible establecer una forma automática
for (i in lista3){
  x2=arcos[which(arcos[,1]==i),3]
  for (j in 2:n){
    if (i!=j){
      if (C[i,j]<x2 & arcos[which(arcos[,2]==i),1]!=j & arcos[which(arcos[,1]==i),2]!=j){
        x2=C[i,j]
        enlace=j
      }
    }
  }
  if (x2!=arcos[which(arcos[,1]==i),3]){
    print (c(i,enlace))
  }
  else{
    print ('No hay cambios')
  }
}

# Entre los distintos nodos de inicio de ruta no enlazados con el nodo 1, vemos posibles inicios de ruta nuevos 
for (i in lista2){
  x3=arcos[which(arcos[,1]==i),3]
  for (j in 2:n){
    if (i!=j){
      if (C[i,j]<x3 & arcos[which(arcos[,1]==i),2]!=j){
        x3=C[i,j]
        enlaceini=j
      }
    }
  }
  if (x3!=arcos[which(arcos[,1]==i),3]){
    print (c(enlaceini,i))
  }
  else{
    print ('No hay cambios')
  }
}

# Más reservas
arcos3=arcos
total3=total
lista22=lista2