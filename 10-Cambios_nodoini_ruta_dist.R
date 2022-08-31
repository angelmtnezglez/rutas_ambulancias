# Introducción de un nodo en una ruta distinta
# Para incluirlo como nuevo inicio de ruta
nuevoini=numeric(2)
l=1
for(i in 1:rut){ # Comprobamos los nodos que inician ruta
  if(sum(nuevoarco==lista2[i])!=0){
    nuevoini[l]=lista2[i]
    l=l+1
  }
}
if (sum(nuevoini==0)==0){ # Si ambos nodos son inicio de ruta
  rutasini=c(arcos[which(arcos[,1]==nuevoini[1]),4],arcos[which(arcos[,1]==nuevoini[2]),4]) # Rutas de cada nodo
  nuevoini=nuevoini[which.min(tiemporutas[rutasini])] # Elegimos el nodo con la ruta de menor coste.
}
if (sum(nuevoini==0)!=0){ # Si solo hay un nodo que inicia ruta, nos quedamos con él.
  nuevoini=nuevoini[-which(nuevoini==0)]
}
nuevoini=nuevoarco[-which(nuevoarco==nuevoini)] # El nodo que inicia la ruta a modificar será elegido.
anteini=nuevoarco[-which(sum(nuevoarco==nuevoini)!=0)] # Nos quedamos con el nodo que iniciaba la ruta.
k=arcos[which(arcos[,1]==nuevoini),2] # Reservamos el nodo que acompaña al nuevo inicio de ruta.
arcos[which(arcos[,1]==nuevoini),2]=anteini # Ponemos el inicio anterior con el nuevo.
arcos[which(arcos[,1]==nuevoini),4]=arcos[which(arcos[,1]==anteini),4] # Actualizamos la ruta a este arco.
if (sum(nuevoini==arcos[,2])>0){ # Si el nodo cambiado era intermedio, introducimos el anterior acompañamiento.
  arcos[which(arcos[,2]==nuevoini),2]=k
}
for (i in 1:(n-1)){ # Actualizamos costes.
  arcos[i,3]=C[arcos[i,1],arcos[i,2]]
}
for (i in 2:n){
  if (sum(arcos[,c(1:2)]==i)==1){
    lista2[arcos[arcos[,1]==i,4]]=i
  }
}
total=sum(arcos[,3]) # Calculamos el nuevo coste total
if (total>total2){ # Nuevo total no mejora el anterior. No se hacen los cambios
  arcos=arcos2
  total=total2
  lista2=lista22
}
if (total<=total2){
  arcos2=arcos
  total2=total
  lista22=lista2
}
tiemporutas=numeric(rut)
for (i in 1:rut){
  tiemporutas[i]=sum(arcos[which(arcos[,4]==i),3])
}
tiemporutas