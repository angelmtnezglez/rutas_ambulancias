# Suma hasta el límite.
lim=12
for (i in 1:rut){ #Sumamos desde el nodo 1 hasta delante y quitamos ruta de aquellos arcos que exceden nuestro límite
  coste=arcos[i,3]
  newnodo=arcos[i,1]
  j=2
  while (j<(n+1)){
    if (sum(newnodo==arcos[,2])==1){
      if (coste+arcos[which(arcos[,2]==newnodo),3]<=lim){
        coste=coste+arcos[which(arcos[,2]==newnodo),3]
        newnodo=arcos[which(arcos[,2]==newnodo),1]
        j=j+1
      }
      else{
        coste=coste+arcos[which(arcos[,2]==newnodo),3]
        arcos[which(arcos[,2]==newnodo),4]=0
        newnodo=arcos[which(arcos[,2]==newnodo),1]
        j=j+1
      }
    }
    else{
      j=n+1
    }
  }
}
libres=sort(arcos[which(arcos[,4]==0),1]) # Conjunto de nodos que quedan sin ruta 

# Añadir periodos de tiempo para solución heurística.
arcos=cbind(arcos,arcos[,3])
arcos[,3]=0
colnames(arcos)=c('NodCar','NodDes','T','Ruta','Coste')
links=crearutas(arcos)
xt=0
for (i in links){
  if (i==1){
    xt=0
    next
  }
  else{
    arcos[which(arcos[,1]==i),3]=xt
    xt=xt+arcos[which(arcos[,1]==i),5]
  }
}
arcos[,3]=floor(arcos[,3])
arcos=arcos[order(arcos[,4],arcos[,3]),]
inirut=rep(0,rut)

# Poner los nodos reales de cada localidad.
for (i in 1:n){
  arcos[which(arcos[,1]==i),1]=loc[i]
  arcos[which(arcos[,2]==i),2]=loc[i]
}
arcos
for (i in loc){
  if (sum(arcos[,c(1:2)]==i)==1){
    lista2[arcos[arcos[,1]==i,4]]=i
  }
}
crearutas(arcos)

# Poner los nombres de las localidades.
for (i in 1:n){
  arcos[which(arcos[,1]==i),1]=Loc[i]
  arcos[which(arcos[,2]==i),2]=Loc[i]
}
arcos

# Cambios de orientación de arcos intermedios (misma ruta)
cambiosimple=arcos[,1] # Conjunto de nodos de salida
cambiosimple=cambiosimple[-seq(1:rut)] # Eliminamos los nodos unidos al nodo 1
cambiosimple=cambiosimple[-which(cambiosimple==lista2)] # Eliminamos los nodos de salida de ruta
ncam=length(cambiosimple)
i=1
while(i<=ncam){
  nodo1=cambiosimple[i] # Nodo i en el listado de nodos de salida
  nodo2=arcos[which(arcos[,1]==nodo1),2] # Nodo de llegada
  antes=arcos[which(arcos[,1]==nodo1),3]+arcos[which(arcos[,2]==nodo1),3]+arcos[which(arcos[,1]==nodo2),3] # Suma considerando el arco actual
  despues=C[nodo2,nodo1]+C[nodo1,arcos[which(arcos[,1]==nodo2),2]]+C[arcos[which(arcos[,2]==nodo1),1],nodo2] # Suma considerando un cambio de orientación
  i=i+1
  if (despues<antes){ # Se hace el cambio si el cambio de orientación nos da mejor coste
    arcos[which(arcos[,2]==nodo1),2]=nodo2
    arcos[which(arcos[,1]==nodo2),1]=nodo1
    arcos[which(arcos[,1]==nodo1 & arcos[,2]==nodo2),c(1:2)]=c(nodo2,nodo1)
    for (j in 1:(n-1)){
      arcos[j,3]=C[arcos[j,1],arcos[j,2]]
    }
    cambiosimple=arcos[,1]
    cambiosimple=cambiosimple[-seq(1:rut)]
    cambiosimple=cambiosimple[-which(cambiosimple==lista2)]
    i=1
  }
}
arcos # Después de los cambios
(total=sum(arcos[,3]))