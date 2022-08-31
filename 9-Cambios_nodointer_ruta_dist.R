# Para incluir un nodo de otra ruta en medio de la que queremos actualizar.

x=0
for (i in 1:2){ # Entre las rutas de los nodos que tenemos, elegimos la de menor coste. En caso de empate, elegimos la primera.
  if(tiemporutas[arcos[which(arcos[,1]==nuevoarco[i]),4]]>x){
    x=tiemporutas[arcos[which(arcos[,1]==nuevoarco[i]),4]]
    vruta=arcos[which(arcos[,1]==nuevoarco[i]),4]
    nodcambio=arcos[which(arcos[,1]==nuevoarco[i]),1]
  }
}
cambruta=-1
if (sum(arcos[,2]==cambruta)==0){
  cambruta=arcos[which(arcos[,2]==nodcambio),1] # Nodo del que se llegaba al nodo de nuevoarco
  arcos[which(arcos[,1]==nodcambio),1]=cambruta # Ocupa su lugar
}
arcos[which(arcos[,2]==nodcambio),]=0 # Eliminamos la fila de llegada al nodo de nuevo arco seleccionado.
if (cambruta!=-1){
  arcos[which(arcos[,1]==cambruta),3]=C[arcos[which(arcos[,1]==cambruta),1],arcos[which(arcos[,1]==cambruta),2]] # Actualizamos el coste de la nueva conexión.
}
  
nodnocambio=nuevoarco[which(nuevoarco!=nodcambio)] # El otro nodo de nuevoarco.
k1=0
if (sum(arcos[,2]==nodnocambio)>0){ # En caso de que nodnocambio no inicie ruta, hay que elegir la mejor opción de enlace.
  k1=arcos[which(arcos[,2]==nodnocambio),1] # Nodo de proccedencia a nodnocambio.
}
k2=arcos[which(arcos[,1]==nodnocambio),2] # Nodo de salida a nodnocambio.
k=k2 # Establecemos k1 como el nodo de enlace con nodcambio.
fcam=which(arcos[,2]==k) # Reservamos la fila en la que tenemos este arco.
if (k1!=0){ # Si tenemos dos opciones, nos toca elegir el nodo con el que unir la nueva adición.
  if (C[k2,nodcambio]>C[k1,nodcambio]){ # Si el coste de unir k2 es menor, es nuestro nuevo arco.
    k=k1
    fcam=which(arcos[,1]==k)
  }
  if (C[k2,nodcambio]==C[k1,nodcambio]){ # En caso de empate, la selección se hace con el arco que une el otro nodo de nuevoarco.
    if (C[k2,nodnocambio]>C[k1,nodnocambio]){
      k=k1
      fcam=which(arcos[,1]==k)
    }
  }
}

arcos[fcam,which(arcos[fcam,c(1:2)]==nodnocambio)]=nodcambio # Ponemos el nuevo nodo para meter el arco que queremos actualizar.
arcos[fcam,3]=C[arcos[fcam,1],arcos[fcam,2]] # Actualizamos el coste
fcam=which(arcos[,1]==0) # Selñeccionamos la fila vacía en la que metemos nuevoarco.
arcos[fcam,3]=C[nuevoarco[1],nuevoarco[2]] # Actualizamos coste.
for (i in 1:2){ # Seleccionamos la ruta y también establecemos el orden en el que orientamos el arco.
  if (sum(nuevoarco[i]==arcos[,1])>0){ # Nos guiamos por el nodo que ya tenga un nodo de salida.
    arcos[fcam,4]=arcos[which(arcos[,1]==nuevoarco[i]),4]
    orden=seq(1:2)[-i]
    orden2=i
  }
  else {
    next
  }
}
arcos[fcam,1]=nuevoarco[orden] # Colocamos el nuevoarco de salida.
arcos[fcam,2]=nuevoarco[orden2] # Colocamos el nuevoarco de llegada.

total=sum(arcos[,3]) # Calculamos el nuevo coste total
if (total<=total2){
  arcos2=arcos
  total2=total
  lista22=lista2
  print('Nuevos arcos')
}
if (total>total2){ # Nuevo total no mejora el anterior. No se hacen los cambios
  arcos=arcos2
  total=total2
  lista2=lista22
}
tiemporutas=numeric(rut)
for (i in 1:rut){
  tiemporutas[i]=sum(arcos[which(arcos[,4]==i),3])
}
tiemporutas