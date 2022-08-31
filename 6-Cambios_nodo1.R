# Cambios para nodos de salida hacia 1
# Posible su modificación para incluir otras actualizaciones
nchus=length(ult)
for (i in 1:nchus){ # Posible inclusión por cada arco nuevo por separado. Válido para una ruta
  if (ult[i]!=ultp[i]){
    arcos[which(arcos[,2]==ult[i]),2]=0
    arcos[which(arcos[,1]==ultp[i]),3]=C[ult[i],1]
    arcos[which(arcos[,1]==ultp[i]),1]=ult[i]
    arcos[which(arcos[,2]==0),3]=C[ultp[i],arcos[which(arcos[,2]==0),1]]
    arcos[which(arcos[,2]==0),2]=ultp[i]
  }
}
arcos=cambiosnonuevo(arcos2,n) # Cambios de orientación necesario tras los cambios
(total=sum(arcos[,3])) # Nuevo total
for (i in 2:n){ # Posible cambio de inicios de ruta
  if (sum(arcos[,c(1:2)]==i)==1){
    lista2[arcos[arcos[,1]==i,4]]=i
  }
}
links=crearutas(arcos) # Comprobamos que no hay ciclos. Si los hay, habrá ceros en el vector resultante
if (sum(links!=0)!=n+rut-1){
  print('Hay subciclos. Solución no válida')
  arcos=arcos2
  total=total2
  lista2=lista22
}
if (total>=total2){ # Nuevo total no mejora el anterior. No se hacen los cambios
#  print('Nuevos arcos')
  arcos=arcos2
  total=total2
  lista2=lista22
}
if (total<total2){ # Nuevo total mejora el anterior. Se hacen los cambios
  print('Nuevos arcos')
  arcos2=arcos
  total2=total
  lista22=lista2
}
arcos # Resultado final de aplicar los cambios
total

# Si no hemos tenido ningún cambio hasta el momento, podemos permitir un coste igual para ver cambios posteriores. Análogo al caso anterior
nchus=length(ult)
for (i in 1:nchus){
  if (ult[i]!=ultp[i]){
    arcos[which(arcos[,2]==ult[i]),2]=0
    arcos[which(arcos[,1]==ultp[i]),3]=C[ult[i],1]
    arcos[which(arcos[,1]==ultp[i]),1]=ult[i]
    arcos[which(arcos[,2]==0),3]=C[ultp[i],arcos[which(arcos[,2]==0),1]]
    arcos[which(arcos[,2]==0),2]=ultp[i]
  }
}
arcos=cambiosnonuevo(arcos2,n)
(total=sum(arcos[,3]))
for (i in 2:n){
  if (sum(arcos[,c(1:2)]==i)==1){
    lista2[arcos[arcos[,1]==i,4]]=i
  }
}
links=crearutas(arcos)
if (sum(links!=0)!=n+rut-1){
  print('Hay subciclos. Solución no válida')
  arcos=arcos2
  total=total2
  lista2=lista22
}
if (total>total2){ 
  #  print('Nuevos arcos')
  arcos=arcos2
  total=total2
  lista2=lista22
}
if (total<=total2){ 
  print('Nuevos arcos')
  arcos2=arcos
  total2=total
  lista22=lista2
}
total