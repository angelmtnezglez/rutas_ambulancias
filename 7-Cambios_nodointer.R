# Cambio de arcos en la misma ruta. Posible su modificación para incluir otras actualizaciones
#nuevoarco=c() # Arco nuevo dentro de la misma ruta. Posible automatización de este proceso
arcos=cambioscol2(arcos2,nuevoarco,n)
l=1
k=arcos2[which(arcos2[,2]==nuevoarco[1]),1]
if (sum(arcos[,2]==0)==1){
  l=2
}
arcos[which(arcos[,l]==0),3]=C[k,arcos[which(arcos[,l]==0),c(1:2)[-l]]] # Ponemos el coste del nuevo arco
arcos[which(arcos[,l]==0),l]=k # Ponemos el nuevo nodo
arcos=cambiosnonuevo(arcos2,n)
for (i in lista2){
  if (sum(arcos[,2]==i)>0){
    p=arcos[which(arcos[,2]==i),1]
    arcos[which(arcos[,2]==p),3]=C[arcos[which(arcos[,2]==p),1],i]
    arcos[which(arcos[,2]==p),2]=i
    print ('Posible nuevo inicio de ruta')
  }
}
total=sum(arcos[,3]) # Calculamos el nuevo coste total
if (total>=total2){ # Nuevo total no mejora el anterior. No se hacen los cambios
  arcos=arcos2
  total=total2
  lista2=lista22
}
links=crearutas(arcos)
if (sum(links!=0)!=n+rut-1){
  print('Hay subciclos. Solución no válida')
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
arcos=cambioscol1(arcos2,nuevoarco,n)
l=1
k=arcos2[which(arcos2[,1]==nuevoarco[1]),2]
if (sum(arcos[,2]==0)==1){
  l=2
}
arcos[which(arcos[,l]==0),3]=C[k,arcos[which(arcos[,l]==0),c(1:2)[-l]]] # Ponemos el coste del nuevo arco
arcos[which(arcos[,l]==0),l]=k # Ponemos el nuevo nodo
arcos=cambiosnonuevo(arcos2,n)
for (i in lista2){
  if (sum(arcos[,2]==i)>0){
    p=arcos[which(arcos[,2]==i),1]
    arcos[which(arcos[,2]==p),3]=C[arcos[which(arcos[,2]==p),1],i]
    arcos[which(arcos[,2]==p),2]=i
    print ('Posible nuevo inicio de ruta')
  }
}
total=sum(arcos[,3]) # Calculamos el nuevo coste total
if (total>=total2){ # Nuevo total no mejora el anterior. No se hacen los cambios
  arcos=arcos2
  total=total2
  lista2=lista22
}
links=crearutas(arcos)
if (sum(links!=0)!=n+rut-1){
  print('Hay subciclos. Solución no válida')
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
if (total==total3){
  na=nuevoarco[2]
  nuevoarco[2]=nuevoarco[1]
  nuevoarco[1]=na
  arcos=cambioscol2(arcos2,nuevoarco,n)
  l=1
  k=arcos2[which(arcos2[,2]==nuevoarco[1]),1]
  if (sum(arcos[,2]==0)==1){
    l=2
  }
  arcos[which(arcos[,l]==0),3]=C[k,arcos[which(arcos[,l]==0),c(1:2)[-l]]] # Ponemos el coste del nuevo arco
  arcos[which(arcos[,l]==0),l]=k # Ponemos el nuevo nodo
  arcos=cambiosnonuevo(arcos2,n)
  for (i in lista2){
    if (sum(arcos[,2]==i)>0){
      p=arcos[which(arcos[,2]==i),1]
      arcos[which(arcos[,2]==p),3]=C[arcos[which(arcos[,2]==p),1],i]
      arcos[which(arcos[,2]==p),2]=i
      print ('Posible nuevo inicio de ruta')
    }
  }
  total=sum(arcos[,3]) # Calculamos el nuevo coste total
  if (total>=total2){ # Nuevo total no mejora el anterior. No se hacen los cambios
    arcos=arcos2
    total=total2
    lista2=lista22
  }
  links=crearutas(arcos)
  if (sum(links!=0)!=n+rut-1){
    print('Hay subciclos. Solución no válida')
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
  arcos=cambioscol1(arcos2,nuevoarco,n)
  l=1
  k=arcos2[which(arcos2[,1]==nuevoarco[1]),2]
  if (sum(arcos[,2]==0)==1){
    l=2
  }
  arcos[which(arcos[,l]==0),3]=C[k,arcos[which(arcos[,l]==0),c(1:2)[-l]]] # Ponemos el coste del nuevo arco
  arcos[which(arcos[,l]==0),l]=k # Ponemos el nuevo nodo
  arcos=cambiosnonuevo(arcos2,n)
  for (i in lista2){
    if (sum(arcos[,2]==i)>0){
      p=arcos[which(arcos[,2]==i),1]
      arcos[which(arcos[,2]==p),3]=C[arcos[which(arcos[,2]==p),1],i]
      arcos[which(arcos[,2]==p),2]=i
      print ('Posible nuevo inicio de ruta')
    }
  }
  total=sum(arcos[,3]) # Calculamos el nuevo coste total
  if (total>=total2){ # Nuevo total no mejora el anterior. No se hacen los cambios
    arcos=arcos2
    total=total2
    lista2=lista22
  }
  links=crearutas(arcos)
  if (sum(links!=0)!=n+rut-1){
    print('Hay subciclos. Solución no válida')
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
}
arcos
total