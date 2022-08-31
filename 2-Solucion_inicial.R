# Los comandos 'print' se pueden omitir

# Selección directorio
#setwd("~/MTE/3er Cuatrimestre/TFM")
C=as.matrix(read.table("nuevosdatos.txt")) # Matriz costes
P=as.matrix(read.table("nuevosdatos2.txt")) # Vector cargas
Loc=as.matrix(read.table("nuevosdatos3.txt")) # Nombre localidades
loc=as.matrix(read.table("nuevosdatos4.txt")) # Nodo real localidades
C=as.matrix(C[-1,-1]) # Nos quedamos con las distancias
rownames(C)=Loc # Renombramos
colnames(C)=Loc
n=nrow(C) # Cantidad nodos
P=P[,-1] # Nos quedamos con los tiempos de carga
names(P)=Loc # Renombramos
for (i in 1:n){
  C[,i]=C[,i]+P[i]
}
C=t(C) # Matriz de costes real carga+transporte

rut=1 # Número de rutas
k=0
r=0
arcospro=matrix(0,nrow=n,ncol=3)
arcos=matrix(0,nrow=n-1,ncol=4)
#temp=matrix(0,nrow=2,ncol=4)
lista1=seq(1:n) # Lista nodos llegada
lista2=seq(1:n) # Lista nodos no conectados completamente
chus=numeric(n-1)
deschus=numeric(n-1)
ci=0
cambio=numeric(n-1)

while (sum(arcos[,c(1:2)]==0)!=0){
  for (i in lista1){
    x=100
    if (i==1){
      for (j in lista2){
        if (sum(arcos[which(arcos[,c(1:2)]==1)])!=0){ # Indica enlace con nodo 1
          for (i2 in 1:(n-1)){
            chus[i2]=sum(arcos[i2,c(1:2)]==1)
          }
          for (l in 1:2){ 
            if (sum(arcos[,l]==1)!=0){ # Nodos enlace nodo 1
              deschus=arcos[which(chus==1),c(1:2)[-l]]
            }
          }
          if (i!=j){
            if (sum(arcos[,1]==j)>0){
              if (arcos[which(arcos[,1]==j),4]!=0){ # Descarta nodos con ruta definida
                next
              }
            }
            if (C[i,j]<x){ # Descarta nodos ya enlazados
              if (sum(deschus==j)!=0){
                next
              }
              else{ # Si coste menor que el actual, es arco provisional desde 1
                x=C[i,j]
                arcospro[i,1]=i
                arcospro[i,2]=j
                arcospro[i,3]=x
              }
            }
          }
        }
        else{
          if (i!=j){
            if (C[i,j]<x){
              x=C[i,j]
              arcospro[i,1]=i
              arcospro[i,2]=j
              arcospro[i,3]=x
            }
          }
        } 
      }
    }
    else{
      for (j in lista2){
        if (i!=j){ # No válido de un nodo en sí mismo
          if (sum(arcos[,1]==j)>0){
            if (sum(arcos[,2]==i)>0){
              if (arcos[which(arcos[,1]==j),2]==i){ # Descarta arcos ya en la solución
                next
              }
            }
          }
          if (sum(arcos[,1]==i)>0){
            if (j==1 & arcos[which(arcos[,1]==i),4]!=0){ # Descarta conexión con 1 si ruta ya definida
              next
            }
          }
          if (C[i,j]<x){
            if (sum(deschus==i)!=0 & j==1){ # Descarta repetir conexión con 1 si ya conectado
              next
            }
            else if (sum(arcos[,c(1:2)]==i)>0 & sum(arcos[,c(1:2)]==i)>0){
              if (sum(arcos[which(arcos[,1]==i),4]!=0)>0 & sum(arcos[which(arcos[,1]==j),4]!=0)>0){
                if (arcos[which(arcos[,1]==i),4]!=arcos[which(arcos[,1]==j),4]){ # Descarta unión de nodos con rutas definidas distintas
                  next
                }
                else {
                  x=C[i,j]
                  arcospro[i,1]=i
                  arcospro[i,2]=j
                  arcospro[i,3]=x
                }
              }
              else {
                x=C[i,j]
                arcospro[i,1]=i
                arcospro[i,2]=j
                arcospro[i,3]=x
              }
            }
            else if (sum(arcos[,1]==i)>0){
              if (arcos[which(arcos[,1]==i),4]!=0){
                next
              }
            }
            else{
              x=C[i,j]
              arcospro[i,1]=i
              arcospro[i,2]=j
              arcospro[i,3]=x
            }
          }
        }
      }
    }
  }
  arcospro # Parar aquí si se quiere comprobar la matriz resultante
  newnodos=0 # Nodos que entran en esta iteración
  for (i in 1:n){
    print(i)
    for (j in 1:n){
      if (arcospro[i,1]==arcospro[j,2] & arcospro[j,1]==arcospro[i,2] & arcospro[i,1]!=0){ # Arco mínimo para nodo i lo une con j y viceversa
        if (sum(arcos!=0)!=0){
          if (arcos[k,1]==arcospro[i,1] & arcos[k,2]==arcospro[i,2]){ # No vuelve a entrar si ya está en la solución
            print ('Nodo ya visto')
            break
          }
        }
        k=k+1
        newnodos=newnodos+1
        print (k)
        if (k>(n-1)){ # Obligamos a tener los arcos deseados
          k=n-1
        }
        if (arcospro[j,2]==1){ # Si llegada a 1, definimos ruta
          r=r+1
          print(arcospro[j,])
          arcos[k,1]=arcospro[j,1]
          arcos[k,2]=arcospro[j,2]
          arcos[k,3]=arcospro[j,3]
          arcos[k,4]=r
        }
        else if (arcospro[j,1]==1){ # Nodo 1 solo de llegada
          print('Nodo 1 solo de llegada')
          k=k-1
          newnodos=newnodos-1
          break
        }
        else { # Para enlaces distintos al nodo 1
          print(arcospro[i,])
          arcos[k,1]=arcospro[i,1]
          arcos[k,2]=arcospro[i,2]
          arcos[k,3]=arcospro[i,3]
        }
        if (sum(which(arcos[k,1]==arcos[,2]))!=0 & sum(which(arcos[k,2]==arcos[,1]))!=0){ # Si arco existente, no se pone
          print(paste('Nodos',i,'y',j,'ya en la lista de arcos'))
          arcos[k,]=0
          k=k-1
          newnodos=newnodos-1
        }
      }
    }
  }
  arcos
  r1=numeric(4)
  r2=numeric(4)
  if (r>0){ # Para poner los enlaces al nodo 1 arriba
    if (arcos[r,2]!=1){
      if (newnodos!=1){
        r1=arcos[r,]
        r2=arcos[k-newnodos+1,]
        arcos[k-newnodos+1,]=arcos[k,]
        arcos[r,]=r2
        arcos[k,]=r1
      }
      if (newnodos==1){
        r1=arcos[r,]
        arcos[r,]=arcos[k,]
        arcos[k,]=r1
      }
    }
  }
  arcos2=arcos
  li=0
  m=numeric(2)
  lista1=numeric(n)
  if (sum(arcos[,c(1:2)]==1)==rut){ # Para quitar nodo 1 para elección
    li=1
    lista1[li]=1
  }
  li2=li
  ci=0
  cambio=numeric(n-1)
  i=1
  while (i<=n){
    i=i+1
    if (ci>3*n){ # Criterio de parada si falla algo
      print ('Revisar cambios')
      break
    }
    if (sum(arcos[,c(1:2)]==i)>1){ # Nodo dos veces en columna de salida
      if (sum(arcos[,1]==i)>1){
        m=which(arcos[,1]==i)
        print(m)
        if (sum(cambio==m[2])>0 & arcos[m[1],2]==1){ # No se cambia el nodo 1 de posición
          arcos=arcos2
          li=li2
          ci=0
          cambio=numeric(n-1)
          i=1
          break
        }
        else if ((sum(cambio==m[2])>0 & arcos[m[1],2]!=1)){ # Si ya se ha cambiado el de abajo
          p=arcos[m[1],2]
          arcos[m[1],2]=arcos[m[1],1]
          arcos[m[1],1]=p
          ci=ci+1
          cambio[ci]=m[1]
          print(arcos)
          i=1
        }
        else{ # Cambio de abajo por defecto
          p=arcos[m[2],2]
          arcos[m[2],2]=arcos[m[2],1]
          arcos[m[2],1]=p
          ci=ci+1
          cambio[ci]=m[2]
          print(arcos)
          i=1
        }
      }
      if (sum(lista1==i)==0){ # Nodo no en lista de elementos en posición 1
        li=li+1
        lista1[li]=i
      }
    }
  }
  
  if (length(which(arcos==arcos2))==4*(n-1)){ # Si antes no se han hecho cambios, es que resulta más efectivo el cambio por defecto de arriba
    while (i<=n){
      i=i+1
      if (ci>3*n){
        print ('Revisar cambios')
        break
      }
      if (sum(arcos[,c(1:2)]==i)>1){
        if (sum(arcos[,1]==i)>1){
          m=which(arcos[,1]==i)
          print(m)
          if ((sum(cambio==m[1])>0 | arcos[m[1],2]==1)){
            p=arcos[m[2],2]
            arcos[m[2],2]=arcos[m[2],1]
            arcos[m[2],1]=p
            ci=ci+1
            cambio[ci]=m[2]
            print(arcos)
            i=1
          }
          else{
            p=arcos[m[1],2]
            arcos[m[1],2]=arcos[m[1],1]
            arcos[m[1],1]=p
            ci=ci+1
            cambio[ci]=m[1]
            print(arcos)
            i=1
          }
        }
        if (sum(lista1==i)==0){
          li=li+1
          lista1[li]=i
        }
      }
    }
  }
  
  lista1
  lista1=seq(1:n)[-lista1] # Lista de nodos no de salida y nodo 1 en caso necesario
  if (li==0){
    lista1=seq(1:n)
  }
  if ((sum(arcos[,2]==1))<rut){
    lista1=c(1,lista1)
  }
  for (l in 1:2){
    #if (sum(arcos[,l]==1)>0){
    lista2=sort(arcos[which(arcos[,l]!=0),l])
    #}
  }
  lista2=seq(1:n)[-lista2] # Nodos que no tienen todos los enlaces requeridos
  if (sum(arcos[,c(1:2)]==1)<rut){
    lista2=c(1,lista2)
  }
  arcospro=matrix(0,nrow=n,ncol=3)
  nodact=numeric(sum(arcos[,1]!=0))
  col2=1
  nuevo=col2
  i=1
  while (i<=length(nodact)){ # Algoritmo de actualización de rutas
    if (nuevo==1){
      nodact[i]=arcos[col2,1]
      nuevo=arcos[col2,1]
      i=i+1
    }
    else if (sum(arcos[,2]==nuevo)==0){
      if (col2<sum(arcos[,2]==1)){
        col2=col2+1
        nuevo=1
      }
      else {
        i=length(nodact)+1
      }
    }
    else{
      nodact[i]=arcos[which(arcos[,2]==nuevo),1]
      nuevo=arcos[which(arcos[,2]==nuevo),1]
      i=i+1
    }
  }
  if (sum(nodact==0)!=0){
    nodact=nodact[-which(nodact==0)]
  }
  for (i in 1:length(nodact)){
    if (arcos[which(arcos[,1]==nodact[i]),4]!=0){
      arcos[which(arcos[,2]==nodact[i]),4]=arcos[which(arcos[,1]==nodact[i]),4]
    }
  } 
  arcos # Parar aquí en caso de ver la solución en la iteración actual
}
(total=sum(arcos[,3])) # Coste total en este punto

for (l in 1:2){ # Conjunto de nodos en cada columna
  if (sum(arcos[,l]==1)>0){
    nodos1=arcos[,l]
    nodos2=arcos[,c(1:2)[-l]]
  }
}

lista2=numeric(rut)
for (i in 2:n){ # Pasa a ser el conjunto de nodos que inician las rutas
  if (sum(arcos[,c(1:2)]==i)==1){
    lista2[arcos[arcos[,1]==i,4]]=i
  }
}
r=max(arcos[,4]) # Rutas que tenemos
while (r<rut){ # Entre los inicios de rutas, nos quedamos con los enlaces con el nodo 1 para tener las rutas deseadas
  if (sum(lista2==0)>0){
    lista2=lista2[-which(lista2==0)]
  }
  r=r+1
  x=100
  for (i in 1:length(lista2)){
    if (arcos[which(arcos[,1]==lista2[i]),2]!=1){
      if (x>C[lista2[i],1]){
        x=C[lista2[i],1]
        nl2=lista2[i]
      }
    }
  }
  arcos[which(arcos[,1]==nl2),1]=nl2
  arcos[which(arcos[,1]==nl2),2]=1
  arcos[which(arcos[,1]==nl2),3]=x
  arcos[which(arcos[,1]==nl2),4]=r
  lista2=numeric(rut)
  for (i in 2:n){
    if (sum(arcos[,c(1:2)]==i)==1){
      lista2[arcos[arcos[,1]==i,4]]=i
    }
  }
}
(arcos=arcos[order(arcos[,2],arcos[,4]),]) # Ordenados los arcos por orden ascendente del número del nodo y de la ruta
tiemporutas=numeric(rut)
for (i in 1:rut){ # Coste actual de cada ruta
  tiemporutas[i]=sum(arcos[which(arcos[,4]==i),3])
}
tiemporutas