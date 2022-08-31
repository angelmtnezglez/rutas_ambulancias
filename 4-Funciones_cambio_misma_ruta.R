# Comandos 'print' son omitibles
# Funciones para cambios en nodos de la misma ruta
cambioscol2<-function(arcos2,nuevoarco,n){ # Utilizable siempre que se quite nuevoarco[2] en la segunda columna.
  i=1 # Nodo a comprobar. Se pone el 1 porque esto se hace para el resto de nodos.
  cambio=numeric(n-1) # Arcos ya cambiados.
  k=arcos[which(arcos[,2]==nuevoarco[1]),1] # Reservamos el nodo a sustituir
  if (k==arcos[which(arcos[,2]==nuevoarco[2]),2]){
    print ('Imposible ir de un nodo en si mismo')
    
  }
  else{
    if (arcos[which(arcos[,1]==nuevoarco[2]),2]!=1){
      arcos[which(arcos[,2]==nuevoarco[2]),2]=0 # Quitamos nodo para evitar más de dos repeticiones del mismo nodo
      arcos[which(arcos[,2]==nuevoarco[1]),1]=nuevoarco[2] # Emparejamos el nuevo arco  
      arcos[which(arcos[,2]==nuevoarco[1]),3]=C[nuevoarco[1],nuevoarco[2]] # Coste del nuevo arco
    }
    ci=1
    cambio[1]=which(arcos[,2]==nuevoarco[1]) # Evitamos que ese arco entre en los intercambios de nuevo
  }
  while (i<=n){ # Vemos que en ningún arco haga falta cambio
    i=i+1
    if (ci>3*n){ # Criterio prueba cambios infinitos
      print ('Revisar cambios')
      
    }
    if (sum(arcos[,2]==i)>1){ # Comprobamos i hay nodos distintos a 1 repetidos en la columna 2.
      m=which(arcos[,2]==i) # Vemos las filas en donde se repite.
      print(m)
      if ((sum(cambio==m[2])>0)){ # Si la segunda fila ya se ha cambiado:
        if (sum(cambio==m[1])>0){ # Si ambas filas ya se ha cambiado, no hacemos más cambios.
          next
        }
        else { # Hacemos el cambio en el arco de la fila superior.
          p=arcos[m[1],1]
          arcos[m[1],1]=arcos[m[1],2]
          arcos[m[1],2]=p
          ci=ci+1
          cambio[ci]=m[1]
          print(arcos)
          i=1
        }
      }
      else{ # De forma predeterminada, el cambio se realiza en el arco de la fila inferior.
        p=arcos[m[2],1]
        arcos[m[2],1]=arcos[m[2],2]
        arcos[m[2],2]=p
        ci=ci+1
        cambio[ci]=m[2]
        print(arcos)
        i=1
      }
    }
    if (sum(arcos[,1]==i)>1){ # Mismo elemento dos veces en la columna de salidas
      m=which(arcos[,1]==i) # Filas en la que se encuentran
      print(m)
      if ((sum(cambio==m[2])>0)){
        if (arcos[m[1],2]==1){ # Si el arco a cambiar tiene llegada en 1, el cambio no es válido.
          arcos=arcos2
          ci=0
          cambio=numeric(n-1)
          i=1
          break
        }
        else if (sum(cambio==m[1])>0){
          break
        }
        else {
          p=arcos[m[1],2]             #
          arcos[m[1],2]=arcos[m[1],1] # Intercambio de los nodos
          arcos[m[1],1]=p             #
          ci=ci+1
          cambio[ci]=m[1] # Incluimos esta fila para que no se vuelva a cambiar.
          print(arcos)
          i=1 # Reiniciamos la búsqueda
        }
      }
      else{ # Por defecto, el cambio se realiza en el nodo de más abajo. Esta elección
        # se debe a que en el nodo de más abajo no  deberíamos tener el nodo 1
        # como posible cambio. El proceso es el mismo que el explicado arriba,
        # pero con el arco de más abajo.
        p=arcos[m[2],2]
        arcos[m[2],2]=arcos[m[2],1]
        arcos[m[2],1]=p
        ci=ci+1
        cambio[ci]=m[2]
        print(arcos)
        i=1
      }
    }
  }
  if (length(which(arcos==arcos2))==4*(n-1)){ # En caso de llegar a un intercambio de nodo 1, repetimos emparejando el arco al revés.
    i=1
    cambio=numeric(n-1)
    k=arcos[which(arcos[,2]==nuevoarco[1]),1] # Reservamos el nodo a sustituir
    if (k==arcos[which(arcos[,2]==nuevoarco[2]),2]){
      print ('Imposible ir de un nodo en si mismo')
      
    }
    else{
      if (arcos[which(arcos[,1]==nuevoarco[2]),2]!=1){
        arcos[which(arcos[,2]==nuevoarco[2]),2]=0 # Quitamos nodo para evitar más de dos repeticiones del mismo nodo
        arcos[which(arcos[,2]==nuevoarco[1]),1]=nuevoarco[2] # Emparejamos el nuevo arco 
        m=which(arcos[,2]==nuevoarco[1]) # Para cambiar el arco de interés.
        p=arcos[m[1],2]
        arcos[m[1],2]=arcos[m[1],1]
        arcos[m[1],1]=p
        arcos[which(arcos[,2]==nuevoarco[2]),3]=C[nuevoarco[1],nuevoarco[2]] # Coste del nuevo arco
        ci=1
        cambio[1]=which(arcos[,2]==nuevoarco[2]) # Evitamos que ese arco entre en los intercambios
      }
    }
    while (i<=n){
      i=i+1
      if (ci>3*n){
        print ('Revisar cambios')
        
      }
      if (sum(arcos[,2]==i)>1){ # Repeticiones en la columna de llegadas.
        m=which(arcos[,2]==i)
        print(m)
        if ((sum(cambio==m[2])>0)){ # Si el arco de abajo ya se ha cambiado:
          if (sum(cambio==m[1])>0){ # Si ambos ya se han cambiado, pasamos al siguiente caso.
            break
          }
          else { # Cambios en el arco superior.
            p=arcos[m[1],1]
            arcos[m[1],1]=arcos[m[1],2]
            arcos[m[1],2]=p
            ci=ci+1
            cambio[ci]=m[1]
            print(arcos)
            i=1
          }
        }
        else{ # Por defecto, cambios en el arco inferior.
          p=arcos[m[2],1]
          arcos[m[2],1]=arcos[m[2],2]
          arcos[m[2],2]=p
          ci=ci+1
          cambio[ci]=m[2]
          print(arcos)
          i=1
        }
      }
      if (sum(arcos[,1]==i)>1){ # Repetición en la columna de salidas.
        m=which(arcos[,1]==i)
        print(m)
        if (arcos[m[1],2]==1){ # Si algún arco tiene de llegada el nodo 1, el cambio 
          # No es válido
          arcos=arcos2
          ci=0
          cambio=numeric(n-1)
          i=1
          break
        }
        else if (sum(cambio==m[1])>0){ # Cambio en el arco inferior. 
          p=arcos[m[2],2]
          arcos[m[2],2]=arcos[m[2],1]
          arcos[m[2],1]=p
          ci=ci+1
          cambio[ci]=m[2]
          print(arcos)
          i=1
        }
        else{ # Por defecto, cambios en el arco superior.
          p=arcos[m[1],2]
          arcos[m[1],2]=arcos[m[1],1]
          arcos[m[1],1]=p
          ci=ci+1
          cambio[ci]=m[1]
          print(arcos)
          i=1
        }
      }
    }
  }
  return(arcos)
}


cambioscol1<-function(arcos2,nuevoarco,n){ # Utilizable siempre que se quite nuevoarco[2] en la primera columna. El proceso es análogo a la función anterior.
  i=1
  cambio=numeric(n-1)
  k=arcos[which(arcos[,1]==nuevoarco[1]),2] # Reservamos el nodo a sustituir
  if (k==arcos[which(arcos[,1]==nuevoarco[2]),2]){
    print ('Imposible ir de un nodo en si mismo')
  }
  else if (k==1){
    print ('No se cambia el nodo 1')
    break
  }
  else{
    if (arcos[which(arcos[,1]==nuevoarco[2]),2]!=1){
      arcos[which(arcos[,1]==nuevoarco[2]),1]=0 # Quitamos nodo para evitar más de dos repeticiones del mismo nodo
      arcos[which(arcos[,1]==nuevoarco[1]),2]=nuevoarco[2] # Emparejamos el nuevo arco  
      arcos[which(arcos[,1]==nuevoarco[1]),3]=C[nuevoarco[1],nuevoarco[2]] # Coste del nuevo arco
    }
    ci=1
    cambio[1]=which(arcos[,1]==nuevoarco[1]) # Evitamos que ese arco entre en los intercambios
  }
  while (i<=n){ # Vemos que en ningún arco haga falta cambio
    i=i+1
    if (ci>3*n){ # Criterio prueba cambios infinitos
      print ('Revisar cambios')
      
    }
    if (sum(arcos[,2]==i)>1){
      m=which(arcos[,2]==i)
      print(m)
      if ((sum(cambio==m[2])>0)){
        if (sum(cambio==m[1])>0){
          next
        }
        else {
          p=arcos[m[1],1]
          arcos[m[1],1]=arcos[m[1],2]
          arcos[m[1],2]=p
          ci=ci+1
          cambio[ci]=m[1]
          print(arcos)
          i=1
        }
      }
      else{
        p=arcos[m[2],1]
        arcos[m[2],1]=arcos[m[2],2]
        arcos[m[2],2]=p
        ci=ci+1
        cambio[ci]=m[2]
        print(arcos)
        i=1
      }
    }
    if (sum(arcos[,1]==i)>1){ # Mismo elemento dos veces en la columna de salidas
      m=which(arcos[,1]==i) # Filas en la que se encuentran
      print(m)
      if ((sum(cambio==m[2])>0)){
        if (arcos[m[1],2]==1){ # Si el arco a cambiar tiene llegada en 1, el cambio no es válido.
          arcos=arcos2
          ci=0
          cambio=numeric(n-1)
          i=1
          next
        }
        else if (sum(cambio==m[1])>0){
          next
        } # Si el arco a cambiar ya se ha cambiado
        else{ # previamente, cambiamos el de arriba.
          p=arcos[m[1],2]             #
          arcos[m[1],2]=arcos[m[1],1] # Intercambio de los nodos
          arcos[m[1],1]=p             #
          ci=ci+1
          cambio[ci]=m[1] # Incluimos esta fila para que no se vuelva a cambiar.
          print(arcos)
          i=1 # Reiniciamos la búsqueda
        }
      }
      else{ # Por defecto, el cambio se realiza en el nodo de más abajo. Esta elección
        # se debe a que en el nodo de más abajo no  deberíamos tener el nodo 1
        # como posible cambio. El proceso es el mismo que el explicado arriba,
        # pero con el arco de más abajo.
        p=arcos[m[2],2]
        arcos[m[2],2]=arcos[m[2],1]
        arcos[m[2],1]=p
        ci=ci+1
        cambio[ci]=m[2]
        print(arcos)
        i=1
      }
    }
  }
  if (length(which(arcos==arcos2))==4*(n-1)){ # En caso de llegar a un intercambio de nodo 1, repetimos emparejando el arco al revés.
    i=1
    cambio=numeric(n-1)
    k=arcos[which(arcos[,1]==nuevoarco[1]),2] # Reservamos el nodo a sustituir
    if (k==arcos[which(arcos[,1]==nuevoarco[2]),2]){
      print ('Imposible ir de un nodo en si mismo')
      
    }
    else{
      if (arcos[which(arcos[,1]==nuevoarco[2]),2]!=1){
        arcos[which(arcos[,1]==nuevoarco[2]),1]=0 # Quitamos nodo para evitar más de dos repeticiones del mismo nodo
        m=which(arcos[,1]==0)
        p=arcos[m[1],2]
        arcos[m[1],2]=arcos[m[1],1]
        arcos[m[1],1]=p
        arcos[which(arcos[,1]==nuevoarco[1]),2]=nuevoarco[2] # Emparejamos el nuevo arco  
        arcos[which(arcos[,1]==nuevoarco[1]),3]=C[nuevoarco[1],nuevoarco[2]] # Coste del nuevo arco
        ci=1
        cambio[1]=which(arcos[,1]==nuevoarco[1]) # Evitamos que ese arco entre en los intercambios
      }
    }
    print(arcos)
    while (i<=n){
      i=i+1
      if (ci>3*n){
        print ('Revisar cambios')
        
      }
      if (sum(arcos[,2]==i)>1){
        m=which(arcos[,2]==i)
        print(m)
        if ((sum(cambio==m[2])>0)){
          if (sum(cambio==m[1])>0){
            next
          }
          else {
            p=arcos[m[1],1]
            arcos[m[1],1]=arcos[m[1],2]
            arcos[m[1],2]=p
            ci=ci+1
            cambio[ci]=m[1]
            print(arcos)
            i=1
          }
        }
        else{
          p=arcos[m[2],1]
          arcos[m[2],1]=arcos[m[2],2]
          arcos[m[2],2]=p
          ci=ci+1
          cambio[ci]=m[2]
          print(arcos)
          i=1
        }
      }
      if (sum(arcos[,1]==i)>1){
        m=which(arcos[,1]==i)
        print(m)
        if (sum(cambio==m[1])>0){ 
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
    }
  }
  return(arcos)
}

cambiosnonuevo<-function(arcos2,n){ # Utilizable siempre. Funcionamiento análogo a las dos funciones anteriores.
  i=1
  cambio=numeric(n-1)
  while (i<=n){ # Vemos que en ningún arco haga falta cambio
    i=i+1
    if (ci>3*n){ # Criterio prueba cambios infinitos
      print ('Revisar cambios')
      
    }
    if (sum(arcos[,2]==i)>1){
      m=which(arcos[,2]==i)
      print(m)
      if ((sum(cambio==m[2])>0)){
        p=arcos[m[1],1]
        arcos[m[1],1]=arcos[m[1],2]
        arcos[m[1],2]=p
        ci=ci+1
        cambio[ci]=m[1]
        print(arcos)
        i=1 
      }
      else{
        p=arcos[m[2],1]
        arcos[m[2],1]=arcos[m[2],2]
        arcos[m[2],2]=p
        ci=ci+1
        cambio[ci]=m[2]
        print(arcos)
        i=1
      }
    }
    cambio=numeric(n-1)
    i=1
    while (i<=n){
      i=i+1
      if (ci>3*n){ # Criterio prueba cambios infinitos
        print ('Revisar cambios')
        
      }
      if (sum(arcos[,1]==i)>1){ # Mismo elemento dos veces en la columna de salidas
        m=which(arcos[,1]==i) # Filas en la que se encuentran
        print(m)
        if ((sum(cambio==m[2])>0)){
          if (arcos[m[1],2]==1){ # Si el arco a cambiar tiene llegada en 1, el cambio no es válido.
            arcos=arcos2
            #          lista2=listaprov
            ci=0
            cambio=numeric(n-1)
            i=1
            next
          }
          else{
            p=arcos[m[1],2]             #
            arcos[m[1],2]=arcos[m[1],1] # Intercambio de los nodos
            arcos[m[1],1]=p             #
            ci=ci+1
            cambio[ci]=m[1] # Incluimos esta fila para que no se vuelva a cambiar.
            print(arcos)
            i=1 # Reiniciamos la búsqueda
          }
        }
        else{ # Por defecto, el cambio se realiza en el nodo de más abajo. Esta elección
          # se debe a que en el nodo de más abajo no  deberíamos tener el nodo 1
          # como posible cambio. El proceso es el mismo que el explicado arriba,
          # pero con el arco de más abajo.
          p=arcos[m[2],2]
          arcos[m[2],2]=arcos[m[2],1]
          arcos[m[2],1]=p
          ci=ci+1
          cambio[ci]=m[2]
          print(arcos)
          i=1
        }
      }
    }
  }
  return(arcos)
}