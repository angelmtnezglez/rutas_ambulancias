# Devuelve un vector con las distintas rutas. Aparición de 1 indica final de la ruta. 
# Aparición de 0 implica existencia de nodos no conectados con 1.
crearutas<-function(arcos){
  link=numeric(n+rut-1) # Se establece este número de nodos pues se espera que, 
  # como mucho, cada nodo aparezca una vez salvo el nodo 1, que aparecerá tantas 
  # veces como rutas hayamos establecido.
  nodofin=1
  for (ruta in 1:rut){
    link[nodofin]=lista2[ruta] # lista2 contiene los nodos distintos a 1 que aparecen 
    # solo una vez en el conjunto de arcos, es decir, son
    # los nodos de salida de cada ruta.
    k=nodofin # k es la posición en la que colocamos el nodo.
    while (k<=n+rut-1){
      #print(k)
      k=k+1 # Buscamos el siguiente elemento.
      link[k]=arcos[which(arcos[,1]==link[k-1]),2] # Buscamos la unión entre el nodo
      # de posición anterior y el nodo
      # de llegada
      if (link[k]==1){ # Si el nodo de llegada es el 1, la ruta se acaba.
        nodofin=k+1
        break
      }
    }
  }
  return(link)
}