# Cambio de orientación del arco de inicio de ruta
# Cambio para ir de ruta en ruta. Válido con una ruta
nlis=arcos[which(arcos[,1]==lista2),2]
arcos[which(arcos==nlis[1])]=lista2[1]
arcos[which(arcos[,1]==arcos[,2]),1]=nlis[1]
for (i in 1:(n-1)){
  arcos[i,3]=C[arcos[i,1],arcos[i,2]]
}
for (i in 2:n){
  if (sum(arcos[,c(1:2)]==i)==1){
    lista2[arcos[arcos[,1]==i,4]]=i
  }
}
(total=sum(arcos[,3]))