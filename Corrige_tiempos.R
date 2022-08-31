# Programa para corregir los periodos de tiempo de llegada

# Distancias
setwd("~/MTE/3er Cuatrimestre/TFM")
load("~/MTE/3er Cuatrimestre/TFM/datos_matrices.RData")
C=M
n=nrow(M)
carga=c(0,rep(0.5,n-1))
for (i in 1:n){
  C[,i]=C[,i]+carga[i]
}
C=t(C)

# Arcos solución. Primera versión.
(arcos=as.matrix(read.table('solcambio.txt'))) # Nombre del archivo donde guardamos la solución
colnames(arcos)=c('NodCar','NodDes','T','Ruta','Coste')
rut=max(arcos[,4])
nsol=nrow(arcos)
arcos[,3]=arcos[,3]-min(arcos[,3])
for (i in 1:nsol){
  arcos[i,5]=C[arcos[i,1],arcos[i,2]]
}
(arcos=arcos[order(arcos[,4],arcos[,3]),])
tiemporutas=numeric(rut)
for (i in 1:rut){
  tiemporutas[i]=sum(arcos[which(arcos[,4]==i),5])
}
tiemporutas
#arcos2=arcos
nodrutfin=0
inirut=numeric(rut)
for (j in 1:rut){
  nodrut=nodrutfin
  nodrutfin=nodrut+sum(arcos[,4]==j)
  inirut[j]=arcos[nodrut+1,3]
  for (i in (nodrut+2):nodrutfin){
    arcos[i,3]=arcos[i-1,3]+arcos[i-1,5]
  }
}
arcos[,3]=floor(arcos[,3])


# Solución con llegadas. También válido con el heurístico a partir de aquí.
tfin=inirut+tiemporutas
#tfin=c(4.5,9,6,8)
tfinnew=tfin
ruta=numeric(rut)
falta=seq(1:rut)
for (i in 1:rut){
  for (j in falta){
    if (j!=0){
      if (sort(tfin)[i]==tfin[j]){
        ruta[i]=j
        falta[j]=0
      }
      else {next}
    }
    else {next}
  }
}
if (tfin[ruta[1]]-floor(tfin[ruta[1]])!=0){
  tfinnew[ruta[1]]=ceiling(tfin[ruta[1]])
}
for (i in 2:rut){
  if (tfinnew[ruta[i]]-tfinnew[ruta[i-1]]!=1){
    tfinnew[ruta[i]]=tfinnew[ruta[i-1]]+1
  }
  else{next}
}
inirutnew=tfinnew-tiemporutas
if (sum(inirutnew<0)>0){
  inirutnew=inirutnew+abs(floor(min(inirutnew)))
}
nodrutfin=0
for (j in 1:rut){
  nodrut=nodrutfin
  nodrutfin=nodrut+sum(arcos[,4]==j)
  arcos[nodrut+1,3]=inirutnew[j]
#  inirut[j]=arcos[nodrut+1,3]
  for (i in (nodrut+2):nodrutfin){
    arcos[i,3]=arcos[i-1,3]+arcos[i-1,5]
  }
}
arcos[,3]=floor(arcos[,3])
