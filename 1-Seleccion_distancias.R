#Selección de los datos. Elegir directorios y conjunto de datos.
#setwd("~/MTE/3er Cuatrimestre/TFM")
#load("~/MTE/3er Cuatrimestre/TFM/datos_matrices.RData")

# Selección de las distancias que buscamos en las distintas rutas a seleccionar.
# Los conjuntos de datos con los nombres de los días de la semana incluyen todos los nodos que 
# se utilizan actualmente en cada uno de esos días.
# Las rutas por separado se especifican con un número al final (del 1 al 7 y del 1 al 8 el miércoles)
# y una letra o un par de letras al principio, siendo J=Jueves, L=Lunes, Ma=Martes, Mi=Miércoles y V=Viernes 
loc=sort(c(1,MaR2))
n=length(loc) # Cantidad de nodos
D=M[loc,loc] # Matriz de costes de distancias entre nodos
D=cbind(loc,D) # Colocación nodos por filas y columnas
(D=rbind(c(0,loc),D))
write.table(D,file="nuevosdatos.txt",row.names=F,col.names=F) # Archivo matriz distancias
car=c(0,rep(0.5,n-1)) # Costes carga en cada nodo
P=cbind(loc,car) # Colocación nodos
write.table(P,file="nuevosdatos2.txt",row.names=F,col.names=F) # Archivo vector cargas
names=nombres[loc] # Nombre localidades
write.table(t(names),file="nuevosdatos3.txt",row.names=F,col.names=F) # Archivo nombre localidades
write.table(t(loc),file="nuevosdatos4.txt",row.names=F,col.names=F) # Archivo nodos generales