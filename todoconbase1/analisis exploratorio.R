genero <- base1$genero
edad <- base1$edad
lado <- base1$lado

#opar <- par(no.readonly=TRUE)

#par(mfcol=c(2,2),mar=c(2,3,3,3),oma=c(4,2,3,0), pty="m", mgp=c(2,0.5,0), cex=.5)


###############################################################################################

#EDAD

hist(edad, breaks=seq(10,60,by=5), ylim=c(0,20), col="hotpink4", main="Edad de pacientes", 
     ylab="Frecuencia", axes=FALSE)
axis(1,las=1)
axis(2,las=1)

#savePlots(file="EDADES", 
#          width=6, height=4, EPS=FALSE, PDF=TRUE, PNG=FALSE)


###############################################################################################

#GÉNERO


plot(genero, col="hotpink4", ylim=c(0,70), main="Pacientes por género", 
     ylab="Frecuencia", axes=FALSE)
axis(2, las=1)
# ¿Se puede poner de diferentes colores las columnas?
# Ejem: rosa para mujeres, azul ppara hombres
# En lugar de que salga "F" y "M", ¿se puede poner "Masculino", "Femenino"?
# ¿Se puede que las columnas sean más angostas?
# ¿convendrá hacer mejor gráfica de pastel?

#savePlots(file="GENERO", 
#          width=6, height=4, EPS=FALSE, PDF=TRUE, PNG=FALSE)

# M=1
# F=0

#id <- seq(1,nrow(base1))
#g <- genero

#for(i in seq(id)){
#  if(g[i]=="M"){
#    g[i]=1
#  }else{
#    g[i]=0
#  }
#}
#print(g)
#g <- as.numeric(g)
#print(g)

#hist(g ,nclass=2, xlim=c(0,1),ylim=c(0,70), col="blue", main="Pacientes por género",
#     ylab="Frecuencia", axes=FALSE)
#axis(2,las=1)



###############################################################################################

#LADO

#L = 40
#R = 47

plot(lado, col="hotpink4", ylim=c(0,50), main="Lado en que se localiza la herida", 
     ylab="Frecuencia", axes=FALSE)
axis(2, las=1)

#savePlots(file="LADO", 
#          width=6, height=4, EPS=FALSE, PDF=TRUE, PNG=FALSE)


# R=1
# L=0

#id <- seq(1,nrow(base1))
#l <- lado

#for(i in seq(id)){
#  if(l[i]=="R"){
#    l[i]=1
#  }else{
#    l[i]=0
#  }
#}
#print(l)
#l <- as.numeric(l)
#print(l)

#hist(l ,nclass=2, xlim=c(0,1),ylim=c(0,50), col="pink", main="Lado en que se encuentra la herida",
#     ylab="Frecuencia", axes=FALSE)
#axis(2,las=1)




#par(opar)      