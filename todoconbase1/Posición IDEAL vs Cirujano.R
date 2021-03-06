#Posici�n
#IDEAL vs Cirujano

#source('F:/nombres.R')
#source('savePlots.R')

opar <- par(no.readonly=TRUE)

par(mfcol=c(4,1),mar=c(1,3,1,3),oma=c(4,2,3,0), pty="m", mgp=c(2,0.5,0), cex=.5)

######################################################################################
#DATOS

ideal1 <- base1$posIdealMed1
ideal2 <- base1$posIdealMed2
ideal3 <- base1$posIdealMed3
ideal4 <- base1$posIdealMed4
#soc1 <- base$posSOCMed1
#soc2 <- base$posSOCMed2
#soc3 <- base$posSOCMed3
#soc4 <- base$posSOCMed4
cirujano <- base1$posCir

idx <- !is.na(ideal1) & !is.na(ideal2) & !is.na(ideal3) & !is.na(ideal4) & !is.na(cirujano)
#idy <- !is.na(soc1) & !is.na(soc2) & !is.na(soc3) & !is.na(soc4) & !is.na(cirujano)
#length(which(idx))

#id <- id[idx]
x1 <- ideal1[idx]
x2 <- ideal2[idx]
x3 <- ideal3[idx]
x4 <- ideal4[idx]
#y1 <- soc1[idx]
#y2 <- soc2[idx]
#y3 <- soc3[idx]
#y4 <- soc4[idx]
z <- cirujano[idx]
#z <- cirujano[idy]
rango <- range(c(x1, x2, x3, x4, z))
#rango <- range(c(y1, y2, y3, y4, z))
m <- max(abs(rango))

######################################################################################

#gr�fica M�dico 1 Ideal vs Cirujano

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  ylim <- m * c(-1,1)
  idx <- !is.na(ideal1) & !is.na(cirujano)
  id <- id[idx]
  x1 <- ideal1[idx]
  z <- cirujano[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posici�n",
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1)
  
  for(i in seq(id)){                    
    if(x1[i] == z[i]){
      points(id[i], z[i], pch=19, col="black")
    }else if(x1[i] > z[i]){
      points(id[i], x1[i], pch=1, col="mediumvioletred")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], x1[i], id[i], z[i], lty=2, col="mediumvioletred")
    }else{
      points(id[i], x1[i], pch=1, col="mediumvioletred")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], x1[i], id[i], z[i], lty=4, col="lightseagreen")
    } 
  }
  #title("M�dico 1")
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  text(83,-2.5,"M�dico 1",pos=4)
}
#legend(0, -1, lwd=2, col=c("mediumvioletred", "lightseagreen"), legend=c("Ideal", "Cirujano"), cex=.8)

######################################################################################

#gr�fica M�dico 2 Ideal vs Cirujano

if(1){

  id <- seq(1,nrow(base1))
  xlim <- range(id)
  ylim <- m * c(-1,1)
  idx <- !is.na(ideal2) & !is.na(cirujano)
  id <- id[idx]
  x2 <- ideal2[idx]
  z <- cirujano[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posici�n",
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1)
  
  for(i in seq(id)){                    
    if(x2[i] == z[i]){
      points(id[i], z[i], pch=19, col="black")
    }else if(x2[i] > z[i]){
      points(id[i], x2[i], pch=1, col="mediumvioletred")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], x2[i], id[i], z[i], lty=2, col="mediumvioletred")
    }else{
      points(id[i], x2[i], pch=1, col="mediumvioletred")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], x2[i], id[i], z[i], lty=4, col="lightseagreen")
    } 
  }
  #title("M�dico 2")
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  text(83,-2.5,"M�dico 2",pos=4)
}



######################################################################################

#gr�fica M�dico 3 Ideal vs Cirujano

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  ylim <- m * c(-1,1)
  idx <- !is.na(ideal3) & !is.na(cirujano)
  id <- id[idx]
  x3 <- ideal3[idx]
  z <- cirujano[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posici�n",
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1)
  
  for(i in seq(id)){                    
    if(x3[i] == z[i]){
      points(id[i], z[i], pch=19, col="black")
    }else if(x3[i] > z[i]){
      points(id[i], x3[i], pch=1, col="mediumvioletred")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], x3[i], id[i], z[i], lty=2, col="mediumvioletred")
    }else{
      points(id[i], x3[i], pch=1, col="mediumvioletred")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], x3[i], id[i], z[i], lty=4, col="lightseagreen")
    } 
  }
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  #title("M�dico 3")
  text(83,-2.5,"M�dico 3",pos=4)
}



######################################################################################

#gr�fica M�dico 4 Ideal vs Cirujano

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  ylim <- m * c(-1,1)
  idx <- !is.na(ideal4) & !is.na(cirujano)
  id <- id[idx]
  x4 <- ideal4[idx]
  z <- cirujano[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posici�n", 
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1)
  axis(1, las=1)
  
  for(i in seq(id)){                    
    if(x4[i] == z[i]){
      points(id[i], z[i], pch=19, col="black")
    }else if(x4[i] > z[i]){
      points(id[i], x4[i], pch=1, col="mediumvioletred")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], x4[i], id[i], z[i], lty=2, col="mediumvioletred")
    }else{
      points(id[i], x4[i], pch=1, col="mediumvioletred")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], x4[i], id[i], z[i], lty=4, col="lightseagreen")
    } 
  }
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  #title("M�dico 4")
  text(83,-2.5,"M�dico 4",pos=4)
}



######################################################################################

mtext("Pacientes",side=1, line=2, outer=TRUE)
mtext("Posici�n",side=2, line=0, outer=TRUE)
title("Posici�n IDEAL M�dicos vs. Cirujano", outer=TRUE, line=0, cex.main=2)


par(opar)

savePlots(file="Posici�n IDEAL vs Cirujano", width=4, height=6, 
          EPS=FALSE, PDF=TRUE, PNG=TRUE)


