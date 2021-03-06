#Extensi�n 
#IDEAL vs SOC

#source('F:/nombres.R')
#source('savePlots.R')

opar <- par(no.readonly=TRUE)

par(mfcol=c(4,1),mar=c(1,3,1,3),oma=c(4,2,3,0), pty="m", mgp=c(2,0.5,0), cex=.5)

######################################################################################

#DATOS

ideal1 <- base1$extIdealMed1
ideal2 <- base1$extIdealMed2
ideal3 <- base1$extIdealMed3
ideal4 <- base1$extIdealMed4
soc1 <- base1$extSOCMed1
soc2 <- base1$extSOCMed2
soc3 <- base1$extSOCMed3
soc4 <- base1$extSOCMed4
cirujano <- base1$extCir

idx <- !is.na(ideal1) & !is.na(ideal2) & !is.na(ideal3) & !is.na(ideal4) & !is.na(cirujano)
idy <- !is.na(soc1) & !is.na(soc2) & !is.na(soc3) & !is.na(soc4) & !is.na(cirujano)
#idx <- is.na(ideal1) & !is.na(ideal2) & !is.na(ideal3) & !is.na(ideal4) & !is.na(soc1) & !is.na(soc2) & !is.na(soc3) & !is.na(soc4)
#length(which(idx))

id <- id[idx]
x1 <- ideal1[idx]
x2 <- ideal2[idx]
x3 <- ideal3[idx]
x4 <- ideal4[idx]
y1 <- soc1[idy]
y2 <- soc2[idy]
y3 <- soc3[idy]
y4 <- soc4[idy]
z <- cirujano[idx]
#z <- cirujano[idy]
rango1 <- range(c(x1, x2, x3, x4, z))
rango2 <- range(c(y1, y2, y3, y4, z))
m1 <- max(abs(rango1))
m2 <- max(abs(rango2))
m <- max(m1, m2)

######################################################################################

#gr�fica M�dico 1 IDEAL vs SOC

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  ylim <- m * c(0,1)
  #ylim <- c(0,4)
  idx <- !is.na(ideal1) & !is.na(soc1)
  id <- id[idx]
  x1 <- ideal1[idx]
  y1 <- soc1[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posici�n",
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1)
  
  for(i in seq(id)){                    
    if(x1[i] == y1[i]){
      points(id[i], y1[i], pch=19, col="black")
    }else if(x1[i] > y1[i]){
      points(id[i], x1[i], pch=1, col="mediumvioletred")
      points(id[i], y1[i], pch=0, col="navy")
      segments(id[i], x1[i], id[i], y1[i], lty=2, col="mediumvioletred")
    }else{
      points(id[i], x1[i], pch=1, col="mediumvioletred")
      points(id[i], y1[i], pch=0, col="navy")
      segments(id[i], x1[i], id[i], y1[i], lty=3, col="navy")
    } 
  }
  abline(v=c(20, 40, 60, 80),lty=3,col=grey(.8))
  text(83,0.1,"M�dico 1",pos=4)
}
#legend(0, -1, lwd=2, col=c("red", "blue"), legend=c("Ideal", "SOC"), cex=.8)

######################################################################################

#gr�fica M�dico 2 IDEAL vs SOC

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  ylim <- m * c(0,1)
  #ylim <- c(0,4)
  idx <- !is.na(ideal2) & !is.na(soc2)
  id <- id[idx]
  x2 <- ideal2[idx]
  y2 <- soc2[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posici�n",
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1)
  
  for(i in seq(id)){                    
    if(x2[i] == y2[i]){
      points(id[i], y2[i], pch=19, col="black")
    }else if(x2[i] > y2[i]){
      points(id[i], x2[i], pch=1, col="mediumvioletred")
      points(id[i], y2[i], pch=0, col="navy")
      segments(id[i], x2[i], id[i], y2[i], lty=2, col="mediumvioletred")
    }else{
      points(id[i], x2[i], pch=1, col="mediumvioletred")
      points(id[i], y2[i], pch=0, col="navy")
      segments(id[i], x2[i], id[i], y2[i], lty=3, col="navy")
    } 
  }
  abline(v=c(20, 40, 60, 80),lty=3,col=grey(.8))
  text(83,.1,"M�dico 2",pos=4)
  }
#legend(0, -1, lwd=2, col=c("red", "blue"), legend=c("Ideal", "SOC"), cex=.8)

######################################################################################

#gr�fica M�dico 3 IDEAL vs SOC

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  ylim <- m * c(0,1)
  #ylim <- c(0,4)
  idx <- !is.na(ideal3) & !is.na(soc3)
  id <- id[idx]
  x3 <- ideal3[idx]
  y3 <- soc3[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posici�n",
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1)
  
  for(i in seq(id)){                    
    if(x3[i] == y3[i]){
      points(id[i], y3[i], pch=19, col="black")
    }else if(x3[i] > y3[i]){
      points(id[i], x3[i], pch=1, col="mediumvioletred")
      points(id[i], y3[i], pch=0, col="navy")
      segments(id[i], x3[i], id[i], y3[i], lty=2, col="mediumvioletred")
    }else{
      points(id[i], x3[i], pch=1, col="mediumvioletred")
      points(id[i], y3[i], pch=0, col="navy")
      segments(id[i], x3[i], id[i], y3[i], lty=3, col="navy")
    } 
  }
  abline(v=c(20, 40, 60, 80),lty=3,col=grey(.8))
  text(83,.1,"M�dico 3",pos=4)
}
#legend(0, -1, lwd=2, col=c("red", "blue"), legend=c("Ideal", "SOC"), cex=.8)

######################################################################################

#gr�fica M�dico 4 Ideal vs Cirujano

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  ylim <- m * c(0,1)
  #ylim <- c(0,4)
  idx <- !is.na(ideal4) & !is.na(soc4)
  id <- id[idx]
  x4 <- ideal4[idx]
  y4 <- soc4[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posici�n",
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1) 
  axis(1, las=1)
  
  for(i in seq(id)){                    
    if(x4[i] == y4[i]){
      points(id[i], y4[i], pch=19, col="black")
    }else if(x4[i] > y4[i]){
      points(id[i], x4[i], pch=1, col="mediumvioletred")
      points(id[i], y4[i], pch=0, col="navy")
      segments(id[i], x4[i], id[i], y4[i], lty=2, col="mediumvioletred")
    }else{
      points(id[i], x4[i], pch=1, col="mediumvioletred")
      points(id[i], y4[i], pch=0, col="navy")
      segments(id[i], x4[i], id[i], y4[i], lty=3, col="navy")
    } 
  }
  abline(v=c(20, 40, 60, 80),lty=3,col=grey(.8))
  text(83,.1,"M�dico 4",pos=4)
}
#legend(0, -1, lwd=2, col=c("red", "blue"), legend=c("Ideal", "SOC"), cex=.8)


######################################################################################

mtext("Pacientes",side=1, line=2, outer=TRUE)
mtext("Extensi�n",side=2, line=0, outer=TRUE)
title("Extensi�n IDEAL vs. SOC", outer=TRUE, line=0, cex.main=2)

par(opar)

#savePlots(file="Extensi�n IDEAL vs SOC", width=4, height=6, EPS=FALSE, PDF=TRUE, PNG=TRUE)


