#Posición
#IDEAL vs SOC

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
soc1 <- base1$posSOCMed1
soc2 <- base1$posSOCMed2
soc3 <- base1$posSOCMed3
soc4 <- base1$posSOCMed4
cirujano <- base1$posCir

#idx <- !is.na(ideal1) & !is.na(ideal2) & !is.na(ideal3) & !is.na(ideal4) & !is.na(cirujano)
#idy <- !is.na(soc1) & !is.na(soc2) & !is.na(soc3) & !is.na(soc4) & !is.na(cirujano)
#idx <- is.na(ideal1) & !is.na(ideal2) & !is.na(ideal3) & !is.na(ideal4) & !is.na(soc1) & !is.na(soc2) & !is.na(soc3) & !is.na(soc4)
#length(which(idx))

#id <- id[idx]
#x1 <- ideal1[idx]
#x2 <- ideal2[idx]
#x3 <- ideal3[idx]
#x4 <- ideal4[idx]
#y1 <- soc1[idy]
#y2 <- soc2[idy]
#y3 <- soc3[idy]
#y4 <- soc4[idy]
#z <- cirujano[idx]
#z <- cirujano[idy]
#rango1 <- range(c(x1, x2, x3, x4, z))
#rango2 <- range(c(y1, y2, y3, y4, z))
#m1 <- max(abs(rango1))
#m2 <- max(abs(rango2))
#m <- max(m1, m2)
#salen diferentes los rangos
#???????????

######################################################################################

#gráfica Médico 1 IDEAL vs SOC

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  #ylim <- m * c(-1,1)
  ylim <- c(-2.617994,2.617994)
  idx <- !is.na(ideal1) & !is.na(soc1)
  id <- id[idx]
  x1 <- ideal1[idx]
  y1 <- soc1[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posición",
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
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  text(83,-2.5,"Médico 1",pos=4)
}
#legend(0, -1, lwd=2, col=c("mediumvioletred", "navy"), legend=c("Ideal", "SOC"), cex=.8)

######################################################################################

#gráfica Médico 2 IDEAL vs SOC

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  #ylim <- m * c(-1,1)
  ylim <- c(-2.617994,2.617994)
  idx <- !is.na(ideal2) & !is.na(soc2)
  id <- id[idx]
  x2 <- ideal2[idx]
  y2 <- soc2[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posición",
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
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  text(83,-2.5,"Médico 2",pos=4)
}
#legend(0, -1, lwd=2, col=c("mediumvioletred", "navy"), legend=c("Ideal", "SOC"), cex=.8)

######################################################################################

#gráfica Médico 3 IDEAL vs SOC

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  #ylim <- m * c(-1,1)
  ylim <- c(-2.617994,2.617994)
  idx <- !is.na(ideal3) & !is.na(soc3)
  id <- id[idx]
  x3 <- ideal3[idx]
  y3 <- soc3[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posición",
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
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  text(83,-2.5,"Médico 3",pos=4)
}
#legend(0, -1, lwd=2, col=c("mediumvioletred", "navy"), legend=c("Ideal", "SOC"), cex=.8)

######################################################################################

#gráfica Médico 4 Ideal vs Cirujano

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  #ylim <- m * c(-1,1)
  ylim <- c(-2.617994,2.617994)
  idx <- !is.na(ideal4) & !is.na(soc4)
  id <- id[idx]
  x4 <- ideal4[idx]
  y4 <- soc4[idx]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posición",
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
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  text(84,-2.5,"Médico 4",pos=4)
}
#legend(0, -1, lwd=2, col=c("mediumvioletred", "navy"), legend=c("Ideal", "SOC"), cex=.8)


######################################################################################

mtext("Pacientes",side=1, line=2, outer=TRUE)
mtext("Posición",side=2, line=0, outer=TRUE)
title("Posición IDEAL vs SOC", outer=TRUE, line=0, cex.main=2)

par(opar)

savePlots(file="PosIDEALvsSOC", width=4, height=6, EPS=FALSE, PDF=TRUE, PNG=TRUE)


