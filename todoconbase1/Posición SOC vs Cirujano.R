#Posición
#SOC = standard of care    vs Cirujano

#source('F:/nombres.R')
#source('savePlots.R')

opar <- par(no.readonly=TRUE)

par(mfcol=c(4,1),mar=c(1,3,1,3),oma=c(4,2,3,0), pty="m", mgp=c(2,0.5,0), cex=.5)

######################################################################################
#DATOS

#ideal1 <- base$posIdealMed1
#ideal2 <- base$posIdealMed2
#ideal3 <- base$posIdealMed3
#ideal4 <- base$posIdealMed4
soc1 <- base1$posSOCMed1
soc2 <- base1$posSOCMed2
soc3 <- base1$posSOCMed3
soc4 <- base1$posSOCMed4
cirujano <- base1$posCir

#idx <- !is.na(ideal1) & !is.na(ideal2) & !is.na(ideal3) & !is.na(ideal4) & !is.na(cirujano)
idy <- !is.na(soc1) & !is.na(soc2) & !is.na(soc3) & !is.na(soc4) & !is.na(cirujano)
#length(which(idx))

#id <- id[idx]
#x1 <- ideal1[idx]
#x2 <- ideal2[idx]
#x3 <- ideal3[idx]
#x4 <- ideal4[idx]
y1 <- soc1[idy]
y2 <- soc2[idy]
y3 <- soc3[idy]
y4 <- soc4[idy]
#z <- cirujano[idx]
z <- cirujano[idy]
#rango <- range(c(x1, x2, x3, x4, z))
rango <- range(c(y1, y2, y3, y4, z))
m <- max(abs(rango))

######################################################################################

#gráfica Médico 1 SOC vs Cirujano

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  #ylim <- m * c(-1,1)
  ylim <- c(-2.9,2.9)            #### aquí cambié los lim porque sale super pegado
  idy <- !is.na(soc1) & !is.na(cirujano)
  id <- id[idy]
  y1 <- soc1[idy]
  z <- cirujano[idy]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posición",
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1)
  
  for(i in seq(id)){                    
    if(y1[i] == z[i]){
      points(id[i], z[i], pch=19, col="black")
    }else if(y1[i] > z[i]){
      points(id[i], y1[i], pch=0, col="navy")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], y1[i], id[i], z[i], lty=3, col="navy")
    }else{
      points(id[i], y1[i], pch=0, col="navy")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], y1[i], id[i], z[i], lty=4, col="lightseagreen")
    } 
  }
  #title("Médico 1")
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  text(83,-2.5,"Médico 1",pos=4)
}
#legend(0, -1, lwd=2, col=c("red", "blue"), legend=c("Ideal", "SOC"), cex=.8)

######################################################################################

#gráfica Médico 2 SOC vs Cirujano

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  #ylim <- m * c(-1,1)
  ylim <- c(-2.617994,2.617994)
  idy <- !is.na(soc2) & !is.na(cirujano)
  id <- id[idy]
  y2 <- soc2[idy]
  z <- cirujano[idy]
 # plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posición")
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posición",
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1)
  
  for(i in seq(id)){                    
    if(y2[i] == z[i]){
      points(id[i], z[i], pch=19, col="black")
    }else if(y2[i] > z[i]){
      points(id[i], y2[i], pch=0, col="navy")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], y2[i], id[i], z[i], lty=3, col="navy")
    }else{
      points(id[i], y2[i], pch=0, col="navy")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], y2[i], id[i], z[i], lty=4, col="lightseagreen")
    } 
  }
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  text(83,-2.5,"Médico 2",pos=4)
}



######################################################################################

#gráfica Médico 3 SOC vs Cirujano

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  #ylim <- m * c(-1,1)
  ylim <- c(-2.617994,2.617994)
  idy <- !is.na(soc3) & !is.na(cirujano)
  id <- id[idy]
  y3 <- soc3[idy]
  z <- cirujano[idy]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posición",
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1)
  
  for(i in seq(id)){                    
    if(y3[i] == z[i]){
      points(id[i], z[i], pch=19, col="black")
    }else if(y3[i] > z[i]){
      points(id[i], y3[i], pch=0, col="navy")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], y3[i], id[i], z[i], lty=3, col="navy")
    }else{
      points(id[i], y3[i], pch=0, col="navy")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], y3[i], id[i], z[i], lty=4, col="lightseagreen")
    } 
  }
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  text(83,-2.5,"Médico 3",pos=4)
}



######################################################################################

#gráfica Médico 4 SOC vs Cirujano

if(1){
  
  id <- seq(1,nrow(base1))
  xlim <- range(id)
  #ylim <- m * c(-1,1)
  ylim <- c(-2.617994,2.617994)
  idy <- !is.na(soc4) & !is.na(cirujano)
  id <- id[idy]
  y4 <- soc4[idy]
  z <- cirujano[idy]
  plot(0, 0, xlim=xlim, ylim=ylim, type="n", xlab="Pacientes", ylab="Posición",
       axes=FALSE, ann=FALSE, frame=TRUE)
  axis(2, las=1)
  axis(1, las=1)
  
  for(i in seq(id)){                    
    if(y4[i] == z[i]){
      points(id[i], z[i], pch=19, col="black")
    }else if(y4[i] > z[i]){
      points(id[i], y4[i], pch=0, col="navy")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], y4[i], id[i], z[i], lty=3, col="navy")
    }else{
      points(id[i], y4[i], pch=0, col="navy")
      points(id[i], z[i], pch=2, col="lightseagreen")
      segments(id[i], y4[i], id[i], z[i], lty=2, col="lightseagreen")
    } 
  }
  abline(v=c(20,40,60,80),lty=3,col=grey(.8))
  #title("Médico 4")
  text(83,-2.5,"Médico 4",pos=4)
}



######################################################################################


mtext("Pacientes",side=1, line=2, outer=TRUE)
mtext("Posición",side=2, line=0, outer=TRUE)
title("Posición SOC Médicos vs Cirujano", outer=TRUE, line=0, cex.main=2)

par(opar)

savePlots(file="PosSOCvsCir", width=4, height=6, EPS=FALSE, PDF=TRUE, PNG=TRUE)


