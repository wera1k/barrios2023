# Posición por paciente

#source('savePlots.R')

opar <- par(no.readonly=TRUE)
par(mfrow=c(5,4),mar=c(1,3,1,2),oma=c(4,3,3,2), pty="m", mgp=c(2,0.5,0), cex=.5)

N <- nrow(base1)
vertical <- c(4*seq(N/4)-3, 85)
horizontal <- NULL
for(i in seq(N/20)) {  
  tt <- 20*i  
  horizontal <- c(horizontal, tt-3, tt-2, tt-1, tt)
}
horizontal <- c(horizontal,84,85,86,87)  

kont <- 0
for(id in seq(nrow(base1))) {
  
  attach(base1[id,])
  x1 <- posIdealMed1
  y1 <- posSOCMed1
  x2 <- posIdealMed2
  y2 <- posSOCMed2  
  x3 <- posIdealMed3
  y3 <- posSOCMed3
  x4 <- posIdealMed4
  y4 <- posSOCMed4
  z <- posCir
  detach()
  xlim <- c(1,5)
  ylim <- 2.617994*c(-1,1)  #range(c(x1,y1,x2,y2,x3,y3,x4,y4,z))
  
  #  if(all(is.na(c(x1,y1,x2,y2,x3,y3,x4,y4,z)))) next()
  
  kont <- kont + 1
 
   
  plot(0,0, type="n", xlim=xlim, ylim=ylim, axes=FALSE, frame=TRUE, 
       ann=FALSE, xlab="medico", ylab="posición")
  text(4.2,-2.3,paste("P",id),pos=4)
  if(any(kont == vertical)) axis(2, las=1)
  if(any(kont == horizontal)) axis(1,at=seq(1,5),labels=c(1,2,3,4,"c"))

  points(seq(1,4), c(x1,x2,x3,x4), pch=1, col="mediumvioletred", cex=2)
  points(seq(1,4), c(y1,y2,y3,y4), pch=0, col="navy", cex=1.5)
  points(5, z, pch=2, col="lightseagreen")
  abline(h=z, lty=4, col="lightseagreen")

  if(!is.na(x1-y1)) segments(1, x1, 1, y1, col=c("navy",0,"mediumvioletred")[sign(x1-y1)+2])
  if(!is.na(x2-y2)) segments(2, x2, 2, y2, col=c("navy",0,"mediumvioletred")[sign(x2-y2)+2])
  if(!is.na(x3-y3)) segments(3, x3, 3, y3, col=c("navy",0,"mediumvioletred")[sign(x3-y3)+2])
  if(!is.na(x4-y4)) segments(4, x4, 4, y4, col=c("navy",0,"mediumvioletred")[sign(x4-y4)+2])
  

  w <- kont/20
  if(w == trunc(w)) {
    mtext("Médicos",side=1, line=2, outer=TRUE)
    mtext("Posición",side=2, line=0, outer=TRUE)
    #  title("Comparación de mediciones por Paciente") 
#    savePlots(paste("Posición por Pacientes pg.",w,sep=""),6,7,FALSE,TRUE,TRUE,FALSE)
    #break()
    next()
  }
  
  if(w == 87/20) {
    mtext("Médicos",side=1, line=2, outer=TRUE)
    mtext("Posición",side=2, line=0, outer=TRUE)
#    savePlots("Posición por Pacientes pg.5",6,7,FALSE, TRUE,TRUE,FALSE)
  }
  
}