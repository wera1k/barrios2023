opar <- par(no.readonly=TRUE)
par(mfcol=c(2,2),mar=c(1,3,1,3),oma=c(4,2,3,0), pty="m", mgp=c(2,0.5,0), cex=.5)

N <- 500
n <- 20  
Idx <- seq(87)

r1i <- rep(NA,N)
r2i <- rep(NA,N)
r3i <- rep(NA,N)
r4i <- rep(NA,N)
r1s <- rep(NA,N)
r2s <- rep(NA,N)
r3s <- rep(NA,N)
r4s <- rep(NA,N)

for(i in seq(N)){
  idx <- sample(Idx,20)
 
  x1i <- base1$extIdealMed1[idx]
  x2i <- base1$extIdealMed2[idx]
  x3i <- base1$extIdealMed3[idx]
  x4i <- base1$extIdealMed4[idx]
  x1s <- base1$extSOCMed1[idx]
  x2s <- base1$extSOCMed2[idx]
  x3s <- base1$extSOCMed3[idx]
  x4s <- base1$extSOCMed4[idx]
  
  X1i <- x1i[!is.na(x1i)]
  X2i <- x2i[!is.na(x2i)]
  X3i <- x3i[!is.na(x3i)]
  X4i <- x4i[!is.na(x4i)]
  X1s <- x1s[!is.na(x1s)]
  X2s <- x2s[!is.na(x2s)]
  X3s <- x3s[!is.na(x3s)]
  X4s <- x4s[!is.na(x4s)]
  
  r1i[i]<- var(X1i)/var(X1s)
  r2i[i]<- var(X2i)/var(X2s)
  r3i[i]<- var(X3i)/var(X3s)
  r4i[i]<- var(X4i)/var(X4s)

}

mmedicos <- matrix(c(r1i,r2i,r3i,r4i), nrow=500)

max(mmedicos)

x <- quantile(mmedicos, probs=seq(.75,1,by=.05))

length(which(mmedicos > 4))/2000
length(which(r1i>1))/500

xlim=c(0,3)

dotPlot(r1i,xlim=xlim)
abline(v=1, col="grey")
title("Pos Med1 Ideal vs SOC")
dotPlot(r2i,xlim=xlim)
abline(v=1, col="grey")
title("Pos Med1 Ideal vs SOC")
dotPlot(r3i,xlim=xlim)
abline(v=1, col="grey")
title("Pos Med1 Ideal vs SOC")
dotPlot(r4i,xlim=xlim)
abline(v=1, col="grey")
title("Pos Med1 Ideal vs SOC")


par(opar)