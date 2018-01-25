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
  xc <- base1$posCir[idx]
  x1i <- base1$posIdealMed1[idx]
  x2i <- base1$posIdealMed2[idx]
  x3i <- base1$posIdealMed3[idx]
  x4i <- base1$posIdealMed4[idx]
  x1s <- base1$posSOCMed1[idx]
  x2s <- base1$posSOCMed2[idx]
  x3s <- base1$posSOCMed3[idx]
  x4s <- base1$posSOCMed4[idx]
  
  XC <- xc[!is.na(xc)]
  S2C <- var(XC)
  X1i <- x1i[!is.na(x1i)]
  X2i <- x2i[!is.na(x2i)]
  X3i <- x3i[!is.na(x3i)]
  X4i <- x4i[!is.na(x4i)]
  X1s <- x1s[!is.na(x1s)]
  X2s <- x2s[!is.na(x2s)]
  X3s <- x3s[!is.na(x3s)]
  X4s <- x4s[!is.na(x4s)]
  
  r1i[i]<- var(X1i)/S2C
  r2i[i]<- var(X2i)/S2C
  r3i[i]<- var(X3i)/S2C
  r4i[i]<- var(X4i)/S2C
  r1s[i]<- var(X1s)/S2C
  r2s[i]<- var(X2s)/S2C
  r3s[i]<- var(X3s)/S2C
  r4s[i]<- var(X4s)/S2C
}

mideal <- matrix(c(r1i,r2i,r3i,r4i), nrow=500)
msoc <- matrix(c(r1s,r2s,r3s,r4s), nrow=500)

max(mideal)
max(msoc)

x <- quantile(mideal, probs=seq(.75,1,by=.05))
y <- quantile(msoc, probs=seq(.75,1,by=.05))
length(which(mideal > 4))/2000
length(which(msoc > 4))/2000
length(which(r1i>1))/500


xlim=c(0,4)

dotPlot(r1i,xlim=xlim)
abline(v=1, col="grey")
title("Pos Ideal Med1")
dotPlot(r2i,xlim=xlim)
abline(v=1, col="grey")
title("Pos Ideal Med2")
dotPlot(r3i,xlim=xlim)
abline(v=1, col="grey")
title("Pos Ideal Med3")
dotPlot(r4i,xlim=xlim)
abline(v=1, col="grey")
title("Pos Ideal Med4")
dotPlot(r1s,xlim=xlim)
abline(v=1, col="grey")
title("Pos SOC Med1")
dotPlot(r2s,xlim=xlim)
abline(v=1, col="grey")
title("Pos SOC Med2")
dotPlot(r3s,xlim=xlim)
abline(v=1, col="grey")
title("Pos SOC Med3")
dotPlot(r4s,xlim=xlim)
abline(v=1, col="grey")
title("Pos SOC Med4")

par(opar)