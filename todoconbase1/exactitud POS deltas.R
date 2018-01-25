opar <- par(no.readonly=TRUE)
par(mfcol=c(2,2),mar=c(1,3,1,3),oma=c(4,2,3,0), pty="m", mgp=c(2,0.5,0), cex=.5)

N <- 500
n <- 20  
Idx <- seq(87)

x1i <- rep(NA,N)
x2i <- rep(NA,N)
x3i <- rep(NA,N)
x4i <- rep(NA,N)
x1s <- rep(NA,N)
x2s <- rep(NA,N)
x3s <- rep(NA,N)
x4s <- rep(NA,N)


d1ipos <- base1$posIdealMed1 - base1$posCir
d2ipos <- base1$posIdealMed2 - base1$posCir
d3ipos <- base1$posIdealMed3 - base1$posCir
d4ipos <- base1$posIdealMed4 - base1$posCir
d1spos <- base1$posSOCMed1 - base1$posCir
d2spos <- base1$posSOCMed2 - base1$posCir
d3spos <- base1$posSOCMed3 - base1$posCir
d4spos <- base1$posSOCMed4 - base1$posCir

for(i in seq(N)){
  idx <- sample(Idx,20)
  #xc <- base1$posCir[idx]
  x1i <- d1ipos[idx]
  x2i <- d2ipos[idx]
  x3i <- d3ipos[idx]
  x4i <- d4ipos[idx]
  x1s <- d1spos[idx]
  x2s <- d2spos[idx]
  x3s <- d3spos[idx]
  x4s <- d4spos[idx]
  
  #XC <- xc[!is.na(xc)]
  #S2C <- var(XC)
  X1i <- x1i[!is.na(x1i)]
  X2i <- x2i[!is.na(x2i)]
  X3i <- x3i[!is.na(x3i)]
  X4i <- x4i[!is.na(x4i)]
  X1s <- x1s[!is.na(x1s)]
  X2s <- x2s[!is.na(x2s)]
  X3s <- x3s[!is.na(x3s)]
  X4s <- x4s[!is.na(x4s)]
  
  r1i[i]<- mean(X1i)
  r2i[i]<- mean(X2i)
  r3i[i]<- mean(X3i)
  r4i[i]<- mean(X4i)
  r1s[i]<- mean(X1s)
  r2s[i]<- mean(X2s)
  r3s[i]<- mean(X3s)
  r4s[i]<- mean(X4s)
}

mideal <- matrix(c(r1i,r2i,r3i,r4i), nrow=500)
msoc <- matrix(c(r1s,r2s,r3s,r4s), nrow=500)

max(mideal)
min(mideal)
max(msoc)
min(msoc)

x <- quantile(mideal, probs=seq(.75,1,by=.05))
y <- quantile(msoc, probs=seq(.75,1,by=.05))
length(which(mideal > 4))/2000
length(which(msoc > 4))/2000
length(which(r1i>1))/500


xlim=c(-2,2)

dotPlot(r1i,xlim=xlim)
abline(v=0, col="grey")
title("Pos Ideal delta(Med1-Cir)")
dotPlot(r2i,xlim=xlim)
abline(v=0, col="grey")
title("Pos Ideal delta(Med2-Cir)")
dotPlot(r3i,xlim=xlim)
abline(v=0, col="grey")
title("Pos Ideal delta(Med3-Cir)")
dotPlot(r4i,xlim=xlim)
abline(v=0, col="grey")
title("Pos Ideal delta(Med4-Cir)")
dotPlot(r1s,xlim=xlim)
abline(v=0, col="grey")
title("Pos SOC delta(Med1-Cir)")
dotPlot(r2s,xlim=xlim)
abline(v=0, col="grey")
title("Pos SOC delta(Med2-Cir)")
dotPlot(r3s,xlim=xlim)
abline(v=0, col="grey")
title("Pos SOC delta(Med3-Cir)")
dotPlot(r4s,xlim=xlim)
abline(v=0, col="grey")
title("Pos SOC delta(Med4-Cir)")

par(opar)