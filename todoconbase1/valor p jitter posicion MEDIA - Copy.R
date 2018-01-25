
opar <- par(no.readonly=TRUE)

par(mfcol=c(2,1),mar=c(1,3,1,3),oma=c(4,2,3,0), pty="m", mgp=c(2,0.5,0), cex=.5)

###################################################################
set.seed(17092016)
###################################################################
# Médico 1 Ideal
d <- base1$posIdealMed1 - base1$posCir
d1i <- d[!is.na(d)]
mu <- 0
media <- mean(d1i)
s <- sd(d1i) 
n <- length(d1i)
estt <- (media-mu)/(s/sqrt(n))
et <- t.test(d,y=NULL,alternative="two.sided",mu=0)
p1i <- 2*pt(-abs(estt), df=n-1)
print(paste("el valor p Med1.Ideal es", p1i))

ft <- 10
dj <- jitter(base1$posIdealMed1, factor=ft, amount=NULL) - base1$posCir
dj1i <- dj[!is.na(dj)]
mu <- 0
mediaj <- mean(dj1i)
s <- sd(dj1i) 
n <- length(dj1i)
esttj <- (mediaj-mu)/(s/sqrt(n))
etj <- t.test(dj,y=NULL,alternative="two.sided",mu=0)
pj1i <- 2*pt(-abs(esttj), df=n-1)
print(paste("el valor p Med1.Ideal.j es", round(pj1i,5)))
pa <- base1$paciente
M <- max(d[!is.na(d)],dj[!is.na(dj)])
m <- min(d[!is.na(d)],dj[!is.na(dj)])
plot(pa,d,ylim=c(m,M))
points(dj,col="red")
abline(v=c(20, 40, 60, 80),lty=3,col=grey(.7))
title("deltas Pos(med-cir) Médico1 Ideal vs jitter")

###################################################################
# Médico 1 SOC
d <- base1$posSOCMed1 - base1$posCir
d1s <- d[!is.na(d)]
mu <- 0
media <- mean(d1s)
s <- sd(d1s) 
n <- length(d1s)
estt <- (media-mu)/(s/sqrt(n))
et <- t.test(d,y=NULL,alternative="two.sided",mu=0)
p1s <- 2*pt(-abs(estt), df=n-1)
print(paste("el valor p Med1.SOC es", p1s))

dj <- jitter(base1$posSOCMed1, factor=ft, amount=NULL) - base1$posCir
dj1s <- dj[!is.na(dj)]
mu <- 0
mediaj <- mean(dj1s)
s <- sd(dj1s) 
n <- length(dj1s)
esttj <- (mediaj-mu)/(s/sqrt(n))
etj <- t.test(dj,y=NULL,alternative="two.sided",mu=0)
pj1s <- 2*pt(-abs(esttj), df=n-1)
print(paste("el valor p Med1.SOC.j es", pj1s))

M <- max(d[!is.na(d)],dj[!is.na(dj)])
m <- min(d[!is.na(d)],dj[!is.na(dj)])
plot(pa,d,ylim=c(m,M))
points(dj,col="red")
abline(v=c(20, 40, 60, 80),lty=3,col=grey(.7))
title("deltas Pos(med-cir) Médico1 SOC vs jitter")

###################################################################
###################################################################
# Médico 2 Ideal
d <- base1$posIdealMed2 - base1$posCir
d2i <- d[!is.na(d)]
mu <- 0
media <- mean(d2i)
s <- sd(d2i) 
n <- length(d2i)
estt <- (media-mu)/(s/sqrt(n))
et <- t.test(d,y=NULL,alternative="two.sided",mu=0)
p2i <- 2*pt(-abs(estt), df=n-1)
print(paste("el valor p Med2.Ideal es", p2i))

dj <- jitter(base1$posIdealMed2, factor=ft, amount=NULL) - base1$posCir
dj2i <- dj[!is.na(dj)]
mu <- 0
mediaj <- mean(dj2i)
s <- sd(dj2i) 
n <- length(dj2i)
esttj <- (mediaj-mu)/(s/sqrt(n))
etj <- t.test(dj,y=NULL,alternative="two.sided",mu=0)
pj2i <- 2*pt(-abs(esttj), df=n-1)
print(paste("el valor p Med2.Ideal.j es", pj2i))

M <- max(d[!is.na(d)],dj[!is.na(dj)])
m <- min(d[!is.na(d)],dj[!is.na(dj)])
plot(pa,d,ylim=c(m,M))
points(dj,col="red")
abline(v=c(20, 40, 60, 80),lty=3,col=grey(.7))
title("deltas Pos(med-cir) Médico2 Ideal vs jitter")

###################################################################
# Médico 2 SOC
d <- base1$posSOCMed1 - base1$posCir
d2s <- d[!is.na(d)]
mu <- 0
media <- mean(d2s)
s <- sd(d2s) 
n <- length(d2s)
estt <- (media-mu)/(s/sqrt(n))
et <- t.test(d,y=NULL,alternative="two.sided",mu=0)
p2s <- 2*pt(-abs(estt), df=n-1)
print(paste("el valor p Med1.SOC es", p2s))

dj <- jitter(base1$posSOCMed2, factor=ft, amount=NULL) - base1$posCir
dj2s <- dj[!is.na(dj)]
mu <- 0
mediaj <- mean(dj2s)
s <- sd(dj2s) 
n <- length(dj)
esttj <- (mediaj-mu)/(s/sqrt(n))
etj <- t.test(dj,y=NULL,alternative="two.sided",mu=0)
pj2s <- 2*pt(-abs(esttj), df=n-1)
print(paste("el valor p Med2.SOC.j es", pj2s))

M <- max(d[!is.na(d)],dj[!is.na(dj)])
m <- min(d[!is.na(d)],dj[!is.na(dj)])
plot(pa,d,ylim=c(m,M))
points(dj,col="red")
abline(v=c(20, 40, 60, 80),lty=3,col=grey(.7))
title("deltas Pos(med-cir) Médico2 SOC vs jitter")

###################################################################
###################################################################
# Médico 3 Ideal
d <- base1$posIdealMed3 - base1$posCir
d3i <- d[!is.na(d)]
mu <- 0
media <- mean(d3i)
s <- sd(d3i) 
n <- length(d3i)
estt <- (media-mu)/(s/sqrt(n))
et <- t.test(d,y=NULL,alternative="two.sided",mu=0)
p3i <- 2*pt(-abs(estt), df=n-1)
print(paste("el valor p Med3.Ideal es", p3i))

dj <- jitter(base1$posIdealMed3, factor=ft, amount=NULL) - base1$posCir
dj3i <- dj[!is.na(dj)]
mu <- 0
mediaj <- mean(dj3i)
s <- sd(dj3i) 
n <- length(dj3i)
esttj <- (mediaj-mu)/(s/sqrt(n))
etj <- t.test(dj,y=NULL,alternative="two.sided",mu=0)
pj3i <- 2*pt(-abs(esttj), df=n-1)
print(paste("el valor p Med3.Ideal.j es", pj3i))

M <- max(d[!is.na(d)],dj[!is.na(dj)])
m <- min(d[!is.na(d)],dj[!is.na(dj)])
plot(pa,d,ylim=c(m,M))
points(dj,col="red")
abline(v=c(20, 40, 60, 80),lty=3,col=grey(.7))
title("deltas Pos(med-cir) Médico3 Ideal vs jitter")

###################################################################
# Médico 3 SOC
d <- base1$posSOCMed3 - base1$posCir
d3s <- d[!is.na(d)]
mu <- 0
media <- mean(d3s)
s <- sd(d3s) 
n <- length(d3s)
estt <- (media-mu)/(s/sqrt(n))
et <- t.test(d,y=NULL,alternative="two.sided",mu=0)
p3s <- 2*pt(-abs(estt), df=n-1)
print(paste("el valor p Med3.SOC es", p3s))

dj <- jitter(base1$posSOCMed3, factor=ft, amount=NULL) - base1$posCir
dj3s <- dj[!is.na(dj)]
mu <- 0
mediaj <- mean(dj3s)
s <- sd(dj3s) 
n <- length(dj3s)
esttj <- (mediaj-mu)/(s/sqrt(n))
etj <- t.test(dj,y=NULL,alternative="two.sided",mu=0)
pj3s <- 2*pt(-abs(estt), df=n-1)
print(paste("el valor p Med3.SOC.j es", pj3s))

M <- max(d[!is.na(d)],dj[!is.na(dj)])
m <- min(d[!is.na(d)],dj[!is.na(dj)])
plot(pa,d,ylim=c(m,M))
points(dj,col="red")
abline(v=c(20, 40, 60, 80),lty=3,col=grey(.7))
title("deltas Pos(med-cir) Médico3 SOC vs jitter")

###################################################################
###################################################################
# Médico 4 Ideal
d <- base1$posIdealMed4 - base1$posCir
d4i <- d[!is.na(d)]
mu <- 0
media <- mean(d4i)
s <- sd(d4i) 
n <- length(d4i)
estt <- (media-mu)/(s/sqrt(n))
et <- t.test(d,y=NULL,alternative="two.sided",mu=0)
p4i <- 2*pt(-abs(estt), df=n-1)
print(paste("el valor p Med4.Ideal es", p4i))

dj <- jitter(base1$posIdealMed4, factor=ft, amount=NULL) - base1$posCir
dj4i <- dj[!is.na(dj)]
mu <- 0
mediaj <- mean(dj4i)
s <- sd(dj4i) 
n <- length(dj4i)
esttj <- (mediaj-mu)/(s/sqrt(n))
etj <- t.test(dj,y=NULL,alternative="two.sided",mu=0)
pj4i <- 2*pt(-abs(esttj), df=n-1)
print(paste("el valor p Med3.Ideal.j es", pj4i))

M <- max(d[!is.na(d)],dj[!is.na(dj)])
m <- min(d[!is.na(d)],dj[!is.na(dj)])
plot(pa,d,ylim=c(m,M))
points(dj,col="red")
abline(v=c(20, 40, 60, 80),lty=3,col=grey(.7))
title("deltas Pos(med-cir) Médico4 Ideal vs jitter")

###################################################################
# Médico 4 SOC
d <- base1$posSOCMed4 - base1$posCir
d4s <- d[!is.na(d)]
mu <- 0
media <- mean(d4s)
s <- sd(d4s) 
n <- length(d4s)
estt <- (media-mu)/(s/sqrt(n))
et <- t.test(d,y=NULL,alternative="two.sided",mu=0)
p4s <- 2*pt(-abs(estt), df=n-1)
print(paste("el valor p Med4.SOC es", p4s))

dj <- jitter(base1$posSOCMed4, factor=ft, amount=NULL) - base1$posCir
dj4s <- dj[!is.na(dj)]
mu <- 0
mediaj <- mean(dj4s)
s <- sd(dj4s) 
n <- length(dj4s)
esttj <- (mediaj-mu)/(s/sqrt(n))
etj <- t.test(dj,y=NULL,alternative="two.sided",mu=0)
pj4s <- 2*pt(-abs(esttj), df=n-1)
print(paste("el valor p Med4.SOC.j es", round(pj4s,5)))
M <- max(d[!is.na(d)],dj[!is.na(dj)])
m <- min(d[!is.na(d)],dj[!is.na(dj)])
plot(pa,d,ylim=c(m,M))
points(dj,col="red")
abline(v=c(20, 40, 60, 80),lty=3,col=grey(.7))
title("deltas Pos(med-cir) Médico4 sOC vs jitter")

###################################################################

par(opar)