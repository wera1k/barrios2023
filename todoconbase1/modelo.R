set.seed(17092016)

ft=10

dj <- jitter(base1$posIdealMed1, factor=ft, amount=NULL) - base1$posCir

med <- factor(rep(c(1,2,3,4),each=2*87))
met <- factor(rep(rep(c(1,2),each=87),times=4))  # 1=Ideal  2=SOC

# k=pacientes (1,...,87)
d11k <- base1$posIdealMed1 - base1$posCir
d12k <- base1$posSOCMed1 - base1$posCir
d21k <- base1$posIdealMed2 - base1$posCir
d22k <- base1$posSOCMed2 - base1$posCir
d31k <- base1$posIdealMed3 - base1$posCir
d32k <- base1$posSOCMed4 - base1$posCir
d41k <- base1$posIdealMed4 - base1$posCir
d42k <- base1$posSOCMed4 - base1$posCir

dj11k <- jitter(base1$posIdealMed1, factor=ft, amount=NULL) - base1$posCir
dj12k <- jitter(base1$posSOCMed1, factor=ft, amount=NULL) - base1$posCir
dj21k <- jitter(base1$posIdealMed2, factor=ft, amount=NULL) - base1$posCir
dj22k <- jitter(base1$posSOCMed2, factor=ft, amount=NULL) - base1$posCir
dj31k <- jitter(base1$posIdealMed3, factor=ft, amount=NULL) - base1$posCir
dj32k <- jitter(base1$posSOCMed4, factor=ft, amount=NULL) - base1$posCir
dj41k <- jitter(base1$posIdealMed4, factor=ft, amount=NULL) - base1$posCir
dj42k <- jitter(base1$posSOCMed4, factor=ft, amount=NULL) - base1$posCir

delta <- c(d11k,d12k,d21k,d22k,d31k,d32k,d41k,d42k)
deltaj <- c(dj11k,dj12k,dj21k,dj22k,dj31k,dj32k,dj41k,dj42k)

mod1 <- lm(delta~med)
mod2 <- lm(delta~met)
mod3 <- lm(delta~med+met)
mod4 <- lm(delta~med+met+med:met,data=base1)

modj1 <- lm(deltaj~med)
modj2 <- lm(deltaj~met)
modj3 <- lm(deltaj~med+met)
modj4 <- lm(deltaj~med+met+med:met,data=base1)

#print(summary(mod1))
#print(summary(modj1))
print(summary(mod2))
print(summary(modj2))
#print(summary(mod3))
#print(summary(modj3))
#print(summary(mod4))
#print(summary(modj4))