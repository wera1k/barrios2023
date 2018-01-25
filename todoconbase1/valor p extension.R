# Médico 1 Ideal

d <- base1$extIdealMed1 - base1$extCir
d1i <- d[!is.na(d)]

mu <- 0
media <- mean(d1i)          ###Duda: df=86 o df=74?????????
s <- sd(d1i) 
n <- length(d)

estt <- (media-mu)/(s/sqrt(n))
et <- t.test(d,y=NULL,alternative="two.sided",mu=0)
p1i <- 2*pt(-abs(t), df=n-1)

print(paste("el valor p Med1.Ideal es", p1i))

# Si Ho fuera cierta, existe solo una probabilidad p de que suceda un evento extremo
# mientras más pequeño sea el valor p, con más razón se rechaza Ho

#######################################################################
# Médico 1 soc

d <- base1$extSOCMed1 - base1$extCir
d1s <- d[!is.na(d)]

mu <- 0
media <- mean(d1s)
s <- sd(d1s) 
n <- length(d)

t <- (media-mu)/(s/sqrt(n))

p1s <- 2*pt(-abs(t), df=n-1)

print(paste("el valor p Med1.SOC es", p1s))

#######################################################################
#######################################################################
# Médico 2 Ideal

d <- base1$extIdealMed2 - base1$extCir
d2i <- d[!is.na(d)]

mu <- 0
media <- mean(d2i)
s <- sd(d2i) 
n <- length(d)

t <- (media-mu)/(s/sqrt(n))

p2i <- 2*pt(-abs(t), df=n-1)

print(paste("el valor p Med2.Ideal es", p2i))

#######################################################################
# Médico 2 soc

d <- base1$extSOCMed2 - base1$extCir
d2s <- d[!is.na(d)]

mu <- 0
media <- mean(d2s)
s <- sd(d2s) 
n <- length(d)

t <- (media-mu)/(s/sqrt(n))

p2s <- 2*(pt(-abs(t), df=n-1))

print(paste("el valor p Med2.SOC es", p2s))

#######################################################################
#######################################################################
# Médico 3 Ideal

d <- base1$extIdealMed3 - base1$extCir
d3i <- d[!is.na(d)]

mu <- 0
media <- mean(d3i)
s <- sd(d3i) 
n <- length(d)

t <- (media-mu)/(s/sqrt(n))

p3i <- 2*pt(-abs(t), df=n-1)

print(paste("el valor p Med3.Ideal es", p3i))

#######################################################################
# Médico 3 soc

d <- base1$extSOCMed3 - base1$extCir
d3s <- d[!is.na(d)]

mu <- 0
media <- mean(d3s)
s <- sd(d3s) 
n <- length(d)

t <- (media-mu)/(s/sqrt(n))

p3s <- 2*pt(-abs(t), df=n-1)

print(paste("el valor p Med3.SOC es", p3s))

#######################################################################
#######################################################################
# Médico 4 Ideal

d <- base1$extIdealMed4 - base1$extCir
d4i <- d[!is.na(d)]

mu <- 0
media <- mean(d4i)
s <- sd(d4i) 
n <- length(d)

t <- (media-mu)/(s/sqrt(n))

p4i <- 2*pt(-abs(t), df=n-1)

print(paste("el valor p Med4.Ideal es", p4i))

#######################################################################
# Médico 4 soc

d <- base1$extSOCMed4 - base1$extCir
d4s <- d[!is.na(d)]

mu <- 0
media <- mean(d4s)
s <- sd(d4s) 
n <- length(d)

t <- (media-mu)/(s/sqrt(n))

p4s <- 2*pt(-abs(t), df=n-1)

print(paste("el valor p Med4.SOC es", p4s))


