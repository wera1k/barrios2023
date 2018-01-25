#Un neurólogo está probando el efecto de un fármaco sobre el tiempo de respuesta
#inyenctando 100 ratas con una dosis unitarioa, exponiéndolas individualmente 
#a estímulos neurológicos y registrando el tiempo de respuesta.
#El neurólogo sabe que el tiempo de respuesta para las ratas que NO han sido  
#inyectadas con el facmaco es de 1.2 segundos
#La media el tiempo de respuesta de las 100 ratas inyectadas es de 1.05 segundos
#con una desviación estándar de 0.5 segundos
#¿Piensas que el fármaco ha tenido algún efecto en el tiempo de respuesta?

#Estoy probando la eficacia de dos métodos de diagnóstico con los datos
#de 87 pacientes
#Espero que el diagnóstico no difiera de lo que dice el cirujano, es decir,
#espero que mu=0 
#La media de esta diferencia es de 0.3211406, con una desviación estándar desconocida
#¿es apropiado el método?

# TEORIA
#El valor p indica el grado en que la evidencia de la muestra apoya el rechazo de H0. 
#Por lo general, mientras más pequeño sea el valor p, mayor influencia tendrá la 
#evidencia de la muestra para rechazar H0.

#######################################################################
#######################################################################
# Médico 1 Ideal

d <- base1$posIdealMed1 - base1$posCir
d1i <- d[!is.na(d)]

mu <- 0
media <- mean(d1i)
s <- sd(d1i) 
n <- length(d)

t <- (media-mu)/(s/sqrt(n))

p1i <- 2*pt(-abs(t), df=n-1)

print(paste("el valor p Med1.Ideal es", p1i))

# Si Ho fuera cierta, existe solo una probabilidad p de que suceda un evento extremo
# mientras más pequeño sea el valor p, con más razón se rechaza Ho

#######################################################################
# Médico 1 soc

d <- base1$posSOCMed1 - base1$posCir
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

d <- base1$posIdealMed2 - base1$posCir
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

d <- base1$posSOCMed2 - base1$posCir
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

d <- base1$posIdealMed3 - base1$posCir
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

d <- base1$posSOCMed3 - base1$posCir
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

d <- base1$posIdealMed4 - base1$posCir
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

d <- base1$posSOCMed4 - base1$posCir
d4s <- d[!is.na(d)]

mu <- 0
media <- mean(d4s)
s <- sd(d4s) 
n <- length(d)

t <- (media-mu)/(s/sqrt(n))

p4s <- 2*pt(-abs(t), df=n-1)

print(paste("el valor p Med4.SOC es", p4s))


