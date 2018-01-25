# PRUEBAS DE HIPÓTESIS DE DOS COLAS   
# sigma desconocida
# Ho: delta=0 vs Ha: delta!=0
# Se debe rechazar si xbarra está muy alejado de muo de ciualquier lado
# esto lleva a rechazar Ho ya sea cuadno z>=c o z<=-c (c=alfa)
# Región de rechazo: mu!=mo, z>=zalfa/2 o z<=-zalfa/2

######################################################################
# Extensión Médico 1 IDEAL

extCir <- base1$extCir
extIdealMed1 <- base1$extIdealMed1

d <- extCir - extIdealMed1
d1 <- d[!is.na(d)]

alfa = .01
n <- length(d)

#media <- mean(d1)                                #DUDAS:
#S <- sd(d1)              # es correcto usar la primera o la segunda S ?
d1cuad <- d1^2            # para el cálculo se quitan los NA, ¿los df son 87 o 75(quitando NA)? 
sum(d1cuad)               # yo digo que 87
S <- sum(d1cuad)/(n-1)

delta <- 0

# estadístico de prueba t
t0 <- (media-delta)/(S/sqrt(n))

# valor de tablas se saca con qt
talfa2 <- qt(p=alfa/2, df=n-1, lower.tail=FALSE)
talfa2

#criterio de rechazo
resulIdeal1 <- abs(t0) > talfa2
print("Resultado Extensión Ideal Médico 1")
print(resulIdeal1)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
# Extensión Médico 1 SOC

extCir <- base1$extCir
extSOCMed1 <- base1$extSOCMed1

d <- extCir - extSOCMed1
d2 <- d[!is.na(d)]

alfa = .01
n <- length(d)

#media <- mean(d2)                               
#S <- sd(d2)              
d2cuad <- d2^2            
sum(d2cuad)               
S <- sum(d2cuad)/(n-1)

delta <- 0

# estadístico de prueba t
t0 <- (media-delta)/(S/sqrt(n))

# valor de tablas se saca con qt
talfa2 <- qt(p=alfa/2, df=n-1, lower.tail=FALSE)
talfa2

#criterio de rechazo
resulSOC1 <- abs(t0) > talfa2
print("Resultado Extensión SOC Médico 1")
print(resulSOC1)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
######################################################################
# Extensión Médico 2 IDEAL

extCir <- base1$extCir
extIdealMed2 <- base1$extIdealMed2

d <- extCir - extIdealMed2
d1 <- d[!is.na(d)]

alfa = .01
n <- length(d)

#media <- mean(d1)                                
#S <- sd(d1)              
d1cuad <- d1^2            
sum(d1cuad)               
S <- sum(d1cuad)/(n-1)

delta <- 0

# estadístico de prueba t
t0 <- (media-delta)/(S/sqrt(n))

# valor de tablas se saca con qt
talfa2 <- qt(p=alfa/2, df=n-1, lower.tail=FALSE)
talfa2

#criterio de rechazo
resulIdeal2 <- abs(t0) > talfa2
print("Resultado Extensión Ideal Médico 2")
print(resulIdeal2)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
# Extensión Médico 2 SOC

extCir <- base1$extCir
extSOCMed2 <- base1$extSOCMed2

d <- extCir - extSOCMed2
d2 <- d[!is.na(d)]

alfa = .01
n <- length(d)

#media <- mean(d2)                               
#S <- sd(d2)              
d2cuad <- d2^2            
sum(d2cuad)               
S <- sum(d2cuad)/(n-1)

delta <- 0

# estadístico de prueba t
t0 <- (media-delta)/(S/sqrt(n))

# valor de tablas se saca con qt
talfa2 <- qt(p=alfa/2, df=n-1, lower.tail=FALSE)
talfa2

#criterio de rechazo
resulSOC2 <- abs(t0) > talfa2
print("Resultado Extensión SOC Médico 2")
print(resulSOC2)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
######################################################################
# Extensión Médico 3 IDEAL

extCir <- base1$extCir
extIdealMed3 <- base1$extIdealMed3

d <- extCir - extIdealMed3
d1 <- d[!is.na(d)]

alfa = .01
n <- length(d)

#media <- mean(d1)                              
#S <- sd(d1)             
d1cuad <- d1^2             
sum(d1cuad)               
S <- sum(d1cuad)/(n-1)

delta <- 0

# estadístico de prueba t
t0 <- (media-delta)/(S/sqrt(n))

# valor de tablas se saca con qt
talfa2 <- qt(p=alfa/2, df=n-1, lower.tail=FALSE)
talfa2

#criterio de rechazo
resulIdeal3 <- abs(t0) > talfa2
print("Resultado Extensión Ideal Médico 3")
print(resulIdeal3)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
# Extensión Médico 3 SOC

extCir <- base1$extCir
extSOCMed3 <- base1$extSOCMed3

d <- extCir - extSOCMed3
d2 <- d[!is.na(d)]

alfa = .01
n <- length(d)

#media <- mean(d2)                               
#S <- sd(d2)              
d2cuad <- d2^2            
sum(d2cuad)               
S <- sum(d2cuad)/(n-1)

delta <- 0

# estadístico de prueba t
t0 <- (media-delta)/(S/sqrt(n))

# valor de tablas se saca con qt
talfa2 <- qt(p=alfa/2, df=n-1, lower.tail=FALSE)
talfa2

#criterio de rechazo
resulSOC3 <- abs(t0) > talfa2
print("Resultado Extensión SOC Médico 3")
print(resulSOC3)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
######################################################################
# Posición Médico 4 IDEAL

extCir <- base1$extCir
extIdealMed4 <- base1$extIdealMed4

d <- extCir - extIdealMed4
d1 <- d[!is.na(d)]

alfa = .01
n <- length(d)

#media <- mean(d1)                                
#S <- sd(d1)              
d1cuad <- d1^2            
sum(d1cuad)               
S <- sum(d1cuad)/(n-1)

delta <- 0

# estadístico de prueba t
t0 <- (media-delta)/(S/sqrt(n))

# valor de tablas se saca con qt
talfa2 <- qt(p=alfa/2, df=n-1, lower.tail=FALSE)
talfa2

#criterio de rechazo
resulIdeal4 <- abs(t0) > talfa2
print("Resultado Extensión Ideal Médico 4")
print(resulIdeal4)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
# Extensión Médico 4 SOC

extCir <- base1$extCir
extSOCMed4 <- base1$extSOCMed4

d <- extCir - extSOCMed4
d2 <- d[!is.na(d)]

alfa = .01
n <- length(d)

#media <- mean(d2)                               
#S <- sd(d2)              
d2cuad <- d2^2            
sum(d2cuad)               
S <- sum(d2cuad)/(n-1)

delta <- 0

# estadístico de prueba t
t0 <- (media-delta)/(S/sqrt(n))

# valor de tablas se saca con qt
talfa2 <- qt(p=alfa/2, df=n-1, lower.tail=FALSE)
talfa2

#criterio de rechazo
resulSOC4 <- abs(t0) > talfa2
print("Resultado Extensión SOC Médico 4")
print(resulSOC4)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho
