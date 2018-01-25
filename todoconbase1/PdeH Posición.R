# PRUEBAS DE HIPÓTESIS DE DOS COLAS   
# sigma desconocida
# Ho: delta=0 vs Ha: delta!=0
# Se debe rechazar si xbarra está muy alejado de muo de ciualquier lado
# esto lleva a rechazar Ho ya sea cuadno z>=c o z<=-c (c=alfa)
# Región de rechazo: mu!=mo, z>=zalfa/2 o z<=-zalfa/2

######################################################################
# Posición Médico 1 IDEAL

posCir <- base1$posCir
posIdealMed1 <- base1$posIdealMed1

d <- posIdealMed1 - posCir     ###### INVERTIR LA RESTA! MED - CIR
d1 <- d[!is.na(d)]

alfa = .01
n <- length(d)

media <- mean(d1)                                #DUDAS:
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
print(paste("Resultado Posición Ideal Médico 1: ", resulIdeal1))
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
# Posición Médico 1 SOC

posCir <- base1$posCir
posSOCMed1 <- base1$posSOCMed1

d <- posCir - posSOCMed1
d2 <- d[!is.na(d)]

alfa = .01
n <- length(d)

media <- mean(d2)                               
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
print("Resultado Posición SOC Médico 1")
print(resulSOC1)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
######################################################################
# Posición Médico 2 IDEAL

posCir <- base1$posCir
posIdealMed2 <- base1$posIdealMed2

d <- posCir - posIdealMed2
d1 <- d[!is.na(d)]

alfa = .01
n <- length(d)

media <- mean(d1)                                
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
print("Resultado Posición Ideal Médico 2")
print(resulIdeal2)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
# Posición Médico 2 SOC

posCir <- base1$posCir
posSOCMed2 <- base1$posSOCMed2

d <- posCir - posSOCMed2
d2 <- d[!is.na(d)]

alfa = .01
n <- length(d)

media <- mean(d2)                               
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
print("Resultado Posición SOC Médico 2")
print(resulSOC2)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
######################################################################
# Posición Médico 3 IDEAL

posCir <- base1$posCir
posIdealMed3 <- base1$posIdealMed3

d <- posCir - posIdealMed3
d1 <- d[!is.na(d)]

alfa = .01
n <- length(d)

media <- mean(d1)                              
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
print("Resultado Posición Ideal Médico 3")
print(resulIdeal3)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
# Posición Médico 3 SOC

posCir <- base1$posCir
posSOCMed3 <- base1$posSOCMed3

d <- posCir - posSOCMed3
d2 <- d[!is.na(d)]

alfa = .01
n <- length(d)

media <- mean(d2)                               
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
print("Resultado Posición SOC Médico 3")
print(resulSOC3)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
######################################################################
# Posición Médico 4 IDEAL

posCir <- base1$posCir
posIdealMed4 <- base1$posIdealMed4

d <- posCir - posIdealMed4
d1 <- d[!is.na(d)]

alfa = .01
n <- length(d)

media <- mean(d1)                                
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
print("Resultado Posición Ideal Médico 4")
print(resulIdeal4)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho

######################################################################
# Posición Médico 4 SOC

posCir <- base1$posCir
posSOCMed4 <- base1$posSOCMed4

d <- posCir - posSOCMed4
d2 <- d[!is.na(d)]

alfa = .01
n <- length(d)

media <- mean(d2)                               
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
print("Resultado Posición SOC Médico 4")
print(resulSOC4)
# si TRUE, se rechaza Ho y se acepta Ha
# si FALSE, no se rechaza Ho


