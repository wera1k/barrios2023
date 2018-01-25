# Teoría
# el valor p es uNA PROBABILIDAD
# Suponiendo que Ho es cierta, ¿cuál es la proba de que 


d <- base1$posIdealMed1 - base1$posCir
d0 <- d[!is.na(d)]
#sd(d0)
n <- length(d)


#two sided
mu=4
#no sabemos sigma
S=2 (#desv est de la muestra)
n=20
#sample avarage
zbar=6
t=(zbar-mu)/(S/sqrt(n))
t

#p value
2*pt(-abs(t), df=n-1)
