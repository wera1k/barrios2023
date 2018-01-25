# its purpose is to teach statistics and R commands interactively 
install.packages("swirl")
library(swirl)
swirl()

# to use KNIT HTML button
install.packages('knitr', dependecies=T)
library(knitr)

# to run de qplot function,
install.packages('ggplot2', dependiecies=T)
library(ggplot2)


install.packages('ggplot2')
library(ggplot2)

# para juntar gráficas
install.packages('gridExtra')
library(gridExtra)

---------------------------------------------------------------------------------------------

#baes 1.1 es lo mismo que base 1 pero con nombres de columnas incluídas

base <- read.csv('base1.1.csv')
names(base)

qplot(x=edad, data=base)

# distribución EDADES
summary(base$edad)

gedades <- qplot(x=edad, data=base, 
      xlab='Edad',
      ylab='Número de pacientes',
      binwidth=1, color=I('black'), fill=I('pink')) + 
  scale_x_continuous(breaks=seq(0,60,1))

savePlots(file="distribucion_edades", width=4, height=3, 
          EPS=FALSE, PDF=TRUE, PNG=TRUE) 

summary(base$edad)

# edades por género

gedadesgenero <- qplot(x=edad, data=base, 
      xlab='Edades',                 
      ylab='Número de pacientes',
      binwidth=1, color=I('black'), fill=genero) + 
  scale_x_continuous(breaks=seq(0,60,5)) +
  facet_wrap(~genero)

savePlots(file="edades_genero_histogrma", width=4, height=3, 
          EPS=FALSE, PDF=TRUE, PNG=TRUE)

grid.arrange(gedades, gedadesgenero, ncol=1)


#distribución GENERO
by(base$edad, base$genero, summary)

genhist <- qplot(x=genero, data=base, color=I('black'), fill=I('pink')) 

ggplot(aes(x=genero), data=base, fill=c('pink','dodgerblue')) + 
  geom_histogram(stat="count", width=0.5)

genbox <- qplot(x=genero, y=edad, data=base,
                xlab='Género',
                ylab='Pacientes por Edad') + 
  geom_boxplot(aes(color=genero))

grid.arrange(genhist,genbox,ncol=1)

grid.arrange(gedades, gedadesgenero, genbox,ncol=1)


summary(base$genero)
x_genero <- c(61,26) #F=61, M=26
lab_genero <- c("61", "26")
col_genero <- c('pink','dodgerblue')
pie(x_genero, lab_genero, col=col_genero, clockwise=TRUE)
legend("right",c("Femino", "Masculino"), fill=c('pink','dodgerblue'))

# distibución del LADO en que se encuentra la herida
by(base$edad, base$genero, summary)

ladohist <- qplot(x=lado, data=base, color=I('black'), fill=I(c('pink','blue'))) 

ladobox <- qplot(x=lado, y=edad, data=base,
                xlab='Lado',
                ylab='Número de pacientes') + 
  geom_boxplot(aes(color=lado))

grid.arrange(ladohist,ladobox,ncol=1)


#savePlots(file="distribución edades", width=4, height=6, 
#          EPS=FALSE, PDF=TRUE, PNG=TRUE)



