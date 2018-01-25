# https://www.youtube.com/watch?v=h3H3N0ziWlQ

data(airquality)
attach(airquality)
head(airquality)

Solar.R2 <- Solar.R[!is.na(Solar.R)]
alfa = .05
n <- length(Solar.R2)

media <- mean(Solar.R2)
S <- sd(Solar.R2)

mu <- 170

t0 <- (media-mu)/(S/sqrt(n))

talfa2 <- qt(p=alfa/2, df=n-1, lower.tail=F)
talfa2

abs(t0) > talfa2

talfa <- qt(p=alfa, df=n-1, lower.tail=F)
talfa

t0 > talfa




