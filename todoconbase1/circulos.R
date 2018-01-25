opar <- par(no.readonly=TRUE)



#cartesianas a polares
cart2pol <- function(x, y) {   
  r <- sqrt(x^2 + y^2)
  t <- atan(y/x)
  cbind(r,t)          
}

#polares a cartesianas
pol2cart <- function(r, theta) {  
  x <- r * cos(theta)
  y <- r * sin(theta)
  cbind(x, y)
}


nPts <- 200
r <- rep(1, nPts)     
th <- seq(0, pi/6, length = nPts)
xy <- pol2cart(r, th)


polarRect <- function(r1, r2, th1, th2, nPts = 200) {
  r <- rep(c(r1, r2, r1), c(nPts, nPts, 1))
  th <- c(seq(th1, th2, length = nPts), seq(th2, th1, length = nPts), th1)
  xy <- pol2cart(r, th)
}


colores <- c("red", "pink", "navy", "blue", "orange", "yellow", "green4", "green")

pCir <- base1$posCir 
eCir <- base1$extCir

pIdeal1 <- base1$posIdealMed1
eIdeal1 <- base1$extIdealMed1
pSOC1 <- base1$posSOCMed1
eSOC1 <- base1$extSOCMed1

pIdeal2 <- base1$posIdealMed2
eIdeal2 <- base1$extIdealMed2
pSOC2 <- base1$posSOCMed2
eSOC2 <- base1$extSOCMed2

pIdeal3 <- base1$posIdealMed3
eIdeal3 <- base1$extIdealMed3
pSOC3 <- base1$posSOCMed3
eSOC3 <- base1$extSOCMed3

pIdeal4 <- base1$posIdealMed4
eIdeal4 <- base1$extIdealMed4
pSOC4 <- base1$posSOCMed4
eSOC4 <- base1$extSOCMed4

x0 <- c(rep(1:10, 8), 1:7)
y0 <- c(rep(10:3, each=10), rep(2, 7))

par(mar = 0*c(1,1,1,1), oma=0*c(1,1,1,1))
plot(1, type = "n", axes = FALSE, ann = FALSE, pty = "s",
     xlim = c(0,11), ylim = c(0,11))

for (i in 1:nrow(base1)) {
  if (!is.na(pCir[i])) {
    aSector <- polarRect(0, 0.55, pCir[i], eCir[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = gray(0.8), border = NA)
  }
}

for (i in 1:nrow(base1)) {
  # DB SOC Médico 1 SOC
  if (!is.na(pSOC1[i])) {
    aSector <- polarRect(0.45, 0.5, pSOC1[i], eSOC1[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = colores[1], border = NA)
  }
}

for (i in 1:nrow(base1)) {
  # DB IDL Médico 1 Ideal
  if (!is.na(pIdeal1[i])) {
    aSector <- polarRect(0.4, 0.45, pIdeal1[i], eIdeal1[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = colores[2], border = NA)
  }
}

for (i in 1:nrow(base1)) {  
  # KD SOC Médico 2 SOC
  if (!is.na(pSOC2[i])) {
    aSector <- polarRect(0.35, 0.4, pSOC2[i], eSOC2[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = colores[3], border = NA)
  }
}

for (i in 1:nrow(base1)) {
  # KD IDL Médico 2 Ideal
  if (!is.na(pIdeal2[i])) {
    aSector <- polarRect(0.3, 0.35, pIdeal2[i], eIdeal2[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = colores[4], border = NA)
  }
}

for (i in 1:nrow(base1)) {
  # RK SOC Médico 3 SOC
  if (!is.na(pSOC3[i])) {
    aSector <- polarRect(0.25, 0.3, pSOC3[i], eSOC3[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = colores[5], border = NA)
  }
}

for (i in 1:nrow(base1)) {
  # RK IDL Médico 3 Ideal
  if (!is.na(pIdeal3[i])) {
    aSector <- polarRect(0.2, 0.25, pIdeal3[i], eIdeal3[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = colores[6], border = NA)
  } 
}

for (i in 1:nrow(base1)) {
  # JD SOC Médico 4 SOC
  if (!is.na(pSOC4[i])) {
    aSector <- polarRect(0.15, 0.2, pSOC4[i], eSOC4[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = colores[7], border = NA)
  }
}


for (i in 1:nrow(base1)) {
  # JD IDL Médico 4 Ideal         #Aquí pasa un error y grafica hasta el paciente 55
  if (!is.na(pIdeal4[i])) {
    aSector <- polarRect(0.1, 0.15, pIdeal4[i], eIdeal4[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = colores[8], border = NA)
  }   
}

text(x0, y0, 1:nrow(base1), cex = 0.65)
legend("bottomright", lty = 1, lwd = 3, col = c(gray(0.9), colores[1:8]),
       c("Cirujano", paste(rep(c("Médico1", "Médico2", "Médico3", "Médico4"), each = 2),
                           rep(c("SOC", "Ideal"), 4), sep = " ")),
       ncol = 2, bty = "n", cex = 0.65)




par(opar)



