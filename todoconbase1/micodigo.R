# plotPie.r - draw IRR plots with circular sectors / ribbons

opar <- par(no.readonly=TRUE)


# library(plotrix)
#windows(9.5,9.5, xpos=-1, ypos=0)

# want to plot "polar rectangle", {(r, theta) s.t. r1 < r < r2, th1 < th < th}
cart2pol <- function(x, y) {   #######cartesianas a polares
  r <- sqrt(x^2 + y^2)
  t <- atan(y/x)
  cbind(r,t)          ######Combine R Objects by Columns
}
pol2cart <- function(r, theta) {     #######polares a cartesianas
  x <- r * cos(theta)
  y <- r * sin(theta)
  cbind(x, y)
}

# circular sector is a rectangle in polar coordinates

# proof of concept  ################################# par(opar) no esta definido opar
#par(opar)
nPts <- 200
r <- rep(1, nPts)     ######rep replicates the values in x. It is a generic function
th <- seq(0, pi/6, length = nPts)
xy <- pol2cart(r, th)

######este plot lo puse yo
plot(0,0, type="n", axes=TRUE, frame=TRUE, xlim=c(-1,1), ylim=c(-1,1),
     ann=TRUE)

lines(xy[,1], xy[,2], type = "l", col="red", lwd = 2)  ########################

polarRect <- function(r1, r2, th1, th2, nPts = 200) {
  r <- rep(c(r1, r2, r1), c(nPts, nPts, 1))
  th <- c(seq(th1, th2, length = nPts), seq(th2, th1, length = nPts), th1)
  xy <- pol2cart(r, th)
}
# empty plot into which unit circle can be plotted
#pdf("plotPieTEST.pdf", h = 8, w = 8)
plot(1, type = "n", frame=TRUE, axes = FALSE, ann = FALSE, pty = "s", ##puse frame=TRUE
     xlim = c(-1,1), ylim = c(-1,1))
mycol2 <- c("red", "pink", "navy", "blue", "orange", "yellow", "green4", "green")
ttt <- tearSpan("1 to 4") ################ no entiendo ttt
# ttt as is 
aSector <- polarRect(0.05, 0.2, ttt[3], ttt[4], nPts = 200)
polygon(aSector[,1], aSector[,2],       
        col = mycol2[2], border = NA)        ############## ¿qué es mycol2?              
# ttt swapping rad1 and rad2 (no effect)
aSector <- polarRect(0.2, 0.4, ttt[4], ttt[3], nPts = 200)
polygon(aSector[,1], aSector[,2],
        col = mycol2[1], border = NA)
# ttt swap rad1 and rad2, make sure rad2 < rad1
aSector <- polarRect(0.4, 0.6, ttt[4]-2*pi, ttt[3], nPts = 200)
polygon(aSector[,1], aSector[,2],
        col = mycol2[3], border = NA)
# ttt as is but ensure rad2 < rad1
aSector <- polarRect(0.6, 0.7, ttt[3], ttt[4] - 2*pi, nPts = 200)
polygon(aSector[,1], aSector[,2],
        col = mycol2[4], border = NA)
#dev.off()

##### esto grafica una parte de dos anillos
aSector <- polarRect(0.4, 0.8, -pi/4, pi/3, nPts = 200)
polygon(aSector, col = rgb(1, 0, 0, 1.0), border = NA)
aSector <- polarRect(0.3, 0.9, -pi/2, pi/6, nPts = 200)
polygon(aSector, col = rgb(0, 0, 1, 0.8), border = NA)

#### grep busca la palabra "clock" en los nombres de dd
cc <- base1[, c(1:7, grep("reloj", names(base1)))]      ##### QUÉ DEMONIOS ES dd?? es la base?
cc$relojCir                                       #####     
hh2rad <- function(y) pi/2 * ((4 - (y - 3)/3) %% 4)    ### ¿qué hace %% ? MODULO
tearSpan <- function(x) {        ######## ¿qué son X y Y?
  # want: 3|6|9|12 to map to pi/2 * (0|3|2|1)       ### no me queda claro lo comentado
  # logic:  start     -3      /3      4-       %% 4
  #    get 3|6|9|12 0|3|6|9 0|1|2|3 4|3|2|1  0|3|2|1
  hh2rad <- function(y) pi/2 * ((4 - (y - 3)/3) %% 4)
  x <- as.character(x)    ##### si la x la hago cc$clock.sx ya hace algo pero en a me marca
  #####      Warning message: NAs introduced by coercion
  ##### ¿qué hago con los NA? ¿pongo is.na()?
  x <- gsub("(.*) and .*", "\\1", x)	# keep only first tear
  ab <- strsplit(x, " to ")      #### separa "1 to 2" a "1" y "2"
  a <- as.numeric(unlist(lapply(ab, "[", 1)))# %% 12  ### a debe tomar la primera columna
  b <- as.numeric(unlist(lapply(ab, "[", 2)))# %% 12  ### b debe tomar la segunda columna
  b[is.na(b)] <- a[is.na(b)]	# make pointwise tears (e.g. "2" -> "2 to 2")
  rad1 <- hh2rad(a)         #### esto convierte "a" a radianes
  rad2 <- hh2rad(b)         #### esto convierte "b" a radianes
  # ensure rad1 > rad2
  bad <- which(rad2 > rad1)
  if (length(bad) > 0) {
    rad2[bad] <- rad2[bad] - 2*pi
  }
  cbind(a, b, rad1, rad2)               
}
ttt <- tearSpan(cc$relojCir)              
head(pi/2 * (4 - (ttt[,1] - 3)/3)) 


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

#pdf("plotPie.pdf", h=8, w=8)
# n = 87 subjects, so let's pack into 10 x 10 grid
# circle centers
x0 <- c(rep(1:10, 8), 1:7)
y0 <- c(rep(10:3, each=10), rep(2, 7))

par(mar = 0*c(1,1,1,1), oma=0*c(1,1,1,1))
plot(1, type = "n", axes = FALSE, ann = FALSE, pty = "s",
     xlim = c(0,11), ylim = c(0,11))
# subset of interesting/problematic cases
#probl <- c(1:4, 7, 15, 21, 23:28)    ####### ¿para qué esto?
#for (i in probl) {
for (i in 1:nrow(base1)) {
  # radius span 0-0.55 for surgeon, so that it protrudes past readers
  if (!is.na(pCir[i])) {
    aSector <- polarRect(0, 0.55, pCir[i], pCir[i]+eCir[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = gray(0.9), border = NA)
  }
  # DB SOC Médico 1 SOC
  if (!is.na(pSOC1[i])) {
    aSector <- polarRect(0.45, 0.5, pSOC1[i], pSOC1[i]+eSOC1[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = mycol2[1], border = NA)
  }
  # DB IDL Médico 1 Ideal
  if (!is.na(pIdeal1[i])) {
    aSector <- polarRect(0.4, 0.45, pIdeal1[i], pIdeal1[i]+eIdeal1[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = mycol2[2], border = NA)
  }
  # KD SOC Médico 2 SOC
  if (!is.na(pSOC2[i])) {
    aSector <- polarRect(0.35, 0.4, pSOC2[i], pSOC2[i]+eSOC2[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = mycol2[3], border = NA)
  }
  # KD IDL Médico 2 Ideal
  if (!is.na(pIdeal2[i])) {
    aSector <- polarRect(0.3, 0.35, pIdeal2[i], pIdeal2[i]+eIdeal2[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = mycol2[4], border = NA)
  }   
  # RK SOC Médico 3 SOC
  if (!is.na(pSOC3[i])) {
    aSector <- polarRect(0.25, 0.3, pSOC3[i], pSOC3[i]+eSOC3[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = mycol2[5], border = NA)
  }
  # RK IDL Médico 3 Ideal
  if (!is.na(pIdeal3[i])) {
    aSector <- polarRect(0.2, 0.25, pIdeal3[i], pIdeal3[i]+eIdeal3[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = mycol2[6], border = NA)
  } 
  # JD SOC Médico 4 SOC
  if (!is.na(pSOC4[i])) {
    aSector <- polarRect(0.15, 0.2, pSOC4[i], pSOC4[i]+eSOC4[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = mycol2[7], border = NA)
  }
  # JD IDL Médico 4 Ideal         #Aquí pasa un error y grafica hasta el paciente 55
  if (!is.na(pIdeal4[i])) {
    aSector <- polarRect(0.1, 0.15, pIdeal4[i], pIdeal4[i]+eIdeal4[i], nPts = 200)
    polygon(x0[i] + aSector[,1], y0[i] + aSector[,2],
            col = mycol2[8], border = NA)
  }   
}
text(x0, y0, 1:nrow(base1), cex = 0.65)
legend("bottomright", lty = 1, lwd = 3, col = c(gray(0.9), mycol2[1:8]),
       c("Cirujano", paste(rep(c("Med1", "Med2", "Med3", "Med4"), each = 2),
                          rep(c("SOC", "IDL"), 4), sep = ".")),
       ncol = 3, bty = "n", cex = 0.65)
# text(x0[probl], y0[probl], (1:nrow(cc))[probl])
#dev.off()

cbind(probl, cc[probl,])
cbind(probl, TScir[probl,], TSsoc1[probl,], TSidea1[probl,]) 


par(opar)




