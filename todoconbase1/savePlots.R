# Function to save figures in graphic format files
# file:     file name
# width:    figure width in inches (6in default)
# height:   figure height in inches (6in default)
# EPS:      logical. Graphic should be save in postcript format
# PDF:      logical. Graphic should be save in pdf format
# PNG:      logical. Graphic should be save in png format
# JPEG:     logical. Graphic should be save in jpg format
#
# Example:    
# hist(rnorm(10000))
# savePlots("figure",6,4,TRUE,FALSE,TRUE,FALSE)
#
savePlots <- function(file,width=6,height=6,EPS=TRUE,PDF=TRUE,PNG=FALSE,JPEG=FALSE) 
{
    if(EPS) {
        nfile <- paste(file,"eps",sep=".")
        dev.copy(postscript,file=nfile,width=width,height=height,horizontal=FALSE,paper="special")
        dev.off(dev.cur())
    }
    if(PDF) {
        nfile <- paste(file,"pdf",sep=".")
        dev.copy(pdf,file=nfile,width=width,height=height)
        dev.off(dev.cur())
    }
    if(PNG) {
        nfile <- paste(file,"png",sep=".")
        dev.copy(png,file=nfile,width=100*width,height=100*height)
        dev.off(dev.cur())
    }
    if(JPEG) {
        nfile <- paste(file,"jpeg",sep=".")
        dev.copy(jpeg,file=nfile,width=100*width,height=100*height)
        dev.off(dev.cur())
    }
}
###savePlots <- function(file,width=6,height=6,EPS=TRUE,PDF=TRUE,PNG=FALSE,JPG=FALSE) 
###{
###    if(EPS) {
###        nfile <- paste(file,"eps",sep=".")
###        dev.copy(postscript,file=nfile,width=width,height=height,horizontal=FALSE,paper="special")
###        dev.off(dev.cur())
###    }
###    if(PDF) {
###        nfile <- paste(file,"pdf",sep=".")
###        dev.copy(pdf,file=nfile,width=width,height=height)
###        dev.off(dev.cur())
###    }
###    if(PNG) {
###        nfile <- paste(file,"png",sep=".")
###        dev.copy(png,file=nfile,width=100*width,height=100*height)
###        dev.off(dev.cur())
###    }
###    if(JPEG) {
###        nfile <- paste(file,"jpeg",sep=".")
###        dev.copy(jpeg,file=nfile,width=100*width,height=100*height)
###        dev.off(dev.cur())
###    }
###}
