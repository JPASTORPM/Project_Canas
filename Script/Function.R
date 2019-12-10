library("readxl")
library(Rmisc)
library(fields) #https://www.rdocumentation.org/packages/fields/versions/9.8-6/topics/interp.surface
library(plot3D) #https://rpubs.com/yoshio/95844

fun.plot3d<-function(data, tratamiento1, tratamiento2, Variable, fig.name){
  # Paso 1
  sum = summarySE(data, measurevar= Variable, groupvars=c("Tratmiento", "Punto"), na.rm=TRUE)
  sum<-sum[,c(1,2,3,4,6,7)]
  sum<-data.frame(Variable, sum)
  names(sum)<-c("Variable","Tratmiento","Punto","N","Mean","S.E.","C.I.95")
  sum
  sum1<-matrix(sum$Mean[sum$Tratmiento=="Control"],nrow = 3, ncol = 4)
  sum2<-matrix(sum$Mean[sum$Tratmiento=="Pennisetum"],nrow = 3, ncol = 4)
  # Paso 2A
  pdf(paste("Results/",fig.name,".pdf"), width=10, height=10)
  layout(matrix(c(1,1, 2,2, 3,3, 4,4,
                  1,1, 2,2, 3,3, 4,4, 
                  7,7, 5,5, 8,8, 6,6,
                  0,0, 0,0, 0,0, 0,0,
                  0,0, 0,0, 0,0, 0,0), nrow = 5, byrow=T))
  pm <- par("mfrow")
  
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(1.5,4,1.5,1))
  boxplot(ORP ~ Fila, data = dat[dat$Tratmiento=="Control",],  xlab=Variable, ylab= "Filas",horizontal=TRUE, col="gray45")
  
  par(xpd = TRUE, mgp = c(1.5,0.5,0), mar = c(1.5,0.5,2,2.5)) #contour = list(lwd = 2, col = jet.col(11))
  obj<- list( x= 1:3, y=1:4, z= sum1)
  set.seed(123)
  grid.list<- list( x= seq( 1,3,,100), y=  seq( 1,4,,100))
  m<-interp.surface.grid(obj, grid.list)
  image2D(z = m, lwd = 3, shade = 0.2, rasterImage = TRUE, contour=TRUE, main = tratamiento1, clab = sum$Variable[1], xlab="", ylab="")
  grid <- mesh(dat$Columna, dat$Fila)
  points(grid, pch=3, lwd=2, cex=1, col="White")
  
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(1.5,4,1.5,1))
  boxplot(ORP ~ Fila, data = dat[dat$Tratmiento=="Pennisetum",],  xlab=Variable, ylab= "Filas",horizontal=TRUE, col="gray45")
  
  par(xpd = TRUE, mgp = c(1.5,0.5,0), mar = c(1.5,0.5,2,2.5)) #contour = list(lwd = 2, col = jet.col(11))
  obj<- list( x= 1:3, y=1:4, z= sum2)
  set.seed(123)
  grid.list<- list( x= seq( 1,3,,100), y=  seq( 1,4,,100))
  m<-interp.surface.grid(obj, grid.list)
  image2D(z = m, lwd = 3, shade = 0.2, rasterImage = TRUE, contour=TRUE, main = expression(paste(italic("Pennisetum"), " sp.")), clab = sum$Variable[1], xlab="", ylab="")
  grid <- mesh(dat$Columna, dat$Fila)
  points(grid, pch=3, lwd=2, cex=1, col="White")
  
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(3,1,1,3))
  boxplot(ORP ~ Columna, data = dat[dat$Tratmiento=="Control",],  ylab=Variable, xlab= "Columnas", horizontal=FALSE, col="gray45")
  
  
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(3,1,1,3))
  boxplot(ORP ~ Columna, data = dat[dat$Tratmiento=="Pennisetum",],  ylab=Variable, xlab= "Columnas", horizontal=FALSE, col="gray45")
  
  #------------------------------------------------
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(0,0,0,0)) #contour = list(lwd = 2, col = jet.col(11))
  obj<- list( x= 1:3, y=1:4, z= sum1)
  set.seed(123)
  grid.list<- list( x= seq( 1,3,,100), y=  seq( 1,4,,100))
  m<-interp.surface.grid(obj, grid.list)
  x <- 1 : nrow(m$z)
  y <- 1 : ncol(m$z)
  panelfirst <- function(pmat) {
    XY <- trans3D(x = rep(1, ncol(m$z)), y = y,
                  z = m$z[50,], pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = m$z[50,],
              type = "l", lwd = 3, add = TRUE, colkey = FALSE)
    XY <- trans3D(x = x, y = rep(ncol(m$z), nrow(m$z)),
                  z = m$z[,50], pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = m$z[,50],
              type = "l", lwd = 3, add = TRUE, colkey = FALSE)
  }
  pmat <- persp3D(z = m$z, x = x, y = y, scale = FALSE, theta = 30,
                  expand = 0.1, panel.first = panelfirst, colkey = FALSE,contour=FALSE)
  XY <- trans3D(x = rep(50, ncol(m$z)), y = y, z = m$z[50,],
                pmat = pmat)
  lines(XY, lwd = 1, lty = 3)
  XY <- trans3D(x = x, y = rep(50, nrow(m$z)), z = m$z[,50],
                pmat = pmat)
  lines(XY, lwd = 1, lty = 3)
  
  obj<- list( x= 1:3, y=1:4, z= sum2)
  set.seed(123)
  grid.list<- list( x= seq( 1,3,,100), y=  seq( 1,4,,100))
  m<-interp.surface.grid(obj, grid.list)
  x <- 1 : nrow(m$z)
  y <- 1 : ncol(m$z)
  panelfirst <- function(pmat) {
    XY <- trans3D(x = rep(1, ncol(m$z)), y = y,
                  z = m$z[50,], pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = m$z[50,],
              type = "l", lwd = 3, add = TRUE, colkey = FALSE)
    XY <- trans3D(x = x, y = rep(ncol(m$z), nrow(m$z)),
                  z = m$z[,50], pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = m$z[,50],
              type = "l", lwd = 3, add = TRUE, colkey = FALSE)
  }
  pmat <- persp3D(z = m$z, x = x, y = y, scale = FALSE, theta = 30,
                  expand = 0.1, panel.first = panelfirst, colkey = FALSE,contour=FALSE)
  XY <- trans3D(x = rep(50, ncol(m$z)), y = y, z = m$z[50,],
                pmat = pmat)
  lines(XY, lwd = 1, lty = 3)
  XY <- trans3D(x = x, y = rep(50, nrow(m$z)), z = m$z[,50],
                pmat = pmat)
  lines(XY, lwd = 1, lty = 3)
  #------------------------------------------------
  dev.off()
  return(sum)
}


ORP<-fun.plot3d(data= dat, tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="ORP", fig.name="Fig. ORP")
pH<-fun.plot3d(data= dat, tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="pH", fig.name="Fig. pH")
OD<-fun.plot3d(data= dat, tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="OD", fig.name="Fig. OD")
Temperatura<-fun.plot3d(data= dat, tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="Temperatura", fig.name="Fig. Temperatura")
Conductividad<-fun.plot3d(data= dat, tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="Conductividad", fig.name="Fig. Conductividad")
## ======================================================================

pirateplot(formula = ORP ~ Columna, data = dat, 
           main = "", xlab = "", ylab = "ORP",
           ylim=c(-60,30),point.pch=NA, xaxt = NA,
           bar.f.col=c("grey"),avg.line.fun=mean,plot=TRUE,
           theme = 4, gl.col = NA,
           avg.line.col = "gray10",
           inf.f.col = "gray10",
           jitter.val = 0.09,
           avg.line.o = 1,bar.b.o=1,
           avg.line.lwd=3,
           inf.lwd=1,
           inf.method="se")

pirateplot(formula = ORP ~ Fila, data = dat, 
           main = "", xlab = "", ylab = "ORP",
           ylim=c(-120,60),point.pch=NA, xaxt = NA,
           bar.f.col=c("grey"),avg.line.fun=mean,plot=TRUE,
           theme = 4, gl.col = NA,
           avg.line.col = "gray10",
           inf.f.col = "gray10",
           jitter.val = 0.09,
           avg.line.o = 1,bar.b.o=1,
           avg.line.lwd=3,
           inf.lwd=1,
           inf.method="se", horizontal=TRUE)


boxplot(ORP ~ Fila, data = dat,  xlab=Variable, ylab= "Filas",horizontal=TRUE, col="gray45")
boxplot(ORP ~ Columna, data = dat,  ylab=Variable, xlab= "Columnas", horizontal=FALSE, col="gray45")


clim <- range(m$z)
persp3D(z = m$z, zlim = c(100, 600), clim = clim, box = FALSE, plot = FALSE)
persp3D(z = m$z + 200, clim = clim, colvar = m$z, add = TRUE, colkey = FALSE, plot = FALSE)
## 
persp3D(z = m$z + 400, clim = clim, colvar = m$z, add = TRUE, colkey = FALSE)    # plot = TRUE by default
