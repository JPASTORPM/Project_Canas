#------------------------------------------------
# Script: Cañas
# Autor: Junior Pastor Pérez-Molina
# Date: 12-10-2019
#------------------------------------------------



#------------------------------------------------
# Initial steps
#------------------------------------------------
rm(list = ls()) # Remove all objects
graphics.off()  # Remove all graphics
cat("\014")     # Remove  console scripts
#------------------------------------------------



#------------------------------------------------
# Packages
#------------------------------------------------
library("readxl")
library(openxlsx)
library(Rmisc)
library(fields) #https://www.rdocumentation.org/packages/fields/versions/9.8-6/topics/interp.surface
library(plot3D) #https://rpubs.com/yoshio/95844
library(yarrr)
library(broom)
library(car)
library(lsmeans)
library(multcompView)
library(multcomp)
library(dplyr)
library(GGally)
library(factoextra)
library(cowplot)
library(ggplot2)
library(grid)
library(gridExtra)
#------------------------------------------------



#------------------------------------------------
# Loading - Database
#------------------------------------------------
dat<-read_excel("Data/Data.xlsx", sheet = "Data")
dat<-data.frame(dat)
dat$Rep<-format(dat$Rep,"%H:%M:%S")
dat$Fecha<-as.Date(dat$Fecha)
pe<-dat[dat$Tratamiento=="Entrada", ]
ps_p<-dat[dat$Tratamiento=="Salida Pennisetum", ]
ps_c<-dat[dat$Tratamiento=="Salida Control", ]
str(dat)
#------------------------------------------------



#------------------------------------------------
# Cuadro 1 - Comparaci?n entrada y salida de los sistemas (Control y Pennisetum)
#------------------------------------------------
g<-merge(pe, ps_c, all=TRUE)
g<-merge(g, ps_p, all=TRUE)
g$Tratamiento_Fecha<-paste(g$Tratamiento, g$Fecha, sep="*")
g$Tratamiento_Fecha_Rep<-paste(g$Tratamiento_Fecha, g$Rep, sep="*")
g$Fecha<-as.factor(g$Fecha)
g$Rep<-as.factor(g$Rep)
g$Tratamiento<-as.factor(g$Tratamiento)
str(g)
#------------------------------------------------
fun.table<-function(model1, model2,Variable){
  shapiro.test(model1$residuals)
  inf<-round(glance(model1),3)
  P.value<-round(as.numeric(inf[5]),3)
  P.value<-ifelse(P.value<0.001, "***",
         ifelse(P.value<0.01, "**",
                ifelse(P.value<0.05, "*","n.s.")))
  
  R2<-round(as.numeric(inf[2]),2)
  F<-round(as.numeric(inf[4]),1)
  coef<-Anova(model1, type="II")
  P<-data.frame(round(coef$`Pr(>F)`,3))
  P<-data.frame(t(P))
  
  sum = summarySE(g, measurevar= Variable, groupvars=c("Tratamiento"), na.rm=TRUE)
  sum<-sum[c(1,2,3,5,6)]
  sum<-data.frame(sum)
  sum<-data.frame(Sistema= sum$Tratamiento, mean=paste(round(sum[,3], 1), round(sum[,4], 2), sep=" ? "))
  sum<-data.frame(t(sum))
  names(sum)<-c("Entrada", "Salida Control","Salida Pennisetum")
  sum<-sum[-1,]
  
  if(P[,3]<0.05){
    lsm = lsmeans(model2, pairwise ~ Tratamiento, adjust="LSD")
    t1<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
    t2<-data.frame(t1[c(1)],t1[c(7)])
    t2<-t2[order(t2$Tratamiento),]
    rm.whitespace <- function (x) gsub("^\\s+|\\s+$", "", x)
    t2$.group<-rm.whitespace(t2$.group)
  } else {
    t2<-data.frame(Tratamiento=c("","",""),".group"=c("","",""))
  }
  
  p<-data.frame(Variable=Variable,Fecha=P[,1],Hora=P[,2],Sistema=P[,3],
                Entrada=paste(sum[1,1], t2[1,2], sep=" "), 
                "Salida Control"=paste(sum[1,2], t2[2,2], sep=" "),
                "Salida Pennisetum"=paste(sum[1,3], t2[3,2], sep=" "),
                R2, F, P.value)
  s<-ifelse(p[,c(2,3,4)]<0.001, "***",
            ifelse(p[,c(2,3,4)]<0.01, "**",
                   ifelse(p[,c(2,3,4)]<0.05, "*","n.s.")))
  p[,2]<-s[,1]
  p[,3]<-s[,2]
  p[,4]<-s[,3]
  return(p)
}
#------------------------------------------------
OD_es<-aov(OD ~  Fecha + Rep + Tratamiento, data=g)
summary(OD_es)
shapiro.test(OD_es$residuals)
leveneTest(g$OD, g$Tratamiento)
OD_es2<-aov(OD ~  Tratamiento, data=g)
summary(OD_es2)
OD_t<-fun.table(model1=OD_es, model2= OD_es2,Variable="OD")
#------------------------------------------------
pH_es<-aov(pH ~  Fecha + Rep + Tratamiento, data=g)
summary(pH_es)
shapiro.test(pH_es$residuals)
leveneTest(g$pH, g$Tratamiento)
pH_es2<-aov(pH ~  Tratamiento, data=g)
summary(pH_es2)
pH_t<-fun.table(model1=pH_es, model2= pH_es2,Variable="pH")
#------------------------------------------------
ORP_es<-aov(ORP ~  Fecha + Rep + Tratamiento, data=g)
summary(ORP_es)
shapiro.test(ORP_es$residuals)
leveneTest(g$ORP, g$Tratamiento)
ORP_es2<-aov(ORP ~  Tratamiento, data=g)
summary(ORP_es2)
ORP_t<-fun.table(model1=ORP_es, model2= ORP_es2,Variable="ORP")
#------------------------------------------------
Temperatura_es<-aov(Temperatura ~  Fecha + Rep + Tratamiento, data=g)
summary(Temperatura_es)
shapiro.test(Temperatura_es$residuals)
leveneTest(g$Temperatura, g$Tratamiento)
Temperatura_es2<-aov(Temperatura ~  Tratamiento, data=g)
summary(Temperatura_es2)
Temperatura_t<-fun.table(model1=Temperatura_es, model2= Temperatura_es2,Variable="Temperatura")
#------------------------------------------------
Conductividad_es<-aov(Conductividad ~  Fecha + Rep + Tratamiento, data=g)
summary(Conductividad_es)
shapiro.test(Conductividad_es$residuals)
leveneTest(g$Conductividad, g$Tratamiento)
Conductividad_es2<-aov(Conductividad ~  Tratamiento, data=g)
summary(Conductividad_es2)
Conductividad_t<-fun.table(model1=Conductividad_es, model2= Conductividad_es2,Variable="Conductividad")
#------------------------------------------------
Cuadro_1<-merge(ORP_t, Conductividad_t, all = TRUE)
Cuadro_1<-merge(Cuadro_1, OD_t, all = TRUE)
Cuadro_1<-merge(Cuadro_1, Temperatura_t, all = TRUE)
Cuadro_1<-merge(Cuadro_1, pH_t, all = TRUE)
Cuadro_1
#------------------------------------------------
write.xlsx(Cuadro_1, "Results/Cuadro 1. ANDEVA tres v?as.xlsx",
           sheetName="Cuadro 1",col.names=TRUE,
           row.names=FALSE, append=FALSE,
           showNA=TRUE, password=NULL)
#------------------------------------------------



#------------------------------------------------
# Cuadro 2 - Comparaci?n entre las filas y columnas dentro de cada sistema
#------------------------------------------------
dat$Columna<-dat$Punto
dat$Fila<-dat$Punto

dat$Columna[dat$Punto==1]<-2.70
dat$Columna[dat$Punto==2]<-5.95
dat$Columna[dat$Punto==3]<-9.2

dat$Columna[dat$Punto==4]<-2.70
dat$Columna[dat$Punto==5]<-5.95
dat$Columna[dat$Punto==6]<-9.2

dat$Columna[dat$Punto==7]<-2.70
dat$Columna[dat$Punto==8]<-5.95
dat$Columna[dat$Punto==9]<-9.2

dat$Columna[dat$Punto==10]<-2.70
dat$Columna[dat$Punto==11]<-5.95
dat$Columna[dat$Punto==12]<-9.2

dat$Fila[dat$Punto==1]<-2.80
dat$Fila[dat$Punto==2]<-2.80
dat$Fila[dat$Punto==3]<-2.80

dat$Fila[dat$Punto==4]<-7.15
dat$Fila[dat$Punto==5]<-7.15
dat$Fila[dat$Punto==6]<-7.15

dat$Fila[dat$Punto==7]<-11.5
dat$Fila[dat$Punto==8]<-11.5
dat$Fila[dat$Punto==9]<-11.5

dat$Fila[dat$Punto==10]<-15.85
dat$Fila[dat$Punto==11]<-15.85
dat$Fila[dat$Punto==12]<-15.85
#------------------------------------------------
e<-as.numeric(c(row.names(dat[dat$Tratamiento=="Entrada",]),
           row.names(dat[dat$Tratamiento=="Salida Control",]),
           row.names(dat[dat$Tratamiento=="Salida Pennisetum",])))
dat2<-dat[-c(e),]
#------------------------------------------------
ORP_aov<-aov( ORP ~ Columna + Fila, data=dat2)
summary.lm(ORP_aov)
shapiro.test(ORP_aov$residuals)
bartlett.test(dat2$ORP, dat2$Fila)
#------------------------------------------------
Conductividad_aov<-aov( Conductividad ~ Columna + Fila, data=dat2)
summary.lm(Conductividad_aov)
shapiro.test(Conductividad_aov$residuals)
bartlett.test(dat2$Conductividad, dat2$Fila)
#------------------------------------------------
OD_aov<-aov( OD ~ Columna + Fila, data=dat2)
summary.lm(OD_aov)
shapiro.test(OD_aov$residuals)
bartlett.test(dat2$OD, dat2$Fila)
#------------------------------------------------
Temperatura_aov<-aov( Temperatura ~ Columna + Fila, data=dat2)
summary.lm(Temperatura_aov)
shapiro.test(Temperatura_aov$residuals)
bartlett.test(dat2$Temperatura, dat2$Fila)
#------------------------------------------------
pH_aov<-aov( pH ~ Fila, data=dat2)
summary.lm(pH_aov)
shapiro.test(pH_aov$residuals)
bartlett.test(dat2$pH, dat2$Fila)
#------------------------------------------------



#------------------------------------------------
# Figure - Analisis espacial
#------------------------------------------------
fun.plot3d<-function(data, var1, var2, tratamiento1, tratamiento2, Variable, fig.name){
  # Paso 1
  sum = summarySE(data, measurevar= Variable, groupvars=c("Tratamiento", "Punto"), na.rm=TRUE)
  sum<-sum[,c(1,2,3,4,6,7)]
  sum<-data.frame(Variable, sum)
  names(sum)<-c("Variable","Tratamiento","Punto","N","Mean","S.E.","C.I.95")
  sum
  sum1<-matrix(sum$Mean[sum$Tratamiento=="Control"],nrow = 3, ncol = 4)
  sum2<-matrix(sum$Mean[sum$Tratamiento=="Pennisetum"],nrow = 3, ncol = 4)
  # Paso 2A
  pdf(paste("Results/",fig.name,".pdf"), width=10, height=10)
  layout(matrix(c(1,1, 2,2, 3,3, 4,4,
                  1,1, 2,2, 3,3, 4,4, 
                  7,7, 5,5, 8,8, 6,6,
                  0,0, 0,0, 0,0, 0,0,
                  0,0, 0,0, 0,0, 0,0), nrow = 5, byrow=T))
  pm <- par("mfrow")
  
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(1.5,4,1.5,1))
  boxplot(var1 ~ dat$Fila[dat$Tratamiento=="Control"], xlab=Variable, ylab= "Row: Distance (m)",horizontal=TRUE, col="gray45")
  
  #-------------------
  x=c(2.70, 5.95, 9.2)
  y=c(2.8, 7.15, 11.5, 15.85)
  #-------------------
  
  par(xpd = TRUE, mgp = c(1.5,0.5,0), mar = c(1.5,0.5,2,2.5)) #contour = list(lwd = 2, col = jet.col(11))
  obj<- list( x= x, y=y, z= sum1)
  set.seed(123)
  grid.list<- list( x= seq( min(x),max(x),,100), y=  seq( min(y),max(y),,100))
  m<-interp.surface.grid(obj, grid.list)
  image2D(z = m, lwd = 3, shade = 0.2, rasterImage = TRUE, contour=TRUE, main = tratamiento1, clab = sum$Variable[1], xlab="", ylab="")
  grid <- mesh(dat$Columna, dat$Fila)
  points(grid, pch=3, lwd=2, cex=1, col="White")
  
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(1.5,4,1.5,1))
  boxplot(var2 ~ dat$Fila[dat$Tratamiento=="Pennisetum"],  xlab=Variable, ylab= "Row: Distance (m)",horizontal=TRUE, col="gray45")
  
  par(xpd = TRUE, mgp = c(1.5,0.5,0), mar = c(1.5,0.5,2,2.5)) #contour = list(lwd = 2, col = jet.col(11))
  obj<- list( x= x, y=y, z= sum2)
  set.seed(123)
  grid.list<- list( x= seq( min(x),max(x),,100), y=  seq( min(y),max(y),,100))
  m<-interp.surface.grid(obj, grid.list)
  image2D(z = m, lwd = 3, shade = 0.2, rasterImage = TRUE, contour=TRUE, main = "Pennisetum", clab = sum$Variable[1], xlab="", ylab="")
  grid <- mesh(dat$Columna, dat$Fila)
  points(grid, pch=3, lwd=2, cex=1, col="White")
  
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(3,1,1,3))
  boxplot(var1 ~ dat$Columna[dat$Tratamiento=="Control"],  ylab=Variable, xlab= "Column: Distance (m)", horizontal=FALSE, col="gray45")
  
  
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(3,1,1,3))
  boxplot(var2 ~ dat$Columna[dat$Tratamiento=="Pennisetum"],  ylab=Variable, xlab= "Column: Distance (m)", horizontal=FALSE, col="gray45")
  
  #------------------------------------------------
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(0,0,0,0)) #contour = list(lwd = 2, col = jet.col(11))
  obj<- list( x= x, y=y, z= sum1)
  set.seed(123)
  grid.list<- list( x= seq( min(x),max(x),,100), y=  seq( min(y),max(y),,100))
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
  
  #-------------------
  x=c(2.70, 5.95, 9.2)
  y=c(2.8, 7.15, 11.5, 15.85)
  #-------------------
  
  obj<- list( x= x, y=y, z= sum2)
  set.seed(123)
  grid.list<- list( x= seq( min(x),max(x),,100), y=  seq( min(y),max(y),,100))
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
#------------------------------------------------
ORP<-fun.plot3d(data= dat, var1=dat$ORP[dat$Tratamiento=='Control'],var2=dat$ORP[dat$Tratamiento=='Pennisetum'],tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="ORP", fig.name="Fig. ORP")
pH<-fun.plot3d(data= dat, var1=dat$pH[dat$Tratamiento=='Control'],var2=dat$pH[dat$Tratamiento=='Pennisetum'],tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="pH", fig.name="Fig. pH")
OD<-fun.plot3d(data= dat, var1=dat$OD[dat$Tratamiento=='Control'],var2=dat$OD[dat$Tratamiento=='Pennisetum'],tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="OD", fig.name="Fig. OD")
Temperatura<-fun.plot3d(data= dat, var1=dat$Temperatura[dat$Tratamiento=='Control'],var2=dat$Temperatura[dat$Tratamiento=='Pennisetum'],tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="Temperatura", fig.name="Fig. Temperatura")
Conductividad<-fun.plot3d(data= dat, var1=dat$Conductividad[dat$Tratamiento=='Control'],var2=dat$Conductividad[dat$Tratamiento=='Pennisetum'],tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="Conductividad", fig.name="Fig. Conductividad")
#------------------------------------------------


 
#------------------------------------------------
# Fig. Spearman - Correlations.
#------------------------------------------------
shapiro.test(dat2$pH)
shapiro.test(dat2$OD)
shapiro.test(dat2$Temperatura)
shapiro.test(dat2$ORP)
shapiro.test(dat2$Conductividad)

shapiro.test(dat2$pH[dat2$Tratamiento=='Control'])
shapiro.test(dat2$OD[dat2$Tratamiento=='Control'])
shapiro.test(dat2$Temperatura[dat2$Tratamiento=='Control'])
shapiro.test(dat2$ORP[dat2$Tratamiento=='Control'])
shapiro.test(dat2$Conductividad[dat2$Tratamiento=='Control'])

shapiro.test(dat2$pH[dat2$Tratamiento=='Pennisetum'])
shapiro.test(dat2$OD[dat2$Tratamiento=='Pennisetum'])
shapiro.test(dat2$Temperatura[dat2$Tratamiento=='Pennisetum'])
shapiro.test(dat2$ORP[dat2$Tratamiento=='Pennisetum'])
shapiro.test(dat2$Conductividad[dat2$Tratamiento=='Pennisetum'])

d <- dat2[, c(3, 5:9)]
str(d)
d<-na.omit(d)
names(d)<-c("Tratamiento","pH","OD","Temp","ORP","Cond")
d<-d[order(d$Tratamiento),]
d2<-d[,1]
d<-d[,-1]

hc <- hclust(as.dist(1-cor(d, method='spearman', use='pairwise.complete.obs')))
hc$height
hc.order <- order.dendrogram(as.dendrogram(hc))
#d <- d[ ,hc]
d[ ,hc.order]
gr <- as.factor(d2)

cols.key <- scales::muted(c('black', 'black'))
cols.key <- adjustcolor(cols.key, alpha.f=1)
pchs.key <- c(19,17)

panel.hist <- function(x, ...) {
  usr <- par('usr'); on.exit(par(usr))
  par(usr=c(usr[1:2], 0, 1.5))
  h <- hist(x, plot=FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col='gray', ...)
}
panel.cor <- function(x, y, ...){
  usr <- par('usr'); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r <- cor(x, y, method='spearman', use='pairwise.complete.obs')
  zcol <- lattice::level.colors(r, at=seq(-1, 1, length=81), col.regions=colorRampPalette(c(scales::muted('red'),'white',scales::muted('blue')), space='rgb')(81))
  ell <- ellipse::ellipse(r, level=0.95, type='l', npoints=50, scale=c(.2, .2), centre=c(.5, .5))
  polygon(ell, col=zcol, border=zcol, ...)
  text(x=.5, y=.5, lab=100*round(r, 2), cex=2, col='black')
  pval <- cor.test(x, y, method='spearman', use='pairwise.complete.obs')$p.value
  sig <- ifelse(pval<0.001,"***",ifelse(pval<0.01,"**", ifelse(pval<0.05,"*","n.s.")))#symnum(pval, corr=FALSE, na=FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c('***', '**', '*', '.', ' '))
  text(.6, .8, sig, cex=2, col='gray20')
}
panel.scatter <- function(x, y){
  cols<-ifelse(as.numeric(gr)==1,'gray25','gray50')
  points(x, y, col=cols, pch=pchs.key[gr], cex=1.15)
  lines(lowess(x, y))
}
#------------------------------------------------
pdf(file = "Results/Fig. Spearman correlations for all treatment.pdf", width = 4.5*1.5, height = 4.5*1.5) # Este elimina todos los NA de la matrix de datos, por tanto, incluye menos datos
pairs(d, diag.panel=panel.hist,lower.panel=panel.scatter,
      upper.panel=panel.cor,gap=0.5,labels=gsub('\\.', '\n', colnames(d)),
      label.pos=0.7,cex.labels=1.4,oma=c(3,3,3,3))
dev.off()
#------------------------------------------------
pdf(file = "Results/Fig. Spearman correlations by treatment.pdf", width = 4.5*2, height = 4.5*1.75) # Este considera todos los valores, no excluye valores NA de la matrix de datos, por lo tanto, m?s datos
dat3<-dat2[,c(3, 5:9)]
dat3$Grupo<-dat3$Tratamiento
dat3$Grupo[dat3$Tratamiento=="Control"]<-"C"
dat3$Grupo[dat3$Tratamiento=="Pennisetum"]<-"P"
dat3 %>% ggpairs(.,legend = 1,columns = 2:6,mapping = ggplot2::aes(colour=Grupo),upper = list(continuous = wrap('cor', method = "spearman")),
  lower = list(continuous = wrap("smooth", alpha = 0.5, size=2, pch=c(19)))) +
  theme(legend.position = "bottom") +
  theme_bw()
dev.off()
#------------------------------------------------



#------------------------------------------------
# Fig. PCA plot.
#------------------------------------------------
df.PCA<-dat2
df.PCA<-df.PCA[,c(-1,-2,-4,-10)]
head(df.PCA)
names(df.PCA)<-c("Tratamiento","pH", "OD", "Temperature", "ORP", "Conductivity", "Fila")
df.PCA<-na.omit(df.PCA)
df.PCA = df.PCA[with(df.PCA, order(-ORP)), ] # df.PCA$Fila==15.85
#------------------------------------------------
res.pca1 <- prcomp(df.PCA[, c(-1,-7)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[, c(1)]) # Only treatment

a1<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
a1<- a1 + theme_minimal()
a1<- a1 +  scale_shape_manual(values = c(1,19))
#------------------------------------------------
pdf(file = "Results/Fig. PCA total database.pdf", width = 4.5*1.5, height = 3.75*1.5)
plot_grid(a1,labels=c("Whole-System"),ncol = 1, nrow = 1)
dev.off()
#------------------------------------------------
res.pca1 <- prcomp(df.PCA[df.PCA$Fila==2.80, c(-1,-7)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$Fila==2.80, c(1)]) # Only treatment
af1<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
af1<- af1 + theme_minimal()
af1<- af1 +  scale_shape_manual(values = c(1,19))

res.pca1 <- prcomp(df.PCA[df.PCA$Fila==7.15, c(-1,-7)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$Fila==7.15, c(1)]) # Only treatment
af2<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
af2<- af2 + theme_minimal()
af2<- af2 +  scale_shape_manual(values = c(1,19))

res.pca1 <- prcomp(df.PCA[df.PCA$Fila==11.50, c(-1,-7)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$Fila==11.50, c(1)]) # Only treatment
af3<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
af3<- af3 + theme_minimal()
af3<- af3 +  scale_shape_manual(values = c(1,19))

res.pca1 <- prcomp(df.PCA[df.PCA$Fila==15.85, c(-1,-7)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$Fila==15.85, c(1)]) # Only treatment
af4<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
af4<- af4 + theme_minimal()
af4<- af4 +  scale_shape_manual(values = c(1,19))
#------------------------------------------------
pdf(file = "Results/Fig. PCA by fila.pdf", width = 4.5*2, height = 3.75*2)
plot_grid(af1, af2, af3, af4, labels=c("Row 1: 2.8 m", "Row 2: 7.15 m", "Row 3: 11.5 m", "Row 4: 15.85 m"),ncol = 2, nrow = 2)
dev.off()
#------------------------------------------------
pdf(file = "Results/Fig. PCA completo.pdf", width = 4.5*3.5, height = 3.75*1.75)
grid.arrange(plot_grid(a1,labels=c("Whole-System"),ncol = 1, nrow = 1), 
             plot_grid(af1, af2, af3, af4, labels=c("Row 1: 2.8 m", "Row 2: 7.15 m", "Row 3: 11.5 m", "Row 4: 15.85 m"),ncol = 2, nrow = 2),
  layout_matrix = rbind(c(1, 1, 2, 2),
                        c(1, 1, 2, 2))
)
dev.off()
#------------------------------------------------



#------------------------------------------------
# Fig. Efecto tiempo (ver Cuadro 1)
#------------------------------------------------
str(g$Rep)
g$Rep<-as.character(g$Rep)
g$Time<-as.character(g$Rep)
g$Time[g$Rep=="08:00:00"]<-"8"
g$Time[g$Rep=="09:00:00"]<-"9"
g$Time[g$Rep=="10:00:00"]<-"10"
g$Time[g$Rep=="11:00:00"]<-"11"
g$Time[g$Rep=="12:00:00"]<-"12"
g$Time[g$Rep=="01:00:00"]<-"13"
g$Time[g$Rep=="02:00:00"]<-"14"
g$Time[g$Rep=="12:40:00"]<-"12"
g$Time[g$Rep=="10:40:00"]<-"11"
g$Time<- as.numeric(g$Time)
#------------------------------------------------
pdf(file = "Results/Fig. Efecto tiempo (ver Cuadro 1, temp y ORP).pdf",width = 8, height = 6)
#------------------------------------------------
layout_matrix = rbind(c(1, 1, 1),
                      c(2, 2, 2))
layout(layout_matrix)
#------------------------------------------------
par(xpd = FALSE,mgp = c(2.5,0.5,0), mar = c(5,7,1,1))
pirateplot(formula = ORP ~  Time + Tratamiento, data = g, 
           main = "", xlab = "", ylab = "ORP (mV, ? SE)",
           ylim=c(-180,160),point.pch=NA,
           bar.f.col=c("white", "gray20"),avg.line.fun=mean,plot=TRUE,
           theme = 4, gl.col = NA,
           avg.line.col = "gray10",
           inf.f.col = "gray10",
           jitter.val = 0.09,
           avg.line.o = 1,bar.b.o=1,
           avg.line.lwd=3,
           inf.lwd=1,
           inf.method="se")
#------------------------------------------------
par(xpd = FALSE,mgp = c(2.5,0.5,0), mar = c(5,7,1,1))
pirateplot(formula = Temperatura ~  Time + Tratamiento, data = g, 
           main = "", xlab = "", ylab = "Temperatura (?C, ? SE)",
           ylim=c(20,30),point.pch=NA,
           bar.f.col=c("white", "gray20"),avg.line.fun=mean,plot=TRUE,
           theme = 4, gl.col = NA,
           avg.line.col = "gray10",
           inf.f.col = "gray10",
           jitter.val = 0.09,
           avg.line.o = 1,bar.b.o=1,
           avg.line.lwd=3,
           inf.lwd=1,
           inf.method="se")
#------------------------------------------------
dev.off()
#------------------------------------------------



#------------------------------------------------
# Fig. Efecto tiempo (ver Cuadro 1)
#------------------------------------------------
str(g)
head(g)
#------------------------------------------------
pdf(file = "Results/Fig. Efecto tiempo.pdf",
    width = 3, height = 6)
#------------------------------------------------
layout(matrix(c(1,1, 1,1, 
                2,2, 2,2, 
                3,3, 3,3), nrow = 3, byrow=T))
#------------------------------------------------
sum = summarySE(g, measurevar= "ORP", groupvars=c("Time", "Tratamiento"), na.rm=TRUE)
sum<-sum[,c(1,2,4,6)]
sum<-data.frame(sum)
names(sum)<-c("Time","Tratamiento","Mean","S.E.")
#------------------------------------------------
par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
plot(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],pch=19, type = "l", lwd=2, lty=1, col="darkred", ylim=c(-300,300),
     ylab='ORP (mV, ? SE)', xlab='')
error.bar.vertical(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],
                   sum$S.E.[sum$Tratamiento=="Entrada"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],pch=15, type = "l", lwd=2, lty=2, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],
                   sum$S.E.[sum$Tratamiento=="Salida Control"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],pch=17, type = "l", lwd=2, lty=3, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],
                   sum$S.E.[sum$Tratamiento=="Salida Pennisetum"], col = "black")
abline(h=0, lty=1, lwd=1)
legend("bottom", c("Inpunt", "Output-Control", "Output-Pennisetum"),lty=c(1, 2, 3),col="darkred",merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
#------------------------------------------------
sum = summarySE(g, measurevar= "pH", groupvars=c("Time", "Tratamiento"), na.rm=TRUE)
sum<-sum[,c(1,2,4,6)]
sum<-data.frame(sum)
names(sum)<-c("Time","Tratamiento","Mean","S.E.")
#------------------------------------------------
par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
plot(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],pch=19, type = "l", lwd=2, lty=1, col="darkred", ylim=c(6,8),
     ylab='pH (? SE)', xlab='Time (hours)')
error.bar.vertical(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],
                   sum$S.E.[sum$Tratamiento=="Entrada"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],pch=15, type = "l", lwd=2, lty=2, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],
                   sum$S.E.[sum$Tratamiento=="Salida Control"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],pch=17, type = "l", lwd=2, lty=3, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],
                   sum$S.E.[sum$Tratamiento=="Salida Pennisetum"], col = "black")
#------------------------------------------------
par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
sum = summarySE(g, measurevar= "Temperatura", groupvars=c("Time", "Tratamiento"), na.rm=TRUE)
sum<-sum[,c(1,2,4,6)]
sum<-data.frame(sum)
names(sum)<-c("Time","Tratamiento","Mean","S.E.")
sum
error.bar.vertical<-function(x, y, se.y, col){arrows(x, y-se.y, x, y+se.y, code=3, angle=90, length=0.05, col=col)}
#------------------------------------------------
par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
plot(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],pch=19, type = "l", lwd=2, lty=1, col="darkred", ylim=c(22,30),
     ylab='Temperature (?C, ? SE)', xlab='Time (hours)')
error.bar.vertical(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],
                   sum$S.E.[sum$Tratamiento=="Entrada"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],pch=15, type = "l", lwd=2, lty=2, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],
                   sum$S.E.[sum$Tratamiento=="Salida Control"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],pch=17, type = "l", lwd=2, lty=3, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],
                   sum$S.E.[sum$Tratamiento=="Salida Pennisetum"], col = "black")
#------------------------------------------------
dev.off()
#------------------------------------------------






#------------------------------------------------







