
#Libraries
library("FactoMineR") # para an?lisis de datos
library("factoextra") # para visualizaci?n de gr?ficos, basado en ggplot2
library(ggplot2) #para hacer gr?ficos

#Data
db<-read.table("2004indicadores.csv", header= T, sep=",", dec=".")
dim(BC)
BC[1:5,]
names(BC)
dbok<- na.omit(db)
BCnum<-as.data.frame(dbok[ ,2:8])


#PCA:
G<-PCA(BCnum, scale.unit = TRUE, graph = TRUE)
#Scale.unit=TRUE estandariza los datos con promedio=0 y desviacion est?ndar=1
#ncp: numero de componentes principales
#Mapa de factores por individuos (muestras):
#Mapa de factores por variables:
#Ver descripci?n del objeto
print(G)

#An?lisis de valores propios (eigenvalues)
#Opciones:
#Elegir los que son mayor a 1
#Elegir los que cubren hasta 70 % o m?s de variaci?n
G$eig
#Gr?fica (scree plot)
fviz_screeplot(G, addlabels=TRUE)
#Gr?fica con ggplot2
Ge<-data.frame(G$eig, Component=c(1:7))
Ge
ggplot(Ge, aes(x=Component, y=percentage.of.variance, label=as.character(round(percentage.of.variance, digits=1)))) +
	geom_col(fill="blue")+
	geom_text(vjust= -1, hjust=0) +
	geom_line() +
	geom_point(size=4) +
	scale_y_continuous(name="Percentage of Variance") +
	scale_x_continuous(breaks=c(1:10))


#Resultados de variables (dimensiones)
names(G$var)
#Correlaci?n de variables con respecto a los componentes principales
#Usa valores G$var$coord (coordenadas)
fviz_pca_var(G, repel=TRUE)
#Variables que se correlacionan positivamente se agrupan juntas
#Variables que se correlacionan negativamente se agrupan en cuadrantes opuestos
#Entre m?s alejadas del centro, las variables est?n mejor representadas por los componentes principales

#Calidad con la que una variable es representada por cada componente principal
#Usa valores G$var$cos2
fviz_cos2(G, choice = "var", axes = 1:2)

#Analizar c?mo contribuye una variable a cada componente
#Usa valores G$var$contrib (en porcentages)
fviz_contrib(G, choice = "var", axes = 1)
fviz_contrib(G, choice = "var", axes = 2)

#Identificar variables que mejor contribuyen a cada componente
#Usa valores G$var$cor
G$var$cor

#An?lisis de muestras individuales
names(G$ind)

#Mapa de factores por individuos (muestras)
#Usa valores G$ind$coord
head(G$ind$coord)
fviz_pca_ind(G, geom.ind="point")


#EDICI?N DE GR?FICAS

#Colores
#Usar valores de cos2 (calidad de representaci?n) o contrib (contribuci?n a cada componente) para cambiar el color
#Crear vetor con tres colores, usando c?digo de colores hex (http://www.color-hex.com/)
cl<-c("#2793e8", "#ff0000","#559900")
cl<-c("#49ab81", "#398564","#1d4433")
fviz_pca_var(G, col.var = "cos2", gradient.cols = cl, repel = TRUE)
fviz_pca_var(G, col.var = "contrib", gradient.cols = "red", repel = TRUE)

#Color por grupos
Gplot<-fviz_pca_ind(G, geom.ind = "point", col.ind = BC$Diagnosis,
	palette = c("#2793e8", "#ff0000"), addEllipses=TRUE,
	legend.title="Diagnosis", mean.point=FALSE)

#Puntos en el gr?fico
fviz_pca_ind(G, geom.ind="point", pointsize = "cos2", pointshape = 21, fill = "#E7B800")
#Otros argumentos
#geom.var y geom.ind: point, text, arrow
#labelsize
#pointsize
#arrowsize
#pointshape, ver opciones:
ggpubr::show_point_shapes()
