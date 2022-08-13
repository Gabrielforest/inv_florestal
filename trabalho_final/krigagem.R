### Bibliotecas obrigatórias ###
library(maptools)
library(raster)
library(geoR)
library(rgdal)
### Arquivos que devem ser importados ###
invparc <- read.csv2("./trabalho_final/hipso_parcela.csv", stringsAsFactors = F)
areaparc <- invparc$areaparc[1]
invparc$vha <- invparc$vprod_6*10000/areaparc
shpfaz <- readOGR("./trabalho_final/fazenda.shp")
plot(shpfaz)

#saber inforções do mapa
shpfaz@data 

#datum <- "+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs";
#proj4string(shpfaz) <- datum;  #sirgas 2000 23S (peguei no QGIS)

#wgs84 lat long
SRID_shp <- 4326 
CRS_shp <- paste("+init=epsg:", SRID_shp, sep = "")
proj4string(shpfaz) <- CRS(CRS_shp)

#sirgas 2000 23S
NOVA_SRID <- 32723 
CRS_NOVA_SRID <- paste("+init=epsg:", NOVA_SRID, sep = "")
shpfaz <- spTransform(shpfaz, CRS_NOVA_SRID)
#proj4string(shpfaz) <- CRS(CRS_NOVA_SRID)

### Informa??es gerais ###
#variavel de interesse
varselec <- "vha"
areaparc <- invparc$areaparc[1];       #area da parcela
#xpixel<-sqrt(areaparc);    #definindo o tamanho do pixel (a raiz quadrada do tamanho da parcela)
#ypixel<-xpixel;            #pixel quadrado: x = y

xpixel<-20;    #definindo o tamanho do pixel (a raiz quadrada do tamanho da parcela)
ypixel<-20;            #pixel quadrado: x = y


coordinates(invparc) <- ~x + y
proj4string(invparc) <- CRS(CRS_shp)
invparc <- spTransform(invparc, CRS_NOVA_SRID)

### Convers?o dos dados para o formato geodata
#vgeo<-as.geodata(invparc[,c("x","y",varselec)],coords.col=1:2,data.col=3);  #objeto padrao para processamento no geoR = valor de x, valor de y e a vari?vel de interesse
invparc<-as.data.frame(cbind(invparc@data,coordinates(invparc)));

vgeo<-as.geodata(invparc[,c("x","y",varselec)],coords.col=1:2,data.col=3);  #objeto padrao para processamento no geoR = valor de x, valor de y e a vari?vel de interesse

### AN?LISE VARIOGR?FICA  ###
binvgeo <- variog(vgeo)                       #gerar o variograma
binvgeo <- variog(vgeo, max.dist = 2000)

#### Plotar o semivariograma emp?rico para tra?ar as linhas de ajuste
plot(binvgeo, pch = "*",xlab = "Distância (m)", ylab = expression(paste("Semivariância(",m^6,")")));

### Par?metro iniciais para ajuste dos modelos
#tau<-min(binvgeo$v);
#sigma<-max(binvgeo$v)-tau;
#phi<-max(binvgeo$uvec)/3;
tau <- min(binvgeo$v);      #Efeito pepita
phi <- max(binvgeo$uvec)/3   #valor m?ximo  #Alcance   #mesma coisa
sigma <- max(binvgeo$v) - tau;    #Patamar   

### Ajuste utilizando M?nimos quadrados
(wlsvgeo <- variofit(binvgeo, ini = c(sigma, phi), nugget = tau, cov.model = "exp"))

### Tra?ar a linha de ajuste do modelo
lines(wlsvgeo)

### KRIGAGEM ###

###Liberarando mem?ria para processamento
memory.size(max = TRUE)

### Determina??o do ret?ngulo que envolve a ?rea a ser krigada
caixa<-bbox(shpfaz)
xmin<-caixa[1,1]
xmax<-caixa[1,2]
ymin<-caixa[2,1]
ymax<-caixa[2,2]


###Cria??o do raster da ?rea de interesse
rasterfaz <- raster(shpfaz)
ncol(rasterfaz) <- round((xmax - xmin)/xpixel, 0)
nrow(rasterfaz) <- round((ymax - ymin)/ypixel, 0)
#zera os pixels que estao fora do vetor
rasterfaz<-rasterize(shpfaz, rasterfaz)             
#transformar cada valor de x e y do meu vetor
reskrige<-as.data.frame(rasterToPoints(rasterfaz))

### Krigagem ordin?ria
kcvgeo <- krige.conv(vgeo, loc = as.matrix(reskrige[,c(1,2)]), krige = krige.control(obj = wlsvgeo))
reskrige$pred <- kcvgeo$predict

View(head(reskrige))

###Exportando dos resultados da krigagem
write.csv2(reskrige, "reskrige.csv", row.names = F)

coordinates(reskrige) <- ~x+y
gridded(reskrige) <- TRUE
reskrige$layer <- NULL
#Adicionando Projeção
proj4string(reskrige) <- CRS(CRS_NOVA_SRID)  
reskrige <- raster(reskrige) 
suppressMessages(writeRaster(reskrige, "reskrige.asc", overwrite = T))
#writeRaster(reskrige,"reskrige.tif","GTiff", overwrite=TRUE);



# deixar melhor esse gráfico ----------------------------------------------

###Exibi??o do mapa
reskrige <- mask(reskrige, shpfaz, inverse=FALSE) #recortando o raster com o talhao
#x11();
plot(reskrige, legend = TRUE, asp = 1)
plot(shpfaz, col = "transparent", add = TRUE, asp = 1)
grid(col = "gray80")
points(invparc$x,invparc$y, pch = "+", cex = 0.6)



