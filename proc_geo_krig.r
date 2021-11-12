#KRIGAGEM ORDINÁRIA

library(rgdal);
library(raster);
library(geoR);

#Arquivos de entrada
shpplt<-readOGR('plantio.shp');

#shpplt<-shapefiles::read.shp('plantio.shp')

##Transformação do datum 
shpplt<-spTransform(shpplt,CRS('+init=epsg:31982'))

graphics.off(); x11(); 
plot(shpplt);
#areafaz<-sum(area(shpplt))/10000;

##Resultados por parcela
invparc<-read.csv2('invparc.csv', stringsAsFactors = F);
#View(invparc);
invparc<-invparc[,c('x','y','vcom')];
coordinates(invparc)<-~x+y;
#proj4string(invparc) <- CRS('+init=epsg:4326'); #Adicionando Projeção
#invparc<-spTransform(invparc,CRS('+init=epsg:31983'))
sum(area(shpplt)/10000)

points(invparc, col='red', pch=16);

##Informações gerais
resolucao<-10;

#Conversão dos dados para o formato geodata
vgeo<-as.geodata(invparc);

#Criação do raster da área de interesse
rasterplt<-raster(shpplt,resolution=resolucao);
rasterplt<-rasterize(shpplt,rasterplt);
reskrige<-as.data.frame(rasterToPoints(rasterplt));

#Análise variográfica
svar<-variog(vgeo);
svar<-variog(vgeo, max.dist = 1500);

graphics.off(); x11();
plot(svar, xlab='distância (m)',
           ylab='Semivariância(m^6)');

##Ajuste utilizando mínimos quadrados
tau<-0;
sigma<-1500-tau;
phi<-800;

ajuste<-variofit(svar,ini=c(sigma,phi),
                 nugget=tau,
                 cov.model = 'exp');
lines(ajuste);

##Krigagem convencional ("Krigagem ordinária")

kvgeo<-krige.conv(vgeo,
                  loc=as.matrix(reskrige[,1:2]),
                  krige=krige.control(obj=ajuste)
                  );
reskrige$vcom<-kvgeo$predict;

###Exportando dos resultados da krigagem
coordinates(reskrige)=~x+y;
gridded(reskrige)<-T;
reskrige$layer<-NULL;
proj4string(reskrige) <- proj4string(shpplt); #Adicionando Projeção
reskrige <- raster(reskrige); 
writeRaster(reskrige,'reskrige.tif',"GTiff", overwrite=TRUE);

###Exibição do mapa
reskrige <- mask(reskrige, shpplt, inverse=FALSE) #recortando o raster com o plantio
graphics.off();x11();
plot(reskrige, legend=TRUE, asp=1, main="KRIGAGEM: VCOM(m³/ha)");
plot(shpplt,lwd=2,border='darkblue',col='transparent',add=TRUE,asp=1);
points(invparc, pch = '+', cex=0.6)

#sp::compassRose(identify(reskrige,plot=F));
sp::compassRose(x=416500, y=7678500,cex=0.5)


