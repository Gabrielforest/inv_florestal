##Amostragem casual estratificada

parc<-read.csv2('parcelas.csv');
View(parc);
names(parc);

parc$vary<-parc$vcom;
sig<-0.05; #alpha=5%

#Médias por estrato
estrato<-with(parc,aggregate(
  list(areaest=areaest,areaparc=areaparc,ym=vary),
  list(estrato=estrato),
  mean
));

#Número de parcelas por estrato
calc<-with(parc,aggregate(
  list(anj=parc),
  list(estrato=estrato),
  length
));

estrato<-merge(estrato,calc);

#Variância e desvio padrão por estrato
calc<-with(parc,aggregate(
  list(s2y=vary),
  list(estrato=estrato),
  var
));
calc$sy<-sqrt(calc$s2y);

estrato<-merge(estrato,calc);

#Número de parcelas cabíveis por estrato
estrato$pnj<-with(estrato,areaest*10000/areaparc);

#Nomeando a população
estrato$populacao<-1;

#Área, número de amostras e número de amostras cabíveis na
#população

populacao<-with(estrato,aggregate(
  list(area=areaest,an=anj,pn=pnj),
  list(populacao=populacao),
  sum
));

estrato<-merge(populacao,estrato);

#Peso de cada estrato
estrato$pwj<-with(estrato,anj/an);

#Cálculo da média estratificada

calc<-with(estrato,aggregate(
  list(ymstr=pwj*ym),
  list(populacao=populacao),
  sum
));

populacao<-merge(populacao,calc);

#Variância da média estratificada
calc<-with(estrato,aggregate(
  list(calc1=(pwj^2)*s2y/anj, calc2=(pwj*s2y)/pn),
  list(populacao=populacao),
  sum
));
calc$s2ystr<-calc$calc1-calc$calc2;
calc$calc1<-NULL;
calc$calc2<-NULL;

populacao<-merge(populacao,calc);

#Cálculo do grau de liberdade efetivo
estrato$calcgl<-with(estrato,pnj*(pnj-anj)/anj);
calc<-with(estrato,aggregate(
  list(calc1=calcgl*s2y, 
       calc2=(calcgl*s2y)^2/(anj-1)),
  list(populacao=populacao),
  sum
));
calc$gle<-with(calc,calc1^2/calc2);
calc$calc1<-NULL;
calc$calc2<-NULL;

populacao<-merge(populacao,calc);
populacao$systr<-sqrt(populacao$s2ystr);

populacao$errounid<-with(populacao, qt(1-sig/2,gle)*systr);
populacao$erroperc<-with(populacao, errounid/ymstr*100);

total<-with(populacao,ymstr*pn);
etotal<-with(populacao,errounid*pn);

##Intervalo de confiança da população
litot<-total-etotal;
lstot<-total+etotal;

print(paste('IC:',round(litot),'<=T<=',round(lstot),'m³',sep=''));



library(cmrinvflor);

estrato<-subset(parc,
                !duplicated(estrato),
                select=c('estrato','areaest'))
amostra<-parc[,c('estrato','parc','areaparc','vary')]

ace<-estats_ace(estrato,amostra,sig=sig*100,fc_dim=1/10000);



