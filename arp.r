rm(list=ls(all=TRUE));

parc<-read.csv2('parcmed.csv');

med<-data.frame(id=unique(parc$idparcela),medicao=1);
med<-rbind(med,data.frame(id=unique(parc$idparcela),medicao=2));
med<-med[order(med$id,med$medicao),];
parc<-merge(med, parc, by.x=c('id','medicao'),by.y=c('idparcela','medicao'),all.x=T);
calc<-with(parc[!is.na(parc$vtcc),],aggregate(list(nmed=id),list(id=id),length));
parc<-merge(parc,calc);

mpf<-subset(parc, nmed==2 & medicao==1,c('id','vtcc'));
names(mpf)<-c('id','x');

mpf2<-subset(parc, nmed==2 & medicao==2,c('id','vtcc'));
names(mpf2)<-c('id','y');

mpf<-merge(mpf,mpf2);
rm(mpf2);

mp1<-subset(parc, nmed==1 & medicao==1 & !is.na(vtcc),c('id','vtcc'));
names(mp1)<-c('id','x');

mp2<-subset(parc, nmed==1 & medicao==2 & !is.na(vtcc),c('id','vtcc'));
names(mp2)<-c('id','y');

m<-nrow(mpf);
cat(paste('N. de parcelas fixas: ',m),'\n');

u<-nrow(mp1);
cat(paste('N. de parcelas mensuradas somente na primeira medicao: ',u),'\n');

n1<-m+u;
cat(paste('N. de parcelas mensuradas na primeira medicao: ',n1),'\n');

n<-nrow(mp2);
cat(paste('N. de parcelas mensuradas somente na segunda medicao: ',n),'\n');

n2<-m+n;
cat(paste('N. de parcelas mensuradas na segunda medicao: ',n2),'\n');

(pu<-u/n1);
(pm<-m/n1);

x<-c(mpf$x,mp1$x);
y<-c(mpf$y,mp2$y);

(mxu<-mean(mp1$x));
(mxm<-mean(mpf$x));
(mym<-mean(mpf$y));
(myn<-mean(mp2$y));
(my<-mean(y));

(s2xm<-var(mpf$x));
(s2x<-var(x));

(s2ym<-var(mpf$y));
(s2yn<-var(mp2$y));
(s2y<-var(y));

(r<-cov(mpf$x,mpf$y)/(sqrt(s2xm)*sqrt(s2ym)));

(byx<-cov(mpf$x,mpf$y)/s2xm);

(a<-((m*pu)/(n2-pu*n*r^2))*byx);
(c<-(m/(n2-pu*n*r^2)));

my2f<-a*mxu-a*mxm+c*mym+(1-c)*myn;
cat(paste('Media da segunda ocasiao: ',round(my2f,2)),'\n');

s2my2f<-a^2*s2x*(1/u+1/m)+c^2*s2y/m+(1-c)^2*s2y/m-2*a*c*r*sqrt(s2xm)*sqrt(s2ym)/m;
cat(paste('Variancia da media da segunda ocasiao: ',round(s2my2f,2)),'\n');

smy2f<-sqrt(s2my2f);
cat(paste('Erro padrao da media da segunda ocasiao: ',round(smy2f,2)),'\n');

errounid<-qt(0.975,n2-1)*smy2f;
cat(paste('Erro da amostragem na unidade da variavel de interesse: ',round(errounid,2)),'\n');

erroperc<-round(errounid/my2f*100,2);
cat(paste('Erro da amostragem percentual: ',round(erroperc,2)),'\n');

#a.5) Intervalo de confiança
li<-my2f-errounid;
ls<-my2f+errounid;
cat(paste0('IC: ',round(li,2),' <= mu <= ',round(ls,2),' m³/ha'))

##ACS
acs<-cmrinvflor::estats_acs(parc$vtcc[parc$medicao==2 & !is.na(parc$vtcc)],sig = 5)
cat(paste('ACS: Erro (%): ',round(acs$eperc,2)),'\n');
cat(paste0('IC: ',round(acs$li,2),' <= mu <= ',round(acs$ls,2),' m³/ha'))


