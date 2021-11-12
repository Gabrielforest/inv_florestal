x11();
par(mfrow=c(1,2));
x<-runif(10000, 180, 250); #Distribuição uniforme
#x<-rexp(100000,0.02); #Distribuição exponencial
hist(x, main='',ylab='frequência',cex.main=0.9);
y<-matrix(NA,5000,1);
for (i in 1:5000){
  y[i]<-mean(sample(x,100));
}
hist(y,main='',ylab='frequência',cex.main=0.9);


y<-matrix(NA,100000,1);
for (i in 1:100000){
  y[i]<-mean(sample(1:60,6,replace = F));
}
hist(y,main='',ylab='frequência',cex.main=0.9);
mean(y);
