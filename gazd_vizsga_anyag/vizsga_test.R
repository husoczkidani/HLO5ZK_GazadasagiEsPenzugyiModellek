generate_numbers = function(){
	x="hlo5zk";#neptun kód
	z=charToRaw(iconv(x, "latin1", "UTF-8"))
	for (i in 1:6) v=paste("0x",z,sep="")
	e=strtoi(v)
	ax=e[1];ay=e[2];az=e[3];av=e[4];ss=sum(strtoi(v))+24
	cat("ax=",ax,"\n")
	cat("ay=",ay,"\n")
	cat("az=",az,"\n")
	cat("av=",av,"\n")
	cat("ss=",ss,"\n")
	ar=c( "FB","AAPL","AMZN","GOOG","NFLX","TSLA")
	ai=ss-6*floor(ss/6)
	ev=2020-(ss-10*floor(ss/10))
	cat("ev=",ev,"\n")
	cat("reszveny=",ar[ai+1],"\n")
}

generate_numbers()


#Készítsen az elõzõ kétdimenziós zn mintarealizációról
#statisztikai elemzést, azaz becsülje meg a paramétereket, 
#ferdeséget, lapultságot és adja meg hogy milyen eloszlás lehet(vizsgálat szükséges)! 
#Mintarealizáció
set.seed(629)
nx=1000
v=matrix(c(ax,abs(ax-az),abs(ax-az),az),2)
w=chol(v)
z1=rexp(nx)
z2=rexp(nx)
zm=matrix(c(z1,z2),ncol=2)
zn=zm%*%w

cat("zn=",zn,"\n")
print("Statisztika=");
print(summary(zn));
install.packages("moments");
library(moments);
print("Ferdeségek=");
print(skewness(zn));
print("Lapultságok=");
print(kurtosis(zn));

plot(zn, main="Ketdimenzios realizacio");


#Töltse le az R kód futtatásával kapott részvény	adatait a
#https://finance.yahoo.com/quote/reszveny/history?p=reszveny
#(vigyázat a "reszveny" kétszer szerepel.)
#honlapról az ev változó értékének megfelelõen (január 01-tõl december 31-ig)!
#Vizsgálja meg milyen eloszlású a napi záró árak("Close*") megváltozásának logaritmusa 
#(javasolt a logreturn, azaz ln(x_(n+1)/x_n) értékek vizsgálata)
#(minimum khí négyzet próba, ez azt jelenti, hogy meg kell adni az eloszlást paraméterekkel)! 
#Grafikus ábrázolás, pontbecslések és intervallumbecslések!!!
#részvényes feladat
reszveny=read.csv("D:/R scriptek/vizsga/TSLA.csv")
x = reszveny$Close
logreturn = c()
for(i in 1:length(x)-1){
   logreturn[i] = abs(log(x[i+1]/x[i]))
}
print(chisq.test(logreturn))
plot(logreturn, main="logreturn")
hist(logreturn)


#Az 1. feladatban adott adatokra vizsgálja meg a peremek függetlenségét!
#fuggetlensegvizsgalat
library(MASS)
x<-zn[,1]
y<-zn[,2]
tbl=table(x,y)
chisq.test(tbl)



#Az 1. feladatban adott adatokra készítsen többdimenziós (háromdimenzióst is) ábrázolást szintvonalakkal és perspektívikusan is
#(feliratozással, a kétdimenziós eloszlás jól látható legyen)!
#szintvonalak
contour(zn);
#perspektívikus
persp(zn, col="orange", shade = 0.4);


#Generáljon geometriai Brown folyamatot ( várható érték: mu= ax, szórás: sigma=(ax+az)/(ax+ay+az) értékkel, az idõintervallum 500 egység)!
#A generálás elõtt állítsa be a set.seed(ss+17) értéket. 
#Ábrázolja a folyamatot és vizsgálja meg a statisztikai jellemzõket!
#brown folyamat
set.seed(ss+17)
nsim <- 50
t <- 500
mu <- ax
sigma <- (ax+ay)/(ax+ay+az)
S0 <- 500
gbm_vec <- function(nsim = 100, t = 25, mu = 0, sigma = 0.1, S0 = 100, dt = 1./365) {
epsilon <- matrix(rnorm(t*nsim), ncol = nsim, nrow = t)  
gbm <- exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
gbm <- apply(rbind(rep(S0, nsim), gbm), 2, cumprod)
return(gbm)
}
gbm <- gbm_vec(nsim, t, mu, sigma, S0)
summary(gbm)

plot(gbm)



#nemtudom ez mi 
plot(zn)
points(zm,col='red')
N <- 900
a <- 1
b <- -1
Z <- rnorm(N)
epsilon <- rnorm(N)
eta <- rnorm(N)
aa <- runif(1)
bb <- runif(1)
X <- (aa + bb * Z + epsilon) + eta
Y <- a + b * X + epsilon
plot(X,Y)
abline(a,b, lty=2, lwd=3)
abline(lm(Y~X), col="red", lwd=3)



