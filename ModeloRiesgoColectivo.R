# Tabajo 1 Econometria Financiera 
# Santiago Giraldo Henao 1152470139
# Modelo de Riesgo Colectivo

#Para poder descargar libreria adk se trabaja con remotes:

install.packages("remotes")
library(remotes)
install_version("adk")

library(MASS) # para fitdistrb
library(adk)  # para anderson durbin k sample test
library(fBasics) # para asimetria y curtosis
library(stringr)

D = read.table('B3.dat',header=T)


head(D,20)
tail(D,20)
summary(D)
#-----------1) lectura para generar vector de conteos

# Separar variable fecha en columnas

D[c('a','m','d')] <- str_split_fixed(D$fecha,'-',3)

D$a <- as.numeric(D$a)
D$m <- as.numeric(D$m)
D$d <- as.numeric(D$d)


# numero de casos x mes = nobs = Xn

summary(D$a)
a = seq(2000,2014)
n = length(a)

#Casos del primer año
b = tabulate(D$m[D$a==a[1] ])

if(length(b) <= 11){b= c(b,0)}
nobs = b

b

# Demás años

for(j in 2:n){
  b = tabulate(D$m[D$a==a[j] ])
  if(length(b) <= 11){b= c(b,0)}
  nobs = c(nobs,b)
}

#gráfica
Xn = nobs
n =seq(1,length(Xj))

plot(n,Xn,type='s')


#---------------------------punto a) estimacion de INAR(1)

# Dos procedimientos para estimar alpha lambda 

#----------------usando regresion lineal
require(data.table)

Xn1 = shift(Xn, n=1, fill=NA, type="lag")

m2 = lm(Xn ~ Xn1)
summary(m2)
coef.m2 = m2$coefficients
coef.m2

# media observada
mean(Xn)
#media teóricaY
(coef.m2[1]/(1-coef.m2[2]))



#----------------usando tscount (muy deficiente en este ejemplo...)


#Chequear que la media de parecida PENDIENTE

require(tscount)
Xn = nobs

m1 = tsglm(Xn, 
           model = list(past_obs = 1,past_mean=1), 
           xreg = NULL, 
           link = "identity",
           distr ="poisson")

summary(m1)



#-------------------parametros INAR(1)

#---E(X(n)) = lambda/(1-a)
#---Var(X(n))= a*(1+lambda)/(1-a^2)
#---E(X(n) | X(n-1)) = aX(n-1) + lambda


par.inar =c(m2$coef[2],m2$coef[1])
names(par.inar)= c("alfa","lambda")
(par.inar)



#--------------------------punto b)
# distribucion  Xn con gamlss

#----------en distribuciones discretas usar gamlss 
require(gamlss)

Sfit1 <- fitDist(y = Xn, type = "counts")


#------- 5 mejores distribuciones ajustes
Sfit1$fits[1:5]
?GPO

 #Estima par aXn la distribución
GPO <- histDist(Xn, "GPO", main = "GPO")
str(GPO)

#Guardan los parametros
(coef.GPO = c(mu=GPO$mu,sigma=GPO$sigma))

mean(Xn)
var(Xn)

#Comparar Media

#comparar con la Bin Neg
#---------------ajuste Xn ~ iid BinomialNeg

#--------------------la BNeg cumple esto:
(mean(Xn)<var(Xn))

#---------ajuste BinomialNeg libreria fitdistrplus

#Se rechaza en este punto que sea binomial negativa 
require(fitdistrplus)
fitnb <- fitdist(Xn,"nbinom")
gofstat(fitnb)

#---------ajuste BinomialNeg libreria vcd

library(vcd)
gf <- goodfit(Xn, type = "nbinom", method = "ML")
summary(gf)

par(mfrow=c(1,1))
plot(gf)
coef.bn = gf$par


#--------------- comparacion ajuste


Bn = as.data.frame(table(Xn))
fn = Bn$Freq
pn = fn/sum(fn)
xn = as.numeric(paste(Bn$Xn))
(cbind(xj,pn))

par(mfrow=c(1,1))
plot(xn,pn,type='h',ylim=c(0,0.1))

#Binomial NEgativa Rojo
#GPO Azul

?GPO
coef.GPO[2]
GPO.teo = dGPO(xn, mu=coef.GPO[1],sigma=coef.GPO[2])
points(xn,GPO.teo,pch=20,col='blue')


bn.teo = dnbinom(xn,size=coef.bn$size,prob=coef.bn$prob)
points(xn,bn.teo,pch=20,col='red')



#--------------------------------punto c)
# valores simulados versus observados



#------------simular Xn = INAR(1)+Poisson
N = length(Xn)
INARn = integer(N)
INARn[1] = mean(Xn)
#Ecuación 4.8
for(j in 2:N){
  INARn[j] = sum(rbinom(INARn[j-1],size=1,prob=par.inar[1]))+
    rpois(1,lambda=par.inar[2])}

#------------simular Xn = INAR(1)+GPO
N = length(Xn)
INARZn = integer(N)
INARZn[1] = mean(Xn)
for(j in 2:N){
  INARZn[j] = sum(rbinom(INARZn[j-1],size=1,prob=par.inar[1]))+
    rGPO(1,  mu=coef.GPO[1],sigma=coef.GPO[2])}

(EXZn=mean(INARZn))
mean(INARZn)
mean(Xn)
par(mfrow=c(1,1))

np = length(Xn)

fecha = seq(as.Date("2000/01/01"), as.Date("2014/12/01"), by="months")

par(mfrow=c(1,1))
plot(fecha,Xn, xaxt="n",panel.first = grid(),type='s',col='black')
axis.Date(1, at=seq(fecha[1],fecha[np], "months"), format="%m/%y")
axis.Date(1, at=seq(fecha[1],fecha[np], "years"), labels = FALSE, tcl = -0.2)


lines(fecha,INARn,col='magenta',type='s')

lines(fecha,INARZn,col='blue',type='s')
abline(h=mean(INARZn),col='red')
abline(h=mean(INARn))
abline(h=mean(Xn))



legend("topleft", 
       c("datos","INAR+Poisson","INAR+GPO"), 
       col = c("black","magenta","blue"),
       lwd=c(2,2,2),lty=c(1,1,1) )

# Comentario de que tanto ajusta la serie

#La serie INARBN ajusta mejor por
# - El estaditico 
# - Se acerca más a la media 
# - Ajusta mejor en cuanto sigue los datos y tienen menos varianza lo qu ehace que no sea tan sensible


# 2----------- Analisis de Costos 

# Leer los datos punto 2

D = read.table('B3.dat',header=T)

head(D,20)

D[c('a','m','d')] <- str_split_fixed(D$fecha,'-',3)

D$a <- as.numeric(D$a)
D$m <- as.numeric(D$m)
D$d <- as.numeric(D$d)

x <- D$costo

summary(D)

par(mfrow=c(2,2))
plot(density(x))
hist(x,100)


# Se buscan quitar los datos extremos 
# Sin embargo no se quita desde el per 90, sino desde el per 98, ya que no queremos que el modelo desconozca estos datos

quantile(x,seq(0.1,1,0.1))
(qx98 = quantile(x,0.98))

#Suponemos que la empresa se cubre mediante un seguro a partir de este valor

Yn = x[x < qx98]

plot(density(Yn))
hist(Yn,100)

((length(x)-length(Yn))/length(Yn))*100

#----------------------------------------punto a)

#---------analizar posibles correlaciones en los costos

ts.plot(Yn[1:200],type='l',main="y")
abline(h=mean(Yn),col='red')
#Ver autocorrelación

# se ven lineas suavizadas po rlo que puede exitir autocorrelacion entre los datos


#----------------------proceso con correlacion Gaussiano?

require(TSA)
TSA::acf(Yn,190,ci.type="ma",drop.lag.0 = TRUE,main="acf y")

# Decrece muy rápido a 0


#----------declarar y serie de tiempo
Yns = ts(Yn,frequency=12)

library(forecast)
auto.arima(Yns)



#----------------------------------punto b)
#------estimacion en modelo GAR(1) con Gamma invariante



require(data.table)

#Resago de Yn 
y1 = shift(Yn, n=1, fill=NA, type="lag")

#Resago 2 de Yn
y2 = shift(Yn, n=2, fill=NA, type="lag")

#Resago 3
y3 = shift(Yn, n=3, fill=NA, type="lag")

M = na.omit(data.frame(x1=y1,x2=y2,y=Yn))

#----------


# Mixed GAM Computation Vehicle with Automatic Smoothness Estimation
require(mgcv)


gar1 <- gam(y ~ x1+x2,
            family=Gamma(link = "identity"),data=M)
summary(gar1)


# 4.56 Pendiente = mu sobre alpha 
coef.gam = gar1$coefficients[1:2]
coef.gam

#----------parametros: alfa, beta, nu
#         Yn = sum(1,Poisson(Y(n-1).alfa.beta),Exp(alfa)) + Zn
#         Zn ~ Gamma(nu,alfa)

#--------distrib. invariante de Yn ~ Gamma(nu,alfa(1-beta)) 


# E(Yn|Y(n-1)) = beta Y(n-1) + nu/alfa
# EYn = nu/(alfa(1-beta))
# VarYn = nu/(alfa(1-beta))^2



(EYn = mean(Yn))
(VarYn = var(Yn))

(beta.est = coef.gam[2])

(nu.est = EYn^2/VarYn)

(alfa.est = nu.est/coef.gam[1])
# tres coeficientes 
coef.gar1 = c(alfa=alfa.est, beta = beta.est,nu=nu.est)
coef.gar1
#Parametros de la distribución invariante
coef.gar1.gamma = c(shape=nu.est, rate = alfa.est*(1-beta.est))

coef.gar1.gamma
(coef.gar1.gamma[1]/coef.gar1.gamma[2])


(coef.gar1.gamma[1]/coef.gar1.gamma[2]^2)



#--------------------------------punto c)
#----------simulacion con GAR(1) : es una forma de comprobar
#          si el modelo ajusta los datos

N = length(Yn)

Wn = rexp(N,alfa.est)
Zn = rgamma(N,shape=nu.est,rate = alfa.est)

# Serie simulada
GAR1n = double(N)

#Ecuacion 4.53
GAR1n[1] = EYn
for(j in 2:N){
  r = sum(rexp(rpois(1,GAR1n[j-1]*beta.est*alfa.est),alfa.est))
  GAR1n[j] = r+rgamma(1,shape=nu.est,rate=alfa.est)}


par(mfrow=c(1,1))
t = seq(1,N)
plot(t,Yn,type='l',col=rgb(1,0,0),ylab='y  versus  y.est',
     xlab='n')
lines(t,GAR1n, col = rgb(0,0,1),lwd=2)


#--- Calculo de probabilidad de insovencia
#----Determinacion de la prima
#----Determinar reservas

#---- asume  costos = Yn ~ invariante GAR(1) Gamma

#----------------------------------punto a)
#--------------calcule la fgm de Yn
require(actuar)

#------My(t) = 1/(1-t/rate)^nu , t < rate
(coef.gar1.gamma[2])

t=seq(-0.2,coef.gar1.gamma[2],0.001)

mt = mgfgamma(t, shape = coef.gar1.gamma[1],
              scale = 1/coef.gar1.gamma[2])

plot(t,mt,type='l',ylim=c(0.8,4))

#------------------------------------punto b)
#----------calcular la prima C

# promedio Xn con INAR(1)

(EXn = par.inar[2]/(1-par.inar[1]))

# promedio conteo con Binomial Negativa

(EXn = coef.bn$size*(1-coef.bn$prob)/coef.bn$prob)

# promedio conteo con datos

(EXn = mean(Xn))

# promedio costos

(EYn = coef.gar1.gamma[1]/coef.gar1.gamma[2])

# shape * scale =shape/rate = media

mean(Yn)

theta = 0.1
(C = (1+theta)*EXn*EYn)

#Comparar estos dos valores

#-------------calcular coeficiente de ajuste



lambda.est = par.inar[2]
a.est = par.inar[1]
nu = coef.gar1.gamma[1]
alfa = 1/coef.gar1.gamma[2]


cr = function(t){
  mt = mgfgamma(t, shape = coef.gar1.gamma[1],scale = alfa)
  p=(lambda.est*(1-a.est)*mt)/(1-a.est*mt)-lambda.est-C*t
  return(p)}

length(t)

fm = double(length(t))
for(j in 1:length(t)){
  fm[j]=cr(t[j])}


par(mfrow=c(1,1))
plot(t,fm,type='l',ylim=c(-0.1,1.5),xlim=c(-0.1,0.1))
abline(h=0)

require(nleqslv)

#Resuleve la ecuación
k=nleqslv(0.05, cr, control=list(btol=0.0001))$x
(k)
k
abline(v=k,col='red')

#--------------calculo reservas

Prob.insolv = c(0.001,0.01,0.02,0.03,0.04)

Prob.insolv
reservas = double(length(Prob.insolv))
for(j in 1:length(reservas)){
  reservas[j] = -log(Prob.insolv[j])/k}

(B = cbind(reservas,Prob.insolv))

#-------------punto c)
# simular la reserva y calcular la probabilidad de insolvencia

#------------simular Xn = INAR(1)+Poisson
N = 500
INARn = integer(N)
INARn[1] = EXn
for(j in 2:N){
  INARn[j] = sum(rbinom(INARn[j-1],size=1,prob=par.inar[1]))+
    rpois(1,lambda=par.inar[2])}

#------costos totales cada mes con Yn = GAR(1)

Sn = double(N)
for(j in 1:N){
  Yj = rgamma(INARn[j],shape=coef.gar1.gamma[1],rate=coef.gar1.gamma[2])
  Sn[j] = sum(Yj)}

(ESn=mean(Sn))
(ESn < C)

#--------estimar insolvencia con simulacion
reservas[2]
Un = double(N)
U0 = reservas[2]
Un[1] = U0 + C-Sn[1]
U0

for(j in 2:N){
  Un[j] = Un[j-1] + C-Sn[j]}

par(mfrow=c(1,1))

tn = seq(1,N)
plot(tn,Un,type='l',ylim=c(-20,3000))
abline(h=0,col='red')

Prob = double(10000)

for(k in 1:10000){
  Xn[1] = EXn
  for(j1 in 2:N){
    INARn[1] = EXn
    INARn[j1] = sum(rbinom(INARn[j1-1],size=1,prob=a.est))+
      rpois(1,lambda=lambda.est)}
  for(j2 in 1:N){
    Yj = rgamma(INARn[j2],shape=coef.gar1.gamma[1],rate=coef.gar1.gamma[2])
    Sn[j2] = sum(Yj)}
  Un[1] =   U0 + C-Sn[1]
  for(j3 in 2:N){
    Un[j3] = Un[j3-1] + C-Sn[j3]}
  Prob[k] = ifelse(min(Un) < 0, 1, 0)
  lines(tn,Un,col='gray')}

#reportar gráfica

#Probabilidad Teórica vs esta probabilidad
# aumentar simulaciones para ver si converge 
(prob.insol = sum(Prob)/10000)
Prob
sum(Prob)


#--------ANALISIS ADICIONAL. Continua el programa analisis.costos.r
# -------usar libreria gamlss para encontrar
#        las distribuciones que mejor ajustan los datos de Yn
#        es usar un modelo iid en lugar de GAR(1)

require(gamlss)


Sfit1 <- fitDist(y = Yn, type = "realplus")

#------las  mejores 5 con respecto al AIC

Sfit1$fits[1:5]

#-----estimacion max ver de GG : tiene generadora de momentos
#     ver: Notas de Gestion de Riesgos, pag. 58, sec. 3.6.2
#     GG : Gamma Generalizada
#     aic GG = -2167

#------------estimacion max ver con GG, GB2
H = gamlssML(Yn, family = GG)
str(H)
?GG
coef.GG = c(mu = H$mu, sigma = H$sigma, nu = H$nu)
coef.GG[1]

library(truncgof)
stats::ks.test(Yn, "pGG", coef.GG) 

#-------visualizar el ajuste Gamma versus GG
par(mfrow=c(1,1))

hist(Yn,50, probability=T,
     col='light gray', main='Costos ')

curve(dGG(x, mu = coef.GG[1], sigma = coef.GG[2],
          nu = coef.GG[3], log = FALSE), 
      col='red', lwd=2, lty=1, add=T)


legend("topright", 
       c("datos","Max.ver"), 
       col = c("darkgray","red"),
       lwd=c(2,2),lty=c(1,2) )


#----------------------compara GG versus Gamma de GAR(1)

hist(Yn,50, probability=T,
     col='light gray', main='Costos ')



curve(dGG(x, mu = coef.GG[1], sigma = coef.GG[2],
          nu = coef.GG[3], log = FALSE), 
      col='red', lwd=2, lty=1, add=T)

curve(dgamma(x, shape=coef.gar1.gamma[1] ,rate = coef.gar1.gamma[2]), 
      col = "blue",lwd=2, add = TRUE)

legend("topright", 
       c("obs","GG","Gamma"), 
       col = c("darkgray","red","blue"),
       lwd=c(2,2,2),lty=c(1,1,1) )

#----------------------compara las fda con la fda empirica

sorty <- sort(Yn)

plot(sorty, pGG(sorty,  mu = coef.GG[1], sigma = coef.GG[2],
                nu = coef.GG[3]),type="l",col="red", 
     main="ECDF y CDF  ", lwd=2,ylab="")
plot(ecdf(sorty),add=TRUE,lwd=2,verticals= TRUE, do.points = FALSE)


lines(sorty, pgamma(sorty,shape=coef.gar1.gamma[1], rate=coef.gar1.gamma[2]),
      col='blue', lwd=2, lty=1)


legend("bottomright", 
       c("obs","GG","Gamma"), 
       col = c("darkgray","red","blue"),
       lwd=c(2,2,2),lty=c(1,1,1) )

###------------ Simulación con modelo alternativo GG

require(actuar)

#------My(t) = 1/(1-t/rate)^nu , t < rate
(coef.GG[2])

t=seq(-0.2,coef.GG[2],0.001)
coef.GG[1]
mt = mgfgamma(t, shape = coef.GG[1],
              scale = 1/coef.GG[2])

plot(t,mt,type='l',ylim=c(0.8,4))

#------------------------------------punto b)
#----------calcular la prima C

# promedio Xn con INAR(1)

(EXn = par.inar[2]/(1-par.inar[1]))

# promedio conteo con Binomial Negativa

(EXn = coef.bn$size*(1-coef.bn$prob)/coef.bn$prob)

# promedio conteo con datos

(EXn = mean(Xn))

# promedio costos

(EYn = coef.GG[1]/coef.GG[2])

# shape * scale =shape/rate = media

mean(Yn)

theta = 0.1
(C = (1+theta)*EXn*EYn)

#Comparar estos dos valores

#-------------calcular coeficiente de ajuste



lambda.est = par.inar[2]
a.est = par.inar[1]
nu = coef.GG[1]
alfa = 1/coef.GG[2]

cr = function(t){
  mt = mgfgamma(t, shape = coef.GG[1],scale = alfa)
  p=(lambda.est*(1-a.est)*mt)/(1-a.est*mt)-lambda.est-C*t
  return(p)}

length(t)

fm = double(length(t))
for(j in 1:length(t)){
  fm[j]=cr(t[j])}


par(mfrow=c(1,1))
plot(t,fm,type='l',ylim=c(-0.1,1.5),xlim=c(-0.1,0.1))
abline(h=0)

require(nleqslv)

#Resuleve la ecuación
k=nleqslv(0.05, cr, control=list(btol=0.0001))$x
(k)
k
abline(v=k,col='red')

#--------------calculo reservas

Prob.insolv = c(0.001,0.01,0.02,0.03,0.04)

Prob.insolv
reservas = double(length(Prob.insolv))
for(j in 1:length(reservas)){
  reservas[j] = -log(Prob.insolv[j])/k}

(B = cbind(reservas,Prob.insolv))

#-------------punto c)
# simular la reserva y calcular la probabilidad de insolvencia

#------------simular Xn = INAR(1)+Poisson
N = 500
INARn = integer(N)
INARn[1] = EXn
for(j in 2:N){
  INARn[j] = sum(rbinom(INARn[j-1],size=1,prob=par.inar[1]))+
    rpois(1,lambda=par.inar[2])}

#------costos totales cada mes con Yn = GG

Sn = double(N)
for(j in 1:N){
  Yj = rgamma(INARn[j],shape=coef.GG[1],rate=coef.GG[2])
  Sn[j] = sum(Yj)}

(ESn=mean(Sn))
(ESn < C)

#--------estimar insolvencia con simulacion
reservas[2]
Un = double(N)
U0 = reservas[2]
Un[1] = U0 + C-Sn[1]
U0

for(j in 2:N){
  Un[j] = Un[j-1] + C-Sn[j]}

par(mfrow=c(1,1))

tn = seq(1,N)
plot(tn,Un,type='l',ylim=c(-20,3000))
abline(h=0,col='red')

Prob = double(1000)

for(k in 1:1000){
  Xn[1] = EXn
  for(j1 in 2:N){
    INARn[1] = EXn
    INARn[j1] = sum(rbinom(INARn[j1-1],size=1,prob=a.est))+
      rpois(1,lambda=lambda.est)}
  for(j2 in 1:N){
    Yj = rgamma(INARn[j2],shape=coef.GG[1],rate=coef.GG[2])
    Sn[j2] = sum(Yj)}
  Un[1] =   U0 + C-Sn[1]
  for(j3 in 2:N){
    Un[j3] = Un[j3-1] + C-Sn[j3]}
  Prob[k] = ifelse(min(Un) < 0, 1, 0)
  lines(tn,Un,col='gray')}

#reportar gráfica

#Probabilidad Teórica vs esta probabilidad
# aumentar simulaciones para ver si converge 
(prob.insol = sum(Prob)/1000)

