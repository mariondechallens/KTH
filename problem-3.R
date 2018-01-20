#Question 1
x_ar<- arima.sim(list(order = c(2,0,0),ar = c(1.3,-0.65)),sd = sqrt(280),n=1000)
pacf(x_ar)
acf(x_ar)
mean(x_ar)

# one step ar 2
c<-rep(0,50)
for (n in 1:50){
  x_ar<- arima.sim(list(order = c(2,0,0),ar = c(1.3,-0.65)),sd = sqrt(280),n=1000)
  ar2<-arima(x_ar, order = c(2,0,0))
  c[n]<-predict(ar2)$se[1]
}
plot(c,type="l",col="green")
mean(c)
sum(c)

#one step ar 10
d<-rep(0,50)
for (n in 1:50){
  x_ar<- arima.sim(list(order = c(2,0,0),ar = c(1.3,-0.65)),sd = sqrt(280),n=1000)
  ar2<-arima(x_ar, order = c(10,0,0))
  d[n]<-predict(ar2)$se[1]
}
lines(d,col="red")
mean(d)
sum(d)

#three steps ar 2
c<-rep(0,50)
for (n in 1:50){
  x_ar<- arima.sim(list(order = c(2,0,0),ar = c(1.3,-0.65)),sd = sqrt(280),n=1000)
  ar2<-arima(x_ar, order = c(2,0,0))
  c[n]<-predict(ar2,n.ahead=3)$se[1] + predict(ar2,n.ahead=3)$se[2] + predict(ar2,n.ahead=3)$se[3]
}
plot(c,type="l",col="green")
sum(c)
mean(c)

#three steps ar 10
d<-rep(0,50)
for (n in 1:50){
  x_ar<- arima.sim(list(order = c(2,0,0),ar = c(1.3,-0.65)),sd = sqrt(280),n=1000)
  ar2<-arima(x_ar, order = c(10,0,0))
  d[n]<-predict(ar2,n.ahead=3)$se[1] + predict(ar2,n.ahead=3)$se[2] + predict(ar2,n.ahead=3)$se[3]
}
lines(d,col="red")  #ar 10 seems better
sum(d)
mean(d)

ar2<-arima(x_ar, order = c(2,0,0)) #fitting AR(2)
ar10<-arima(x_ar, order = c(10,0,0))

pred<-predict(ar2)
pred2<-predict(ar2,n.ahead=3)

predict(ar10) # all coefficients after 2 are near zero
predict(ar10,n.ahead=3)

#Question 2
x_ar<- arima.sim(list(order = c(1,0,0),ar = 0.8),n=1000)
pacf(x_ar)
acf(x_ar)
mean(x_ar)

# one step ar 1
c<-rep(0,50)
for (n in 1:50){
  x_ar<- arima.sim(list(order = c(1,0,0),ar = 0.8),n=1000)
  ar2<-arima(x_ar, order = c(1,0,0))
  c[n]<-predict(ar2)$se[1]
}
plot(c,type="l",col="green")
mean(c)
sum(c)

#one step ma 10
d<-rep(0,50)
for (n in 1:50){
  x_ar<- arima.sim(list(order = c(1,0,0),ar =0.8),n=1000)
  ar2<-arima(x_ar, order = c(0,0,10))
  d[n]<-predict(ar2)$se[1]
}
lines(d,col="red")
mean(d)
sum(d)

#three steps ar 1
c<-rep(0,50)
for (n in 1:50){
  x_ar<- arima.sim(list(order = c(1,0,0),ar = 0.8),n=1000)
  ar2<-arima(x_ar, order = c(1,0,0))
  c[n]<-predict(ar2,n.ahead=3)$se[1] + predict(ar2,n.ahead=3)$se[2] + predict(ar2,n.ahead=3)$se[3]
}
plot(c,type="l",col="green")
sum(c)
mean(c)

#three steps ma 10
d<-rep(0,50)
for (n in 1:50){
  x_ar<- arima.sim(list(order = c(1,0,0),ar = 0.8),n=1000)
  ar2<-arima(x_ar, order = c(0,0,10))
  d[n]<-predict(ar2,n.ahead=3)$se[1] + predict(ar2,n.ahead=3)$se[2] + predict(ar2,n.ahead=3)$se[3]
}
lines(d,col="red")  #ma 10 seems better
sum(d)
mean(d)
ar2<-arima(x_ar, order = c(0,0,10)) #seven significant coefficients
ar<-arima(x_ar, order = c(1,0,0))
#why an MA(10) ?
#errors values are really near with both models but a little bit smaller with MA(10)
#with more coefficients, an error on one coefficient has less impact than with only one coefficient


#Question 3
c<-sqrt((1+sqrt(5))/2)
c2<-c**2
c2**2-c2-1
sigma2<-log(c**2)
x_ar<- arima.sim(list(order = c(1,0,0),ar = 0.8),n=1000,rand.gen = rlnorm, sdlog=sqrt((exp(sigma2)-1)*exp(sigma2)),meanlog=exp(sigma2/2))
pacf(x_ar)
acf(x_ar)
mean(x_ar) 
plot(x_ar)
ar2<-arima(x_ar, order = c(1,0,0))

a<-rep(0,50)
m<-rep(0,50)
mm<-rep(0,50)
for (n in 1:50){
  x_ar<- arima.sim(list(order = c(1,0,0),ar = 0.8),n=1000,rand.gen = rlnorm, sdlog=sqrt((exp(sigma2)-1)*exp(sigma2)),meanlog=exp(sigma2/2))
  
  ar2<-arima(x_ar, order = c(1,0,0))
  a[n]<-ar2$coef[1]
  m[n]<-ar2$sigma2
  mm[n]<-ar2$coef[2]
}
plot(a)
plot(m)
plot(mm)

#one step prediction
dd<-rep(0,50)
for (n in 1:50){
  x_ar<- arima.sim(list(order = c(1,0,0),ar = 0.8),n=1000,rand.gen = rlnorm, sdlog=sqrt((exp(sigma2)-1)*exp(sigma2)),meanlog=exp(sigma2/2))
  
  ar2<-arima(x_ar, order = c(1,0,0))
  dd[n]<-predict(ar2)$se[1]
}
plot(dd)
mean(dd)
sum(dd) 


#Comparison
#worse than question b)