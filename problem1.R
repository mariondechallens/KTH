library(MASS) 

#simulation of iid samples
x<-mvrnorm(n=1,rep(0,1000),diag(1,1000,1000))
x_ts<-ts(x)

#autocorrelation
# cor(x_ts[-100],x_ts[-1]) #lag1
# cor(x_ts[-(99:100)],x_ts[-(1:2)]) #lag2
# acf(x_ts,plot=FALSE)
a<-acf(x_ts,lag.max = 200) # values near zero => uncorrelated so WN
mean(x_ts)

c<-rep(0,100)
for (n in 1:100){
  x<- mvrnorm(n=1,rep(0,n),diag(1,n,n))
  
  c[n]<-mean(x)
}

plot(c)

#AR(1)
x_ar<- arima.sim(list(order = c(0,0,0)),n=50) #Wn when all 0
x_ar<- arima.sim(list(order = c(2,0,0),ar = c(0.5,0.4)),n=1000)
x_ar<- arima.sim(list(order = c(1,0,0),ar = 0.05),n=50)
# cor(x_ar[-100],x_ar[-1]) #lag1
# cor(x_ar[-(99:100)],x_ar[-(1:2)]) #lag2
acf(x_ar)
acf(x_ar,lag.max = 200)
pacf(x_ar) # non zero at lag 1

#MA(1)
x_ma<- arima.sim(list(order = c(0,0,2),ma = c(1.7,1.7)),n=1000)
x_ma<- arima.sim(list(order = c(0,0,1),ma = 0.7),n=50)
# cor(x_ma[-100],x_ma[-1]) #lag1
# cor(x_ma[-(99:100)],x_ma[-(1:2)]) #lag2
acf(x_ma) #non zero at lag 1 or lag 1 and 2 for order 2
acf(x_ma,lag.max = 200)
pacf(x_ma)


#portmanteau test if p-value small, we reject (Ho : this is iid noise)
Box.test(x_ts) #p value 0.99, so we clearly accept Ho
Box.test(x_ar)
Box.test(x_ma)#very small p value we reject Ho

c<-rep(0,100)
for (n in 2:101){
  x<- mvrnorm(n=1,rep(0,n),diag(1,n,n))
  
  c[n-1]<-Box.test(x)[3]
}

plot(c)
#the turning point test
#turning.point.test(x_ts)

library(BSDA)

#the difference sign test  Ho : no trend in the data, iid sample

SIGN.test(x_ts) #high p value we accept Ho
SIGN.test(x_ar)#0,3
SIGN.test(x_ma) #0,1

#rank test Ho : no trend in the data, iid sample
wilcox.test(x_ts) #0,6 high
wilcox.test(x_ar)#very small
wilcox.test(x_ma)#very small

#Yule-Walker algorithm : selected order equal to zero suggests that the data is WN

ar.yw(x_ts) # order 0
ar.yw(x_ar) #order 1
ar.yw(x_ma) #order 4

#Influence of sample size
c<-rep(0,300)
for (n in 1:300){
  x<-mvrnorm(n=1,rep(0,n),diag(1,n,n))
  x<-ts(x)
  c[n]<-mean(x)
}
plot(c)

