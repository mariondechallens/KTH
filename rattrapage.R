#data1

df1=read.table("~/KTH/Times Series Analysis/data1.txt")
View(df1)
#df1$V2

s1<-ts(df1$V2)
plot(s1) #no trend, season?
mean(s1) #mean zero
acf(s1) #until lag 4 or 6
pacf(s1) #season? lag 3? 

ar.yw(s1) #26 pour AR!!

#arma34<-arima(s1, order = c(3,0,4)) #higher LL
#arma36<-arima(s1, order = c(3,0,6)) #smaller AIC
ma4<-arima(s1, order = c(0,0,4))
ma6<-arima(s1, order = c(0,0,6))
ar3<-arima(s1, order = c(3,0,0))
sum(ma4$residuals)
sum(ma6$residuals)
sum(ar3$residuals)

#final choice: ar3

#data2

df2=read.table("~/KTH/Times Series Analysis/data2.txt")
View(df2)
#df1$V2

s2<-ts(df2$V2)
plot(s2) #trend
mean(s2) #mean 8
acf(s2) 
pacf(s2) 

s22<-diff(s2)
plot(s22) # no trend
mean(s22) #mean 0
acf(s22) # lag 1 or 2
pacf(s22)

#decompose(s2)
ar.yw(s22) #pas ar

#final choice: ma2

#data3
df3=read.table("~/KTH/Times Series Analysis/data3.txt")
View(df3)
#df1$V2

s3<-ts(df3$V2)
plot(s3) #trend
mean(s3) #mean 2
acf(s3) 
pacf(s3) 

#decompose(s3)

s33<-diff(s3)
plot(s33) # no trend
mean(s33) #mean 0
acf(s33) # lag 1
pacf(s33) #lag 4

ma1<-arima(s33, order = c(0,0,1))
ar4<-arima(s33, order = c(4,0,0)) #almost the same
arma41<-arima(s33, order = c(4,0,1)) #??

sum(ma1$residuals)
sum(ar4$residuals)

ar.yw(s33) # ar6
ar6<-arima(s33, order=c(6,0,0))
sum(ar6$residuals)

#final choice: ar6