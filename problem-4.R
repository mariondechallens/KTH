
library(readr)
Data_Series_1 <- read_csv("~/KTH/Times Series Analysis/Project/Data_Series_1.txt", 
                          col_names = FALSE)
Data_Series_2 <- read_csv("~/KTH/Times Series Analysis/Project/Data_Series_2.txt", 
                          col_names = FALSE)
Data_Series_3 <- read_csv("~/KTH/Times Series Analysis/Project/Data_Series_3.txt", 
                        col_names = FALSE)
Data_Series_4 <- read_csv("~/KTH/Times Series Analysis/Project/Data_Series_4.txt", 
                          col_names = FALSE)
Data_Series_5 <- read_csv("~/KTH/Times Series Analysis/Project/Data_Series_5.txt", 
                          col_names = FALSE)
Data_Series_6 <- read_csv("~/KTH/Times Series Analysis/Project/Data_Series_6.txt", 
                           col_names = FALSE)

#View(Data_Series_1)

s1<-ts(Data_Series_1)
s2<-ts(Data_Series_2)
s3<-ts(Data_Series_3)
s4<-ts(Data_Series_4)
s5<-ts(Data_Series_5)
s6<-ts(Data_Series_6)

#Series 1
plot(s1)#little trend
mean(s1) #not constant
acf(s1) #there exists seasonal component
pacf(s1)


#s1compo<-decompose(s1)
#plot(s1compo)

s1diff<-diff(s1) #remove linear trends
plot(s1diff) #constant mean
mean(s1diff)# zero
acf(s1diff)# lag 1 and 2 MA(2) ?
pacf(s1diff)#really weird

ma2<-arima(s1diff, order = c(0,0,2))
test<-arima.sim(list(order = c(0,0,2),ma = c(-0.55,-0.43)),sd = sqrt(194),n=1000)

s<-rep(0,999)
for (i in 1:999){
  s[i]<-(s1diff[i]-test[i])**2
  
}
sqrt(sum(s))

#Residuals analysis
r<-rep(0,999)
for (i in 1:999){
  r[i]<-(-1)*(s1diff[i]-test[i])
  
}
r<-ts(r)
plot(r)#no trend, no season, mean zero
acf(r)#lag 1, 2 non zero

#Series 2
plot(s2)#little trend
mean(s2) #not constant
acf(s2) #there exists seasonal component
pacf(s2)
 
#decompose(s2)

s2diff<-diff(s2) #remove linear trends
plot(s2diff) #constant mean
mean(s2diff)# zero
acf(s2diff)# lag 1 MA(1) or 2, or more?
pacf(s2diff)#really weird

ma1<-arima(s2diff, order = c(0,0,1))
test<-arima.sim(list(order = c(0,0,1),ma = -0.55),sd = sqrt(238),n=500)

s<-rep(0,499)
for (i in 1:499){
  s[i]<-(s2diff[i]-test[i])**2
  
}
sqrt(sum(s))

#Residuals analysis
r<-rep(0,499)
for (i in 1:499){
  r[i]<-(-1)*(s2diff[i]-test[i])
  
}
r<-ts(r)
plot(r)#no trend, no season, mean zero
acf(r)#lag 1 non zero

ma2<-arima(s2diff, order = c(0,0,2))
test<-arima.sim(list(order = c(0,0,2),ma = c(-0.53,-0.43)),sd = sqrt(173),n=500)

s<-rep(0,499)
for (i in 1:499){
  s[i]<-(s2diff[i]-test[i])**2
  
}
sqrt(sum(s))

plot(test)

#Residuals analysis
r<-rep(0,499)
for (i in 1:499){
  r[i]<-(-1)*(s2diff[i]-test[i])
  
}
r<-ts(r)
plot(r)#no trend, no season, mean zero
acf(r)#lag 1,2 non zero

#Series 3
plot(s3)#seasonal component period 100
mean(s3) #seems zero
acf(s3) #
pacf(s3)# AR(4) or 8?

s3diff<-diff(s3)
plot(s3diff)
mean(s3diff)
acf(s3diff)
pacf(s3diff) #AR(4) or 7

# ALSO USE PReDICTION ERROR???
ar4<-arima(s3diff, order = c(4,0,0))
test<-arima.sim(list(order = c(4,0,0),ar = c(-1.27,-0.68,-0.36,-0.14)),sd = sqrt(0.285),n=500)

s<-rep(0,499)
for (i in 1:499){
  s[i]<-(s3diff[i]-test[i])**2
  
}
sqrt(sum(s))

#Residuals analysis
r<-rep(0,499)
for (i in 1:499){
  r[i]<-(-1)*(s3diff[i]-test[i])
  
}
r<-ts(r)
plot(r)#no trend, season, mean zero
acf(r)#BIG PROBLEM

ar7<-arima(s3diff, order = c(7,0,0))
test<-arima.sim(list(order = c(7,0,0),ar = c(-1.32,-0.82,-0.68,-0.5,-0.44,-0.35,-0.16)),sd = sqrt(0.2679),n=500)

s<-rep(0,499)
for (i in 1:499){
  s[i]<-(s3diff[i]-test[i])**2
  
}
sqrt(sum(s))

#Residuals analysis
r<-rep(0,499)
for (i in 1:499){
  r[i]<-(-1)*(s3diff[i]-test[i])
  
}
r<-ts(r)
plot(r)#no trend, season, mean zero
acf(r)#big problem
#ar7 has better aic and likelihood but higher sum square error

#Series 4
plot(s4)#big trend
mean(s4) #seems 1
acf(s4) #
pacf(s4)# AR(2)?

s4diff<-diff(s4) 
plot(s4diff)
mean(s4diff)
acf(s4diff) #seasonal MA(8)
pacf(s4diff)# 13

arma138<-arima(s4diff, order = c(13,0,8))


#Series 5
plot(s5)#trend
mean(s5) #seems 0
acf(s5) #
pacf(s5)# AR(1)?

s5diff<-diff(s5) 
plot(s5diff)
mean(s5diff)
acf(s5diff) #MA(2)
pacf(s5diff)#AR(1)

arma12<-arima(s3diff, order = c(1,0,2))
test<-arima.sim(list(order = c(1,0,2),ar = -0.91,ma=c(-0.42,-0.29)),sd = sqrt(0.2693),n=500)

s<-rep(0,499)
for (i in 1:499){
  s[i]<-(s5diff[i]-test[i])**2
  
}
sqrt(sum(s))

#Residuals analysis
r<-rep(0,499)
for (i in 1:499){
  r[i]<-(-1)*(s5diff[i]-test[i])
  
}
r<-ts(r)
plot(r)#no trend, season, mean zero
acf(r)#big problem

#Series 6
plot(s6)#no trends, no season, no transform
mean(s6) #seems 0
acf(s6) # MA(3)
pacf(s6)# AR(3)? or 6?


arma33<-arima(s3diff, order = c(3,0,3))
test<-arima.sim(list(order = c(3,0,3),ar = c(0.58,0.8,-0.44),ma=c(-1.96,0.99,-0.01)),sd = sqrt(0.2606),n=500)

s<-rep(0,499)
for (i in 1:499){
  s[i]<-(s6[i]-test[i])**2
  
}
sqrt(sum(s))

#Residuals analysis
r<-rep(0,499)
for (i in 1:499){
  r[i]<-(-1)*(s6[i]-test[i])
  
}
r<-ts(r)
plot(r)#no trend, season, mean zero
acf(r)#pb

arma63<-arima(s3diff, order = c(6,0,3))
test<-arima.sim(list(order = c(6,0,3),ar = c(-0.82,-0.8,-0.49,0.18,-0.09,0),ma=c(-0.52,0.64,-0.67)),sd = sqrt(0.2684),n=500)

s<-rep(0,499)
for (i in 1:499){
  s[i]<-(s6diff[i]-test[i])**2
  
}
sqrt(sum(s))

#Residuals analysis
r<-rep(0,499)
for (i in 1:499){
  r[i]<-(-1)*(s6[i]-test[i])
  
}
r<-ts(r)
plot(r)#no trend, season, mean zero
acf(r)#pb
#arma33 better for aic and likelihood, same for error


#Difficulties:
# detecting and Removing seasonal component
# diff works most of the time but log should work for big trends only with positive values!
# choosing between two models can be hard when the measurements of adequacy are similar
# 
# Alternative approaches:
# try different models even though it doesn t look like for instance take an MA for an AR
  
