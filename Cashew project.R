set.seed(123)
d=read.csv("Cashew.csv",header=TRUE)
dd=d[1:120,c(-1,-9,-11,-14)]
head(dd)
attach(dd)
X=cbind(RL,RB,RT,RW ,KL ,KB ,KT,NC,Sinkers..Count.basis.,Moisture )
summary(dd)
library(moments)
Y=Yield;Y
n=length(Yield);n
m=apply(X,2,mean)
sd=apply(X,2,sd)
test=data.frame(d[121:133,c(-1,-9,-11,-14)]);test
test1=data.frame(test[,11]);test1

summary(lm(Yield~.,data=dd))
reg=(lm(Yield~.,data=dd))
step(reg)
fit=reg$residual;fit
Xc=data.frame(X)
fo=lm(Yield~.,data=Xc)$residual;fo
fit1=lm(Yield~.,data=Xc)$fitted;fit1
ssr=sum((fit1-me)^2);ssr
ee=test1-fit1
sse=sum(ee^2);sse
sst=sse+ssr;sst
rsq=ssr/sst;rsq
rmse=sqrt((sum((test1-fit1)^2))/11);rmse



me=apply(test1,2,mean)
#Decision tree
library(e1071)
library(rpart.plot)
dec=rpart(Yield~.,data=d[,-1]);dec
dec$y
rpart.plot(dec,type=1,extra=1)
fit2=predict(dec,test1);fit2
SSR=sum((fit2-me)^2);SSR
e=test1-fit2
SSE=sum(e^2);SSE
SST=SSE+SSR;SST
Rsq=SSR/SST;Rsq
rmse1=sqrt((sum((test1-fit2)^2))/11);rmse1

#MLR



#SVR
sv=svm(dd,Y,kernal="guassian");sv
fit3=predict(sv,dd);fit3
SSR1=sum((fit3-me)^2);SSR
e1=test1-fit3
SSE1=sum(e1^2);SSE1
SST1=SSE1+SSR1;SST1
Rsq1=1-(SSE1/SST1);Rsq1
rmse2=sqrt((sum((test1-fit3)^2))/nrow(test));rmse2


#Random forest
library(randomForest)
rf <- randomForest(Y
   ~ .,  data=dd
)

fit4 = predict(rf, dd)
SSR2=sum((fit4-me)^2);SSR2
e2=test1-fit4
SSE2=sum(e2^2);SSE2
SST2=SSE2+SSR2;SST2
Rsq2=1-(SSE2/SST2);Rsq2
rrmse3=sqrt((sum((test1-fit4)^2))/nrow(test));rrmse3

#Bagging
library(ipred)
Bag <- bagging(Yield ~.,data=dd, coob=TRUE)
fit5=predict(Bag,dd,method="standard");fit5
SSR3=sum((fit5-me)^2);SSR3
e3=test1-fit5
SSE3=sum(e3^2);SSE3
SST3=SSE3+SSR3;SST3
Rsq3=1-(SSE3/SST3);Rsq3
rmse4=sqrt((sum((test1-fit5)^2))/11);rmse4


##KNN
knnmodel = knnreg(dd, Yield)
pred_knn <- predict(knnmodel, (dd))
SSR4=sum((pred_knn-me)^2);SSR4
e4=test1-pred_knn
SSE4=sum(e4^2);SSE4
SST4=SSE4+SSR4;SST4
Rsq4=1-(SSE4/SST4);Rsq4
rmse5=sqrt((sum((test1-pred_knn)^2))/nrow(test));rmse5

#Hybrid SVR RT
svpre=predict(dec,dd)
trainsv=cbind(dd$OT,dd$RT,dd$RL,dd$KB,dd$KL,dd$NC,dd$KT,dd$Moisture,dd$Sinkers.Weight.basis.,svpre)
hysv=svm(trainsv,Y,kernal="guassian")
test1=cbind(test$OT,test$RT,test$RL,test$KB,test$KL,test$NC,test$KT,test$Moisture,test$Sinkers.Weight.basis.)
fit8=predict(hysv,trainsv);fit8
SSR8=sum((fit8-me)^2);SSR8
e8=Y-fit8
SSE8=sum(e8^2);SSE8
SST8=SSE8+SSR8;SST8
Rsq8=1-(SSE8/SST8);Rsq8
rmse8=sqrt((sum((Y-fit8)^2))/n);rmse8


#Xgboost
library(xgboost)
library(caret)
m1_xgb <-
  xgboost(
    data = as.matrix(dd),
    label = as.matrix(Y),
    nrounds = 5000,
    objective = "reg:squarederror",
    early_stopping_rounds = 5,
    max_depth = 9,
    eta = .01
  )  

pred_xgb <- predict(m1_xgb, as.matrix(test1))
SSR5=sum((pred_xgb-me)^2);SSR5
e5=test1-pred_xgb
SSE5=sum(e5^2);SSE5
SST5=SSE5+SSR5;SST5
Rsq5=1-(SSE5/SST5);Rsq5
rmse6=sqrt((sum((test1-pred_xgb)^2))/nrow(test));rmse6


method=c("MLR","RT","SVR","Random forest","Bagging","KNN","Hybrid-SVR RT","Hybrid-SVR")
Rsquared=c(rsq,Rsq,Rsq1,Rsq2,Rsq3,Rsq4,Rsq8,Rsq10)
RMSE=c(rmse,rmse1,rmse2,rrmse3,rmse4,rmse5,rmse8,rmse10)
df=data.frame(method,Rsquared,RMSE);df


rr



#knnmodel = knnreg(dd, Yield)
#pred_knn <- predict(knnmodel, (dd))
#SSR4=sum((pred_knn-me)^2);SSR4
#e4=test1-pred_knn
#SSE4=sum(e4^2);SSE4
#SST4=SSE4+SSR4;SST4
#Rsq4=1-(SSE4/SST4);Rsq4





df=rpart.rules(dec)
df
test=data.frame(d[114:133,-1]);test

testt=dd
#r1=subset(testt,RT>19 & OT>56);r1
#testr1=data.frame(r1[,14])
#y1cap= beta1%*%r1[,-14]
library(Rcmdr)
library(rpart)
library(caret)
c=dec[2]
c1=dec[1]
c1[1]
predictions=0
v=as.numeric(unlist(c))
f=data.frame(table(v))$v
u=0
predictions=0
D1={}
ff={}
gg1={}
gg={}
fg={}
f1={}
p1={}
testt=cbind(test,test1)
r1=subset(testt, OT< 55.7 & KT< 12.195&NC>=182&Sinkers..Weight.basis.< 428.5);r1
r2=subset(testt,OT< 55.7 & KT< 12.195&NC>=182&Sinkers..Weight.basis.>= 428.5)
r3=subset(testt,OT< 55.7 & KT< 12.195&NC<182)
r4=subset(testt,OT< 55.7 & KT>= 12.195& KL< 24.25)
r5=subset(testt,OT< 55.7 & KT>= 12.195& KL>= 24.25 & RL< 36.595)
r6=subset(testt,OT< 55.7 & KT>= 12.195& KL>= 24.25 & RL>=36.595)
r7=subset(testt,OT>=55.7&RT< 18.955& Moisture>=6.045 & KB< 16.42)
r8=subset(testt,OT>=55.7&RT< 18.955& Moisture>=6.045 & KB>= 16.42)
r9=subset(testt,OT>=55.7&RT< 18.955& Moisture<6.045)
r10=subset(testt,OT>=55.7&RT>=18.955)

D1=na.omit(dd[which(v==f[10]),])
u=na.omit(Y[which(v==f[10])])
ddd=d[,-1]

#SVM-hybrid
f1=svm(u~.,data=D1)
p1=predict(f1,r10)
g1=cbind(original=r10[,14],pred=p1)
f1
g1
dx=read.csv("caaashew.csv",header=TRUE)
head(dx)
attach(dx)
yy=data.frame(dx[,1]);yy
men=apply(dd,2,mean)
ycap=data.frame(dx[,2])
SSR10=sum((ycap-men)^2);SSR10
e10=(Y-ycap);e10
SSE10=sum(e10^2);SSE10
SST10=SSE10+SSR10;SST10
Rsq10=1-(SSE10/SST10);Rsq10
rmse10=sqrt((sum((Y-ycap)^2))/nrow(test));rmse10


