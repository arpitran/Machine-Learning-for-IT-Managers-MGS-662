library(Rmosek)
#Reading CSV from local system
train<-read.csv('/Users/ranaa/Box Sync/spring 2018 (arpitran@buffalo.edu)/Machine Learning for IT managers/BlogFeedback/blogData_train.csv',header=FALSE,sep=",",)
summary(train)

#Sample the data 
basic_feature<-train[,51:60]
basic_feature
target_feature<-train["V281"]
target_feature
train1<-data.frame(basic_feature,target_feature)
train1

#dropping columns 55 and 60 as they were leading to rank deficiency
drop=c('V55','V60')
train1=train1[,!(names(train1) %in% drop)]
train1
drops=c('V281')
Ax<-train1[ , !(names(train1) %in% drops)]
Ax
#sampling the data and dividing it into train and test set
set.seed(1) #To reproduce the results
train2<-train1[sample(5000,replace=FALSE),]
train2.rows<-sample(nrow(train2),4000)
train.set<-train2[train2.rows,]
test.set<-train2[-train2.rows,]
#scaling the features
drops=c('V281')
Ax<-train2[ , !(names(train2) %in% drops)]
Ax
scaled.A <- scale(Ax,center=TRUE)
scaled.A
V281<-train2[,'V281']
V281
train.scaled<-data.frame(scaled.A,V281)
train.scaled
train.scaled.rows<-sample(nrow(train2),4000)
train.scaled.set<-train.scaled[train.scaled.rows,]
test.scaled.set<-train.scaled[-train.scaled.rows,]
train.scaled.set
#building your model on the train data( Adhoc Solution)
fitlm<-lm(formula=V281~.,data=train.set)
fitlm.scaled<-lm(formula=V281~.,data=train.scaled.set)
summary(fitlm)
summary(fitlm.scaled)

#predicting the model
pred.train<-predict(fitlm,train.set,se.fit=TRUE)
pred.test<-predict(fitlm,test.set,se.fit=TRUE)
pred.train.scaled<-predict(fitlm.scaled,train.scaled.set,se.fit=TRUE)
pred.test.scaled<-predict(fitlm.scaled,test.scaled.set,se.fit=TRUE)

#RSS value for the linear regression model
rss.train=sum((train.set$V281-pred.train$fit)^2)
rss.test=sum((test.set$V281-pred.test$fit)^2)
rss.test
rss.train
#RSS value for scaled linear regression model
rss.train.scaled=sum((train.scaled.set$V281-pred.train.scaled$fit)^2)
rss.test.scaled=sum((test.scaled.set$V281-pred.test.scaled$fit)^2)
rss.test.scaled
rss.train.scaled




#MSE value for linear regression model
MSE.train<-rss.train/nrow(train.set)
MSE.test<-rss.test/nrow(test.set)
MSE.train
MSE.test

#MSE value for scaled linear regression model
MSE.train.scaled<-rss.train.scaled/nrow(train.set)
MSE.test.scaled<-rss.test.scaled/nrow(test.set)
MSE.train.scaled
MSE.test.scaled

#building a function to find the weights of the co-efficents through RMOSEK
solve.ols<-function(X,y, verb=1){
  p<-dim(X)[2]  # number of parameters of interest
  
  #correspondence between OLS and the Quadratic Program
  xx<-crossprod(X) # X'X=Q variable in the Quadratic program
  c<--crossprod(X,y) # X'y=c variable in the Quadratic program
  xx2<-xx
  xx2[upper.tri(xx)]<-0 #mosek needs Q to be  triangular
  idx <- which(xx2 != 0, arr.ind=TRUE) #index of the nonzero elements of Q
  
  #problem definition in Mosek
  qo1<-list() #empty list that contains the QP problem
  qo1$sense<-"min" #problem sense
  qo1$c<-as.vector(c) #objective coefficients
  qo1$qobj<-list(i = idx[,1],
                 j = idx[,2],
                 v = xx2[idx] ) #the Q matrix is imputed by the row indexes i, the col indexes j and the values v that define the Q matrix
  qo1$A<-Matrix(rep(0,p), nrow=1,byrow=TRUE,sparse=TRUE) #constrain matrix A is a null matrix in this case
  
  
  qo1$bc<-rbind(blc=-Inf, buc= Inf) #constraint bounds
  qo1$bx<-rbind(blx=rep(-Inf,p), bux = rep(Inf,p)) #parameter bounds 
 
  r<-mosek(qo1, opts = list(verbose = verb)) #call mosek solver
  return(r)
}

rf=solve.ols(as.matrix(train.set[,1:ncol(train.set)-1]),as.matrix(train.set$V281))
rf

rf.scaled=solve.ols(as.matrix(train.scaled.set[,1:ncol(train.scaled.set)-1]),as.matrix(train.scaled.set$V281))
rf.scaled

#Taking the wieghts from R mosek and converting them into a 8X1 matrix for matrix multiplication
t<-t(rf$sol$itr$xx)
t.scaled<-t(rf.scaled$sol$itr$xx)
t
t.scaled
t<-t(t)
t
t.scaled<-t(t.scaled)
t.scaled
#finding the error in prediction
yhat.test<-as.matrix(test.set[,1:8])%*%t
yhat.train<-as.matrix(train.set[,1:8])%*%t

yhat.test.scaled<-as.matrix(test.scaled.set[,1:8])%*%t.scaled
yhat.train.scaled<-as.matrix(train.scaled.set[,1:8])%*%t.scaled

YMinusYHat.train = train.set$V281-yhat.train
YMinusYHat.test = test.set$V281-yhat.test

YMinusYHat.train.scaled = train.scaled.set$V281-yhat.train.scaled
YMinusYHat.test.scaled = test.scaled.set$V281-yhat.test.scaled

#RSS and MSE for train and test set
RSS_Mosek.train=sum(YMinusYHat.train^2)
RSS_Mosek.train
RSS_Mosek.test=sum(YMinusYHat.test^2)
RSS_Mosek.test

MSE_Mosek.train=mse(YMinusYHat.train)
MSE_Mosek.train

MSE_Mosek.test=mse(YMinusYHat.test)
MSE_Mosek.test

#RSS and MSE for scaled train and test set
RSS_Mosek.train.scaled=sum(YMinusYHat.train.scaled^2)
RSS_Mosek.train.scaled
RSS_Mosek.test.scaled=sum(YMinusYHat.test.scaled^2)
RSS_Mosek.test.scaled

MSE_Mosek.train.scaled=mse(YMinusYHat.train.scaled)
MSE_Mosek.train.scaled

MSE_Mosek.test.scaled=mse(YMinusYHat.test.scaled)
MSE_Mosek.test.scaled




