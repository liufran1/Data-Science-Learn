library(e1071)

#kernalType = "radial"
#kernalType = "linear"
kernalType = "sigmoid"
#hardcoded function to wrap iterations in
svmSimulation<-function(kernalType){
  #generate 50 observations of x from R10. 500 total observations drawn from a normal distribution
  trainx<-matrix(rnorm(500),100,10)
  testx<-matrix(rnorm(500),100,10)
  
  #create a y vector of 50 observations evenly divided between 0 and 1
  
  trainy<-rep(c(0,1),c(50,50))
  testy<-rep(c(0,1),c(50,50))
  
  #for the indices where y=1, add the vector mu
  trainx[trainy==1,]<-trainx[trainy==1,]+c(1,1,1,1,1,0,0,0,0,0)
  testx[testy==1,]<-testx[testy==1,]+c(1,1,1,1,1,0,0,0,0,0)
  
  #put all the data together into a dataframe
  trainDat<-data.frame(trainx,trainy=as.factor(trainy))
  testDat<-data.frame(testx,testy=as.factor(testy))
  colnames(trainDat)[11] <- "y"
  colnames(testDat)[11] <- "y"
  #shuffle data
  #dat<-dat[sample(nrow(dat)),]
  #plot first two dimensions
  #plot(dat$X1,dat$X2, col=dat$y,pch=19)
  
  #train/test 70/30 split
  #trainDat<-dat[(1:70),]
  #testDat<-dat[(71:100),]
  
  #fit SVM
  svmfit=svm(y~.,data=trainDat,kernel=kernalType,cost=10,scale=FALSE)
  print(svmfit)
  
  #predict on test
  svmpredict<-predict(svmfit,testDat[,1:10])
  testErrorFrame<-data.frame(svmpredict,testDat$y)
  testErrorFrame$diff = testErrorFrame$svmpredict == testErrorFrame$testDat.y
  testError = 1 - sum(testErrorFrame$diff)/100
  #print(table(svmpredict,testDat$y))
  return(testError)
}

svmSimulation(kernalType)
testErrorsMC <- c()
for(i in 1:100){
  testErrorsMC[i] <- svmSimulation(kernalType)
}
mean(testErrorsMC)
