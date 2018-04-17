library(e1071)

#hardcoded function to wrap iterations in
svmSimulation<-function(){
  #generate 50 observations of x from R10. 500 total observations drawn from a normal distribution
  x<-matrix(rnorm(500),100,10)
  
  #create a y vector of 50 observations evenly divided between 0 and 1
  y<-rep(c(0,1),c(50,50))
  
  #for the indices where y=1, add the vector mu
  x[y==1,]<-x[y==1,]+c(1,1,1,1,1,0,0,0,0,0)
  
  #put all the data together into a dataframe
  dat<-data.frame(x,y=as.factor(y))
  
  #shuffle data
  dat<-dat[sample(nrow(dat)),]
  #plot first two dimensions
  #plot(dat$X1,dat$X2, col=dat$y,pch=19)
  
  #train/test 70/30 split
  trainDat<-dat[(1:70),]
  testDat<-dat[(71:100),]
  
  #fit SVM
  svmfit=svm(y~.,data=trainDat,kernel="radial",cost=10,scale=FALSE)
  print(svmfit)
  
  #predict on test
  svmpredict<-predict(svmfit,testDat[,(1:10)])
  testErrorFrame<-data.frame(svmpredict,testDat$y)
  testErrorFrame$diff = testErrorFrame$svmpredict == testErrorFrame$testDat.y
  testError = 1 - sum(testErrorFrame$diff)/30
  #print(table(svmpredict,testDat$y))
  return(testError)
}

#svmSimulation()
testErrorsMC <- c()
for(i in 1:100){
  testErrorsMC[i] <- svmSimulation()
}
mean(testErrorsMC)
