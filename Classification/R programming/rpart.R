data("iris")  
trainIndex <- sample(1:nrow(iris), 0.8 * nrow(iris))
  train <- iris[trainIndex, ]
  test <- iris[-trainIndex, ]
  
  dt<-rpart(Species~.,data = train,method ='class', parms = list(split = "information"),control = rpart.control(minsplit = 35, cp = 0.04))
  
  plot(dt)
  print(dt)
  dtp<-predict(dt,test,type="class")
  table(test[[5]],predicted = dtp)
  dtt<-table(test[[5]],predicted = dtp)
  sum(diag(dtt))/sum(dtt)
 
  