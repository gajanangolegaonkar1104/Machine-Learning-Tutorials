library(neuralnet)
data("iris")
maxs=apply(iris,MARGIN = 2,max)
mins=apply(iris,MARGIN = 2,min)
scaled = as.data.frame(scale(b,center = mins,scale = maxs - mins))
trainIndex <- sample(1:nrow(iris), 0.8 * nrow(iris))
iristrain <- iris[trainIndex, ]
iristest <- iris[-trainIndex, ]

nnet_iristrain <-iristrain
nnet_iristrain <- cbind(nnet_iristrain, 
                        iristrain$Species == 'setosa')
nnet_iristrain <- cbind(nnet_iristrain,
                        iristrain$Species == 'versicolor')
nnet_iristrain <- cbind(nnet_iristrain, 
                        iristrain$Species == 'virginica')
names(nnet_iristrain)[6] <- 'setosa'
names(nnet_iristrain)[7] <- 'versicolor'
names(nnet_iristrain)[8] <- 'virginica'
nn <- neuralnet(setosa+versicolor+virginica ~ 
                  Sepal.Length+Sepal.Width
                +Petal.Length
                +Petal.Width,
                data=nnet_iristrain, 
                hidden=c(2,3), rep = 1)
plot(nn)
mypredict <- compute(nn, iristest[-5])$net.result

maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, c(1), maxidx)
prediction <- c('setosa', 'versicolor', 'virginica')[idx]
dtt<-table(prediction, iristest$Species)
sum(diag(dtt))/sum(dtt)