library(e1071)
data("iris")
# Can handle both categorical and numeric input, 
# but output must be categorical
trainIndex <- sample(1:nrow(iris), 0.8* nrow(iris))
iristrain <- iris[trainIndex, ]
iristest <- iris[-trainIndex, ]

model <- naiveBayes(Species~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iristrain, threshold = 0.000001)
prediction <- predict(model, iristest[,-5])
dtt<-table(prediction, iristest[,5])
sum(diag(dtt))/sum(dtt)