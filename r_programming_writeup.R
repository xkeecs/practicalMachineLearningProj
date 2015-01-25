
library(ggplot2)
library(caret)

training <- read.csv("pml-training.csv", na.string="#DIV/0!", row.names = 1)
testing <- read.csv("pml-testing.csv", na.string="#DIV/0!", row.names = 1)


# remove columns that have more than 50% of NA or empty value
threshold <- dim(training)[2]*0.5 
columns <- !apply(training, 2, function(x) sum(is.na(x)) > threshold || sum(x == "") > threshold)

# filter the bad training columns
training <- training[, columns]

# columns with zero variation
badColumns <- nearZeroVar(training, saveMetrics = TRUE)

# remove columns with near zero variation
training <- training[, badColumns$nzv == FALSE]

training$classe = factor(training$classe)

inTrain <- createDataPartition(training$classe, p = 0.6)[[1]]
# create cross validation and training data
crossv <- training[ -inTrain, ]
training <- training[ inTrain, ]

inTrain <- createDataPartition(crossv$classe, p = 0.75)[[1]]
crossv_test <-crossv[-inTrain, ]
crossv <- crossv[inTrain, ]

testing <- testing[, columns]
testing$classe <- NA
testing <- testing[,badColumns$nzv==FALSE]

# preprocessing data using PCA
#pca<-preProcess(training[, 6:dim(testing)[2]], method="pca",pcaComp=2)

mod1 <- train(classe ~ ., data=training, method="rf", preProcess="pca")
# cross validation
pred1 <- predict(mod1, crossv)
confusionMatrix(pred1, crossv$classe)

# out of sample error
pred2 <- predict(mod1, crossv_test)
accuracy <- sum(pred2 == crossv_test$classe) / length(pred2)

varRF <- varImp(mod1)
plot(varRF, main = "Importance of Top 40 Variables")
pred3 <- predict(mod1, testing)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(pred3)

featurePlot(x=training[,c("roll_belt","pitch_belt","yaw_belt")], y=training$user_name, plot="pairs")
