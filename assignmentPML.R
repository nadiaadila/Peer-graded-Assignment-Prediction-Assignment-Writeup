setwd("C:/Users/saya/Desktop/DS/real kuiz/Practical Machine Learning/Week 4/Assignment")

install.packages("rpart.plot")
install.packages("rattle")

library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)


pmlTraining <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))

pmlTesting <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

#Remove variables that we believe have too many NA values.
#remove also the 7 unrelevant variables as it not related to time series and numeric
remoNA <- names(pmlTesting[ , colSums(is.na(pmlTesting)) == 0])[8:59]

# Only use features used in testing cases.
dataTraining <- pmlTraining[,c(remoNA,"classe")]
dataTesting <- pmlTesting[,c(remoNA,"problem_id")]

dim(dataTraining); dim(dataTesting);

set.seed(12345)
dataTraining2 <- dataTraining
#convert the classe in datataraining column to numeric so that there will be no problem in create data partition
dataTraining2$classe <- as.numeric(dataTraining2$classe)

usTrain <- createDataPartition(dataTraining2$classe, p=0.6, list=FALSE)
training <- dataTraining2[usTrain,]
testing <- dataTraining2[-usTrain,]

dim(training); dim(testing);

#Using Decision Tree
modDT <- rpart(classe ~ ., data = training, method="class")
fancyRpartPlot(modDT)

#predict using decision tree
set.seed(12345)

predictionDT <- predict(modDT, testing, type = "class")
confusionMatrix(predictionDT, testing$classe)

#build random forest model
set.seed(12345)

#need to convert the training classe to factor first so that randomForest can be run and to avoid fewer unique value
training$classe <- as.factor(training$classe)
modRF <- randomForest(classe ~ ., data = training, ntree = 1000, importance=TRUE)

varImpPlot(modRF, )

#predict RFM
predictionRFM <- predict(modRF, testing, type = "class")
confusionMatrix(predictionRFM, testing$classe)

#Predicting on the Testing Data
#Decision Tree Prediction
predictionDT <- predict(modDT, pmlTesting, type = "class")
predictionDT

predictionRF <- predict(modRF, pmlTesting, type = "class")
predictionRF

tree.pred=predict(modRF,testing,type="class")
predMatrix = with(testing,table(tree.pred,classe))
sum(diag(predMatrix))/sum(as.vector(predMatrix)) # error rate

