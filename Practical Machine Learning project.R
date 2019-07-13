library(caret)
library(ggplot2)
library(randomForest)

test <- read.csv('pml-testing.csv')
train <- read.csv('pml-training.csv')

head(train) #Look at some of the data
colSums(is.na(train)) #identify columns with na

subsettrain<-train[,(colSums(is.na(train)) == 0)]
subsettest<-test[,(colSums(is.na(train)) == 0)]

#remove other variables that has too many missing values that it is also hard to impute
subseting <- !grepl('^X|user|window|kurtosis|skewness|timestamp|max_yaw|min_yaw|amplitude',names(subsettrain))
trainclean <- subsettrain[,subseting]
testclean <- subsettest[,subseting]

heatmap(abs(cor(trainclean[1:52])))

training <- trainclean[,-findCorrelation(cor(trainclean[, 1:52]), cutoff = .7)]
testing <- testclean[,-findCorrelation(cor(trainclean[, 1:52]), cutoff = .7)]

set.seed(123123)
inTrain <- createDataPartition(training$classe,p=0.7,list=FALSE)
training2 <- training[inTrain,]
validation <- training[-inTrain,]


prComp <- prcomp(training2[,1:30])
qplot(prComp$x[,1],prComp$x[,2],colour=training2$classe) 
summary(prComp)

set.seed(123123)
train_control <- trainControl(method="cv", number=10)
pca <- preProcess(training2[,1:30],method='pca',pcaComp=5)
trainpc <- predict(pca,training2[,1:30])
trainpc$classe <- training2$classe
modpca <- train(classe~.,data=trainpc,method='rf', trControl=train_control)

testpc <- predict(pca,validation[1:30])
testpc$classe <- validation$classe
confusionMatrix(validation$classe,predict(modpca,testpc))

#mod pca has 86 percent of accuracy

