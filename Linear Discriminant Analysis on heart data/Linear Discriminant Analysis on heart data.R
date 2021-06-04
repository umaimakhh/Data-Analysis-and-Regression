#author: "Umaima Khurshid Ahmad"
#date: "8/22/2020

#Libraries

library(MASS)
library(caret)
##############################################################################################

#Set Working Directory
setwd('E:/DPU/Summer I/Advance Data Analysis/HW4')
#Read in responsess

responses <- read.csv(file="heart.csv", header=TRUE, sep=",") 

#Check Sample Size and Number of Variables
dim(responses)
head(responses)

names(responses)
summary(responses)

#cross-validation

heardLDA <- lda(heartdisease~., data =responses,CV=T)
heardLDA
table(heardLDA$class,responses$heartdisease)
accuracy = (373 +476)/(373 +476+126+50)
accuracy

p = predict(heardLDA,newdata=responses[,1:13])$class
p

table(p,responses$heartdisease)
accuracy = (376+485)/(376+485+41+123)
accuracy

require(caTools)
library(caTools)
set.seed(123)

sample = sample.split(responses,SplitRatio=0.70)
train=subset(responses,sample==TRUE)
test=subset(responses,sample==FALSE)
heardLDAv <- lda(heartdisease~., data =responses)
heardLDAv
p = predict(heardLDAv,newdata=train[,1:13])$class
p
table(p,train$heartdisease)
accuracy = (249  +306)/(249  +306+76 +28)
accuracy



train$heartdisease <- as.factor(train$heartdisease[[1]])
modelFit <- train(heartdisease ~ ., method='lda',preProcess=c('scale','center'),data =train)
confusionMatrix(train$heartdisease,predict(modelFit,train))
