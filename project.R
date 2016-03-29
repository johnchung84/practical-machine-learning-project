train.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(train.url,destfile="pml-training.csv",method="curl")
download.file(test.url,destfile="pml-testing.csv",method="curl")

training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

dim(training)
dim(testing)
names(training)

summary(training[,18:24])


summary(training[1:24,])
training[25,]

head(training$classe)

head(training[,1:5])

summary(training)

library(caret)
library(ggplot2)

set.seed(1111)

trainControl(method="cv")

## PreProcess

preProcess(method="BoxCox")

train(,preProcess=c("BoxCox"))

## Imputing data

preProcess(method="knnImpute")


## Removing zero covariates
nearZeroVar

head(nearZeroVar(training,names=TRUE),15)

library(dplyr)

training1 <- select(training,-c(get(head(nearZeroVar(training,names=TRUE),2))))



training1.roll <- gather(training1,key="part",value="roll",roll_belt,roll_forearm,roll_dumbbell,roll_arm)
training1.pitch <- gather(training1,key="part",value="pitch",pitch_belt,pitch_forearm,pitch_dumbbell,pitch_arm)
training1.yaw <- gather(training1,key="part",value="yaw",roll_belt,roll_forearm,roll_dumbbell,roll_arm)

ggplot(training1.pitch,aes(x=pitch,fill=part))+geom_density()

boxplot(pitch~part,data=training1.pitch)

nzv.names<-rownames(nsv[nsv$nzv==TRUE,])

na.col.train<-apply(training,2,function(x) sum(is.na(x))/length(x)*100)
na.col.test<-apply(testing,2,function(x) sum(is.na(x))/length(x)*100)


#####

# Decision tree

dtree<-train(classe~.,data=training.clean,method="rpart",trControl=trainControl(method="cv"))
dtree

# random forecast

rf<-train(classe~.,data=training.clean,method="rf",trControl=trainControl(method="cv"))
rf


# boosting

gbm<-train(classe~.,data=training.clean,method="gbm",trControl=trainControl(method="cv"),verbose=FALSE)
gbm

# lda

lda<-train(classe~.,data=training.clean,method="lda",trControl=trainControl(method="cv"))
lda


# nb
nb <-train(classe~.,data=training.clean,method="nb",trControl=trainControl(method="cv"))
nb





